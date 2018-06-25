{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception.Base
import           Control.Monad
import           Data.Aeson                hiding (Success)
import qualified Data.ByteString.Char8     as BS
import           Data.Either
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Maybe                (catMaybes, fromJust, mapMaybe)
import           Data.Semigroup
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as TIO
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           Network.URI               hiding (path)
import           Nix
import           System.IO
import           Text.Megaparsec.Pos       (Pos, pos1)
import           Text.Regex.Applicative

data Entry = Entry
  { name :: String
  , file :: FilePath
  , urls :: [String]
  } deriving (Generic, Show)

instance FromJSON Entry

type Str = (NString NExprLoc, SrcSpan)

-- Finds all strings that could be accessed in an expression
findStrings :: NExprLoc -> [Str]
findStrings (AnnE s (NStr str)) = [(str, s)]
findStrings (AnnE _ val) = let
    inBinding (NamedVar _ val _) = findStrings val
    inBinding Inherit {}         = []
  in case val of
    (NList vals) -> concatMap findStrings vals
    (NSet bindings) -> concatMap inBinding bindings
    (NRecSet bindings) -> concatMap inBinding bindings
    (NUnary _ val) -> findStrings val
    (NBinary _ left right) -> findStrings left ++ findStrings right
    (NSelect left _ Nothing) -> findStrings left
    (NSelect left _ (Just right)) -> findStrings left ++ findStrings right
    (NAbs _ val) -> findStrings val
    (NLet bindings val) -> findStrings val ++ concatMap inBinding bindings
    (NIf _ left right) -> findStrings left ++ findStrings right
    (NWith left right) -> findStrings left ++ findStrings right
    (NAssert _ val) -> findStrings val
    _ -> []

newtype Pattern = Pattern [Maybe Text]

extractPattern :: Str -> Maybe ([(Text, SrcSpan)], Pattern)
extractPattern (Indented _ _, _) = Nothing
extractPattern (DoubleQuoted parts, span) = convertParts (span { spanBegin = (spanBegin span) { sourceColumn = pos1 Data.Semigroup.<> sourceColumn (spanBegin span) } }) parts
  where
    convertParts :: SrcSpan -> [Antiquoted Text NExprLoc] -> Maybe ([(Text, SrcSpan)], Pattern)
    convertParts span [] = Just ([], Pattern [])
    convertParts span (Plain text:rest) = do
      let endpoint = if Text.length text == 1 then sourceColumn (spanBegin span) else sourceColumn (spanBegin span) Data.Semigroup.<> mkPos (Text.length text - 1)
      let newspan = span { spanBegin = (spanBegin span) { sourceColumn = endpoint Data.Semigroup.<> pos1 } }
      (spans, Pattern patts) <- convertParts newspan rest
      return ((text, SrcSpan
               { spanBegin = spanBegin span
               , spanEnd = (spanBegin span)
                 { sourceColumn = endpoint }
               }) : spans
             , Pattern (Just text : patts))
    convertParts span (EscapedNewline:rest) = Nothing
    convertParts span (Antiquoted (AnnE s val):rest) = do
      let len = unPos (sourceColumn (spanEnd s)) - unPos (sourceColumn (spanBegin s)) + 3
      let newspan = span { spanBegin = (spanBegin span) { sourceColumn = mkPos len Data.Semigroup.<> (sourceColumn (spanBegin span)) } }
      (spans, Pattern patts) <- convertParts newspan rest
      -- TODO Merge antiquote patterns in a row
      return (spans, Pattern (Nothing : patts))

maybeMatch :: String -> Pattern -> Maybe [Maybe Text]
maybeMatch text (Pattern patt) = match (traverse matchPatt patt) text
  where
    matchPatt :: Maybe Text -> RE Char (Maybe Text)
    matchPatt Nothing     = Just . Text.pack <$> many anySym
    matchPatt (Just text) = const Nothing <$> string (Text.unpack text)

updateUrl :: Manager -> String -> IO String
updateUrl mng url = case parseURI url of
  Nothing -> do
    --putStrLn $ "Couldn't parse uri " ++ url
    return url
  Just uri -> case requestFromURI uri of
    Nothing -> do
      --putStrLn $ "Can't handle " ++ url
      return url
    Just request -> do
      if secure request then do
          --putStrLn "Is secure already"
          return url
        else do
          let secureRequest = fromJust $ requestFromURI (uri { uriScheme = "https:" })
          putStrLn $ "Replacing with secure url (" ++ url ++ ")"
          handle (\(e :: SomeException) -> do
                     putStrLn $ "Got exception: " ++ show e
                     return url) $ withResponseHistory secureRequest mng $ \hr -> do
            let req = hrFinalRequest hr
            let status = statusCode . responseStatus . hrFinalResponse $ hr
            if status < 200 || status >= 300
              then do
                putStrLn $ "Status code is " ++ show status
                return url
              else do
                let final = "https://" ++ BS.unpack (host req) ++ BS.unpack (path req)
                putStrLn $ "Got final url: " ++ final
                return final

matchString :: String -> Str -> Maybe (SrcSpan, [(Text, SrcSpan)], Pattern)
matchString url str@(_, span) = do
  (src, patt) <- extractPattern str
  newpatt <- Pattern <$> maybeMatch url patt
  return (span, src, newpatt)

replaceLine :: Text -> [(Int, Int, Text)] -> Text
replaceLine = foldr (\(start, end, newtext) line -> Text.take start line <> newtext <> Text.drop end line)
-- file: Which file to replace in
-- line: Which line in the file to replace
-- replacements: List of replacement pairs (start column, end column, new text)
replaceInFile :: FilePath -> (Int, Int) -> [(Int, Int, Text)] -> IO ()
replaceInFile file (line, column) replacements = do
  lines <- Text.lines <$> TIO.readFile file
  let oldline = lines !! line
  print column
  let hasQuotes = Text.index oldline column == '"'
  --print $ "Is quote: " ++ show hasQuotes
  let newline = replaceLine oldline (if hasQuotes
                                     then map (\(from, to, newtext) -> (from - 1, to, newtext)) replacements
                                     else map (\(from, to, newtext) -> (from - 2, to - 1, newtext)) replacements)
  let newlines = take line lines ++ [newline] ++ drop (line + 1) lines
  --putStrLn $ "Replacing " ++ show oldline ++ " with " ++ show newline
  TIO.writeFile file (Text.unlines newlines)

handleUrl :: Manager -> FilePath -> NExprLoc -> String -> IO ()
handleUrl mng path expr url = case mapMaybe (matchString url) (findStrings expr) of
  [] -> return ()--putStrLn $ "Url " ++ show url ++ " not found"
  [(sp, src, newpatt)] -> do
    -- src is the info on the original sections in our file
    -- newpatt is the pattern to match the new url against
    --putStrLn "Found"
    newurl <- updateUrl mng url
    case maybeMatch newurl newpatt of
      Nothing -> return ()--putStrLn "New url didn't match"
      Just res -> if newparts == map fst src then return () {-putStrLn "No changes"-} else do
        replaceInFile path ((\x -> x - 1) . unPos . sourceLine . spanBegin . snd . head $ src, (\x -> x - 1) . unPos . sourceColumn . spanBegin $ sp)
          $ zipWith (\(_, span) newtext -> (unPos $ sourceColumn (spanBegin span), unPos $ sourceColumn (spanEnd span), newtext)) src newparts
        putStrLn $ "Changed from " ++ show url ++ " to " ++ show newurl
        where newparts = catMaybes res
  _ -> return () --putStrLn $ "Ambiguous url " ++ show url

handleEntry :: Manager -> Entry -> IO ()
handleEntry mng Entry { urls, file } = do
  res <- parseNixFileLoc file
  case res of
    Failure doc -> print doc
    Success expr -> do
      --print $ "Parsed nix file " ++ file path
      traverse_ (handleUrl mng file expr) urls
      return ()

main :: IO ()
main = do
  contents <- BS.readFile "urls.json"
  mng <- newManager (tlsManagerSettings {
                        managerResponseTimeout = responseTimeoutMicro (10 * 1000 * 1000)
                        })
  case eitherDecodeStrict' contents :: Either String [Entry] of
    Left error    -> print error
    Right entries -> do
      putStrLn $ "Handling " ++ show (length entries) ++ " entries"
      mapM_ (handleEntry mng) entries
