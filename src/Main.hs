{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson             hiding (Success)
import qualified Data.ByteString        as BS
import           Data.Either
import           Data.Maybe             (mapMaybe)
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GHC.Generics
import           Nix                    hiding (try)
import           Text.Megaparsec.Pos    (pos1)
import           Text.Regex.Applicative

data Path = Path
  { file :: FilePath
  , line :: Int
  } deriving (Generic, Show)

data Entry = Entry
  { name :: String
  , path :: Path
  , urls :: [Text]
  } deriving (Generic, Show)

instance FromJSON Path
instance FromJSON Entry


findStrings :: NExprLoc -> [Str]
findStrings (AnnE s (NStr str)) = [(str, s)]
findStrings (AnnE _ (NList vals)) = concatMap findStrings vals
findStrings (AnnE _ (NSet bindings)) = concatMap (\case
                                                     NamedVar _ val _ -> findStrings val
                                                     Inherit {} -> []
                                                    ) bindings
findStrings (AnnE _ (NRecSet bindings)) = concatMap (\case
                                                     NamedVar _ val _ -> findStrings val
                                                     Inherit {} -> []
                                                    ) bindings
findStrings (AnnE _ (NUnary _ val)) = findStrings val
findStrings (AnnE _ (NBinary _ left right)) = findStrings left ++ findStrings right
findStrings (AnnE _ (NSelect left _ Nothing)) = findStrings left
findStrings (AnnE _ (NSelect left _ (Just right))) = findStrings left ++ findStrings right
findStrings (AnnE _ (NAbs _ val)) = findStrings val
findStrings (AnnE _ (NLet bindings val)) = findStrings val ++ concatMap (\case
                                                     NamedVar _ val _ -> findStrings val
                                                     Inherit {} -> []
                                                    ) bindings
findStrings (AnnE _ (NIf _ left right)) = findStrings left ++ findStrings right
findStrings (AnnE _ (NWith left right)) = findStrings left ++ findStrings right
findStrings (AnnE _ (NAssert _ val)) = findStrings val
findStrings _ = []

-- Nothing: Antiquoted
-- Just: Fixed text
toThing :: Str -> Maybe [Maybe (SrcSpan, Text)]
toThing (DoubleQuoted parts, span) = convertParts (span { spanBegin = (spanBegin span) { sourceColumn = pos1 Data.Semigroup.<> sourceColumn (spanBegin span) } }) parts
  where
    convertParts :: SrcSpan -> [Antiquoted Text NExprLoc] -> Maybe [Maybe (SrcSpan, Text)]
    convertParts span [] = Just []
    convertParts span (Plain text:rest) = do
      let endpoint = if Text.length text == 1 then sourceColumn (spanBegin span) else sourceColumn (spanBegin span) Data.Semigroup.<> mkPos (Text.length text - 1)
      let newspan = span { spanBegin = (spanBegin span) { sourceColumn = endpoint Data.Semigroup.<> pos1 } }
      r <- convertParts newspan rest
      return $ Just (SrcSpan
                     { spanBegin = spanBegin span
                     , spanEnd = (spanBegin span)
                       { sourceColumn = endpoint }
                     }, text) : r
    convertParts span (EscapedNewline:rest) = Nothing
    convertParts span (Antiquoted (AnnE s val):rest) = do
      let newspan = span { spanBegin = (spanBegin span) { sourceColumn = pos1 Data.Semigroup.<> sourceColumn (spanEnd s) } }
      r <- convertParts newspan rest
      return $ Nothing : r
toThing (Indented _ _, _) = Nothing


parseThing :: Maybe (a, Text) -> RE Char (Either (Text, a) Text)
parseThing Nothing = Right . Text.pack <$> many anySym
parseThing (Just (span, text)) = const (Left (text, span)) <$> string (Text.unpack text)

parseMany :: [Maybe (a, Text)] -> RE Char [Either (Text, a) Text]
parseMany = traverse parseThing

doMatch :: Text -> [Maybe (a, Text)] -> Maybe [Either (Text, a) Text]
doMatch url parts = match (traverse parseThing parts) (Text.unpack url)

match' :: Text -> Str -> Maybe MatchedUrl
match' url str = do
  res <- toThing str
  x <- match (parseMany res) (Text.unpack url)
  return $ MatchedUrl x

toPatterns :: [Antiquoted Text NExprLoc] -> Maybe [Either Char NExprLoc]
toPatterns [] = Just []
toPatterns (Plain text:rest) = do
  r <- toPatterns rest
  let this = map Left $ Text.unpack text
  return $ this ++ r
toPatterns (EscapedNewline:rest) = Nothing
toPatterns (Antiquoted val:rest) = do
  r <- toPatterns rest
  return $ Right val : r

type Str = (NString NExprLoc, SrcSpan)
-- Left: Plain text
-- Right: Matched aniquotation
newtype MatchedUrl = MatchedUrl [Either (Text, SrcSpan) Text] deriving Show

-- To replace: query url again, match again, Text parts should match, replace SrcSpan parts

findUrl :: Text -> NExprLoc -> Either String MatchedUrl
findUrl url expr = case mapMaybe (match' url) $ findStrings expr of
  []  -> Left "Url not found"
  [m] -> Right m
  _   -> Left "Ambiguous"

updateUrl :: Text -> IO Text
updateUrl url = return $ "h" <> url <> "/"

handleUrl :: NExprLoc -> Text -> IO ()
handleUrl expr url = do
  let stringPatterns = mapMaybe toThing $ findStrings expr
  let theStringPattern = mapMaybe (doMatch url) stringPatterns
  case findUrl url expr of
    Left error -> print error
    Right (MatchedUrl m) -> do
      let inverted = map (\x -> case x of
                             Left (text, span) -> Nothing
                             Right text        -> Just ((), text)) m
      print "Matched"
      newurl <- updateUrl url
      print $ "Finding url " ++ show newurl
      -- TODO: Need to switch around: Previously fixed strings should be .*, previously antiquoted parts should be fixed (be determined the values with the first match)
      -- This way we can vary only the previously fixed parts. Match the url with the new pattern

      -- To replace: Replace the source spans from the back to the front -> Doesn't change the indices
      case findUrl newurl expr of
        Left error -> do
          print "Didn't match the second time"
        Right (MatchedUrl m') -> do
          print $ "COMPARING: " <> show (rights m) <> " AND " <> show (rights m')
          if rights m == rights m'
            then do
              print "IS MATCHING"
              print $ "CAN REPLACE " <> show (map fst (lefts m)) <> " WITH " <> show (map fst (lefts m'))
            else do
              print "NOT MATCHING"

          return ()

handleEntry :: Entry -> IO ()
handleEntry Entry { urls, path } = do
  res <- parseNixFileLoc (file path)
  case res of
    Failure doc -> print doc
    Success expr -> do
      print $ "Parsed nix file " ++ file path
      traverse (handleUrl expr) urls
      return ()

main :: IO ()
main = do
  contents <- BS.readFile "urls.json"
  case eitherDecodeStrict' contents :: Either String [Entry] of
    Left error    -> print error
    Right entries -> do
      putStrLn $ "Handling " ++ show (length entries) ++ " entries"
      mapM_ handleEntry entries
