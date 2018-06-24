{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Nix

import           Control.Monad.Catch

import           Control.Monad
import           Control.Monad.Trans
import           Data.Fix
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as TextIO
import           Data.Time
import           Nix.Utils

getDrvs :: (MonadIO m, MonadThunk (NValue m) (NThunk m) m) => AttrSet (NThunk m) -> m [(Text, AttrSet (NThunk m))]
getDrvs set = do
  attrs <- mapM (\(n, t) -> force t $ \v -> case _baseValue v of
                    NVSetF x _ -> return (Just (n, x))
                    _          -> return Nothing
                ) (H.toList set)
  filterM (\(n, v) -> isDerivation v) (catMaybes attrs)

nixpkgs :: (MonadIO m, MonadNix e m, Has e Options) => m (NValue m)
nixpkgs = do
  --let expr = Fix (NBinary NApp (Fix (NEnvPath "nixpkgs")) (Fix (NSet [])))
  let path = "/home/infinisil/src/nixpkgs/default.nix"
  r <- parseNixFile path
  case r of
    Failure doc  -> fail (show doc)
    Success expr -> do
      -- import <nixpkgs> {}
      let x = NBinary NApp expr (Fix (NSet []))
      nixEvalExpr (Just path) (Fix x)

program :: (MonadIO m, MonadNix e m, Has e Options) => m ()
program = do
  n <- nixpkgs
  case _baseValue n of
    NVSetF set _ -> do
      drvs <- catch (getDrvs set) $ \x -> case x of
        NixException frames ->
          errorWithoutStackTrace . show
            =<< renderFrames @(NThunk (Lazy IO)) frames
      liftIO . print $ "The first 2 derivations names are:"
      liftIO . print . map fst . take 2 $ drvs
      return ()
    _ -> fail "Nixpkgs is no attrset???"

main :: IO ()
main = do
  time <- liftIO getCurrentTime
  let opts = defaultOptions time
  runLazyM opts program
