{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Conduit (MonadIO (liftIO), MonadResource, mapM_C, runResourceT)
import New

import Data.Attoparsec
import Data.Attoparsec.Char8 (char, isDigit_w8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit
import Data.Conduit.Attoparsec (conduitParserEither)
import System.IO

data Expr = Add Int Int
  deriving (Show)

evalExpr :: Expr -> Int
evalExpr (Add a b) = a + b

parseAdd :: Parser Expr
parseAdd = do
  let convertW8ToInt w = fromIntegral (w - 48)
  d1 <- convertW8ToInt <$> satisfy isDigit_w8
  _ <- char '+'
  d2 <- convertW8ToInt <$> satisfy isDigit_w8
  pure $ Add d1 d2

readLinesForever :: (Monad m, MonadIO m, MonadResource m) => ConduitT i ByteString m ()
readLinesForever =
  bracketP
    (openFile "./test.fifo" ReadMode)
    (\handle -> hClose handle)
    loop
 where
  loop handle = do
    eof <- liftIO $ hIsEOF handle
    if eof
      then loop handle
      else do
        l <- liftIO $ hGetLine handle
        yield $ BS8.pack l
        loop handle

main :: IO ()
main =
  runResourceT $
    runConduit $
      readLinesForever
        .| conduitParserEither parseAdd
        .| mapM_C (liftIO . print . fmap (evalExpr . snd))
