module Main where

import Control.Monad (forever, when)
import Morse
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)


convert :: (String -> IO ()) -> IO ()
convert f = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  ln <- getLine
  f ln

toMorse :: String -> IO ()
toMorse line = do
  let morse = stringToMorse line
  case morse of
    (Just str) ->
      putStrLn $ unwords str
    Nothing -> do
      putStrLn $ "ERROR: " ++ line
      exitFailure

fromMorse :: String -> IO ()
fromMorse line = do
  let decoded :: Maybe String
      decoded = traverse morseToChar (words line)
  case decoded of
    (Just str) ->
      putStrLn str
    Nothing -> do
      putStrLn $ "ERROR: " ++ line
      exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convert fromMorse
        "to"   -> convert toMorse
        _      -> argError
    _ -> argError
  where argError = do
          putStrLn "Please specify the first argument \
                   \as being 'from' or 'to' morse.\
                   \ such as: morse to"
          exitFailure
