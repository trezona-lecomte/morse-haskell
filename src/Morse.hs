module Morse ( Morse
             , charToMorse
             , morseToChar
             , stringToMorse
             , letterToMorse
             , morseToLetter
             ) where


import qualified Data.Map as Map


type Morse = String

morseToLetter :: Map.Map Morse Char
morseToLetter =
  Map.foldWithKey (flip Map.insert) Map.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse char =
  Map.lookup char letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m =
  Map.lookup m morseToLetter

stringToMorse :: String -> Maybe [Morse]
stringToMorse str =
  sequence $ fmap charToMorse str

letterToMorse :: (Map.Map Char Morse)
letterToMorse =
  Map.fromList [ ('a', ".-")
               , ('b', "-...")
               , ('c', "-.-.")
               , ('d', "-..")
               , ('e', ".")
               , ('f', "..-.")
               , ('g', "--.")
               , ('h', "....")
               , ('i', "..")
               , ('j', ".---")
               , ('k', "-.-")
               , ('l', ".-..")
               , ('m', "--")
               , ('n', "-.")
               , ('o', "---")
               , ('p', ".--.")
               , ('q', "--.-")
               , ('r', ".-.")
               , ('s', "...")
               , ('t', "-")
               , ('u', "..-")
               , ('v', "...-")
               , ('w', ".--")
               , ('x', "-..-")
               , ('y', "-.--")
               , ('z', "--..")
               , ('1', ".----")
               , ('2', "..---")
               , ('3', "...--")
               , ('4', "....-")
               , ('5', ".....")
               , ('6', "-....")
               , ('7', "--...")
               , ('8', "---..")
               , ('9', "----.")
               , ('0', "-----")
               ]
