module PdfAnkiTranslator.Main where

import Protolude
import Foreign

newtype InputElement
  = InputElement
  { "annotations" ::
    Array
      { "contents" :: Maybe String
      , "text" :: String
      }
  , "sentence" :: String
  }

-- https://github.com/sindresorhus/get-stdin/blob/a3260098ca7ac98167badeec3753d42ab12b1b0d/index.js#L4
readStdin :: Effect (Maybe String)
readStdin = undefined

main :: Effect Unit
main = undefined
