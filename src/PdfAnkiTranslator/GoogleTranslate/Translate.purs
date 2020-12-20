module PdfAnkiTranslator.GoogleTranslate.Translate where

import Protolude
import PdfAnkiTranslator.Languages

type Config =
  { from :: Language
  , to :: Language
  , word :: String
  , accessKey :: String
  }

type Response =
  { "data" ::
    { translations :: Array
      { translatedText :: String
      }
    }
  }

request :: Config -> Aff Response
request = undefined
