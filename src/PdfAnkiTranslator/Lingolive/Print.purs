module PdfAnkiTranslator.Lingolive.Print where

import PdfAnkiTranslator.Lingolive.Types
import Protolude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.String as String

type AnkiFields =
  { question      :: String -- orig lang
  , transcription :: String
  , answer        :: String -- my lang (using google translate)
  , myContext     :: String
  , body          :: String -- examples, etc.
  }

-- from "Body"
findTranscriptionFromBody :: NonEmptyArray ArticleModel -> String -- maybe empty
findTranscriptionFromBody = undefined

printBodyFromAbbyy :: NonEmptyArray ArticleModel -> String
printBodyFromAbbyy = undefined

printArticleModel ::
  { fromAbbyy           :: NonEmptyArray ArticleModel
  , fromGoogleTranslate :: NonEmptyArray String
  , sentence            :: String
  , annotation_text     :: String
  , annotation_content  :: Maybe String
  } ->
  AnkiFields
printArticleModel input =
  { question:      input.annotation_text -- TODO: print TitleMarkup from abbyy
  , transcription: findTranscriptionFromBody input.fromAbbyy
  , answer:        String.joinWith ", " input.fromGoogleTranslate
  , myContext:     sentence <> maybe "" (\x -> " [" <> x <> "]") annotation_content
  , body:          printBodyFromAbbyy input.fromAbbyy
  }
