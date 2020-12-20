module PdfAnkiTranslator.Lingolive.Print where

import Protolude
import PdfAnkiTranslator.Lingolive.Types

printArticleModel :: ArticleModel -> { "TitleMarkup" :: String, "Body" :: String, ipa :: Maybe String }

