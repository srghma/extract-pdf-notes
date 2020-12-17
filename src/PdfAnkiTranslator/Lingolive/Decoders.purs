module PdfAnkiTranslator.Lingolive.Decoders where

import Data.Argonaut.Decode
import PdfAnkiTranslator.Lingolive.DecodeUtils
import PdfAnkiTranslator.Lingolive.Types
import Protolude
import Affjax as Affjax
import Control.Monad.Except (withExceptT)
import Control.Monad.ST (ST)
import Control.Monad.ST as Control.Monad.ST
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Array as Array
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject

decodeNodeType_Text :: forall r. STObject r Json -> ExceptT JsonDecodeError (ST r) NodeType_Text
decodeNodeType_Text stobj = do
  isItalics <- stobj `getFieldAndPop` "IsItalics"
  isAccent <- stobj `getFieldAndPop` "IsAccent"
  text <- stobj `getFieldAndPop` "Text"
  isOptional <- stobj `getFieldAndPop` "IsOptional"
  pure
    $ NodeType_Text
        { "IsItalics": isItalics
        , "IsAccent": isAccent
        , "Text": text
        , "IsOptional": isOptional
        }

decodeNodeType_Text' :: Json -> Either JsonDecodeError NodeType_Text
decodeNodeType_Text' =
  decodeJson
    >=> decodeObjectAndUseUpAllFields \stobj ->
        withExceptT (Named "NodeType_Text") do
          (_ :: String) <- stobj `getFieldAndPop` "Node"
          decodeNodeType_Text stobj

decodeNodeType :: Json -> Either JsonDecodeError NodeType
decodeNodeType json =
  let
    decodeNodeType_Example :: Json -> Either JsonDecodeError NodeType_Example
    decodeNodeType_Example =
      decodeJson
        >=> \obj -> do
            (markup :: Array NodeType_Text) <- Decoders.getField (Decoders.decodeArray decodeNodeType_Text') obj "Markup"
            (isOptional :: Boolean) <- obj `getField` "IsOptional"
            pure $ NodeType_Example { "Markup": markup, "IsOptional": isOptional }

    decodeNodeType_ExampleItem :: Json -> Either JsonDecodeError NodeType_ExampleItem
    decodeNodeType_ExampleItem =
      decodeJson
        >=> \obj -> do
            (markup :: Array NodeType_Example) <- Decoders.getField (Decoders.decodeArray decodeNodeType_Example) obj "Markup"
            (isOptional :: Boolean) <- obj `getField` "IsOptional"
            pure $ NodeType_ExampleItem { "Markup": markup, "IsOptional": isOptional }

    decodeNodeType_ListItem :: Json -> Either JsonDecodeError NodeType_ListItem
    decodeNodeType_ListItem =
      decodeJson
        >=> \obj -> do
            (markup :: Array NodeType) <- Decoders.getField (Decoders.decodeArray decodeNodeType) obj "Markup"
            pure $ NodeType_ListItem { "Markup": markup }
  in
    decodeJson json
      >>= decodeObjectAndUseUpAllFields \stobj ->
          stobj `getFieldAndPop` "Node"
            >>= case _ of
                "Comment" ->
                  withExceptT (Named "NodeType__Comment") do
                    (_ :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
                    (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    markup <- getFieldAndPop' (Decoders.decodeArray decodeNodeType) stobj "Markup"
                    pure
                      $ NodeType__Comment
                          { "IsOptional": isOptional
                          , "Markup": markup
                          }
                "Paragraph" ->
                  withExceptT (Named "NodeType__Paragraph") do
                    (text :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
                    (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    markup <- getFieldAndPop' (Decoders.decodeArray decodeNodeType) stobj "Markup"
                    pure
                      $ NodeType__Paragraph
                          { "IsOptional": isOptional
                          , "Markup": markup
                          }
                "Text" -> withExceptT (Named "NodeType__Text") $ decodeNodeType_Text stobj <#> NodeType__Text
                "List" ->
                  withExceptT (Named "NodeType__List") do
                    (_type :: Int) <- getFieldAndPop' Decoders.decodeInt stobj "Type"
                    (items :: Array NodeType_ListItem) <- getFieldAndPop' (Decoders.decodeArray decodeNodeType_ListItem) stobj "Items"
                    (_ :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
                    (_ :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    pure
                      $ NodeType__List
                          { "Type": _type
                          , "Items": items
                          }
                "Examples" ->
                  withExceptT (Named "NodeType__Examples") do
                    (_ :: Unit) <- getFieldAndPop' decodeNull stobj "Type"
                    (items :: Array NodeType_ExampleItem) <- getFieldAndPop' (Decoders.decodeArray decodeNodeType_ExampleItem) stobj "Items"
                    (_ :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
                    (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    pure
                      $ NodeType__Examples
                          { "IsOptional": isOptional
                          , "Items": items
                          }
                "CardRefs" -> withExceptT (Named "NodeType__CardRefs") $ except $ Left MissingValue
                "CardRefItem" -> withExceptT (Named "NodeType__CardRefItem") $ except $ Left MissingValue
                "CardRef" ->
                  withExceptT (Named "NodeType__CardRef") do
                    dictionary <- stobj `getFieldAndPop` "Dictionary"
                    articleId <- stobj `getFieldAndPop` "ArticleId"
                    (text :: String) <- stobj `getFieldAndPop` "Text"
                    (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    pure
                      $ NodeType__CardRef
                          { "Dictionary": dictionary
                          , "ArticleId": articleId
                          , "Text": text
                          , "IsOptional": isOptional
                          }
                "Transcription" -> withExceptT (Named "NodeType__Transcription") $ except $ Left MissingValue
                "Abbrev" ->
                  withExceptT (Named "NodeType__Abbrev") do
                    (fullText :: String) <- stobj `getFieldAndPop` "FullText"
                    (text :: String) <- stobj `getFieldAndPop` "Text"
                    (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    pure
                      $ NodeType__Abbrev
                          { "FullText": fullText
                          , "Text": text
                          , "IsOptional": isOptional
                          }
                "Caption" -> withExceptT (Named "NodeType__Caption") $ except $ Left MissingValue
                "Sound" ->
                  withExceptT (Named "NodeType__Sound") do
                    (text :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
                    (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
                    (fileName :: String) <- stobj `getFieldAndPop` "FileName"
                    pure
                      $ NodeType__Sound
                          { "IsOptional": isOptional
                          , "FileName": fileName
                          }
                "Ref" -> withExceptT (Named "NodeType__Ref") $ except $ Left MissingValue
                "Unsupported" -> withExceptT (Named "NodeType__Unsupported") $ except $ Left MissingValue
                other -> except $ Left $ AtKey "Node" $ UnexpectedValue $ Json.fromString other

decodeArticleNode :: Json -> Either JsonDecodeError ArticleNode
decodeArticleNode = decodeNodeType >>> map ArticleNode

decodeArticleModel :: Json -> Either JsonDecodeError ArticleModel
decodeArticleModel =
  decodeJson
    >=> \(obj :: Object Json) -> do
        title <- obj .: "Title"
        titleMarkup <- Decoders.getField (Decoders.decodeArray decodeArticleNode) obj "TitleMarkup"
        dictionary <- obj .: "Dictionary"
        articleId <- obj .: "ArticleId"
        body <- Decoders.getField (Decoders.decodeArray decodeArticleNode) obj "Body"
        pure
          $ ArticleModel
              { "Title": title
              , "TitleMarkup": titleMarkup
              , "Dictionary": dictionary
              , "ArticleId": articleId
              , "Body": body
              }

decodeArticleModels :: Json -> Either JsonDecodeError (Array ArticleModel)
decodeArticleModels = Decoders.decodeArray decodeArticleModel
