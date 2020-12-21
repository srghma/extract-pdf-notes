module PdfAnkiTranslator.Print where

import Data.Foldable
import PdfAnkiTranslator.Lingolive.Types
import Protolude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.String as String
import PdfAnkiTranslator.SimpleXMLWithIndentation

type AnkiFields =
  { question      :: String -- orig lang
  , transcription :: String
  , answer        :: String -- my lang (using google translate)
  , myContext     :: String
  , body          :: String -- examples, etc.
  }

-- from "Body"
findTranscriptionFromBody :: NonEmptyArray ArticleModel -> String -- maybe empty
findTranscriptionFromBody = fromMaybe "" <<< findMap \(ArticleModel articleModel) ->
  flip findMap articleModel."Body" \(ArticleNode nodeType) -> findFromNodeType nodeType
  where
    findFromNodeType :: NodeType -> Maybe String
    findFromNodeType =
      case _ of
           NodeType__Transcription x -> Just x."Text"
           NodeType__Comment x -> findMap findFromNodeType x."Markup"
           NodeType__Paragraph x -> findMap findFromNodeType x."Markup"
           NodeType__Text _ -> Nothing
           NodeType__List x -> findMap findNodeType_ListItem x."Items"
           NodeType__Examples x -> Nothing
           NodeType__CardRefs -> Nothing
           NodeType__CardRefItem -> Nothing
           NodeType__CardRef _ -> Nothing
           NodeType__Abbrev _ -> Nothing
           NodeType__Caption -> Nothing
           NodeType__Sound _ -> Nothing
           NodeType__Ref -> Nothing
           NodeType__Unsupported -> Nothing
      where
        findNodeType_ListItem :: NodeType_ListItem -> Maybe String
        findNodeType_ListItem (NodeType_ListItem x) = findMap findFromNodeType x."Markup"

printBodyFromAbbyy :: NonEmptyArray ArticleModel -> String
printBodyFromAbbyy = String.joinWith "\n" <<< map printArticleModel <<< NonEmptyArray.toArray
  where
    printArticleModel :: ArticleModel -> String
    printArticleModel (ArticleModel x) = String.joinWith "\n" $ [ String.joinWith "" $ map printArticleNode x."TitleMarkup" ] <> (map printArticleNode x."Body")

    wrapText :: forall r . { "IsItalics" :: Boolean, "IsAccent" :: Boolean | r } -> String -> String
    wrapText x str =
      let
        props = Array.catMaybes
          [ if x."IsItalics" then Just (Tuple "font-style" "italic") else Nothing
          , if x."IsAccent" then Just (Tuple "color" "red") else Nothing
          ]
      in
        if null props
          then str
          else tagOneline "span" props str

    printArticleNode :: ArticleNode -> String
    printArticleNode (ArticleNode x) = printNodeType x

    printNodeType_Text :: NodeType_Text -> String
    printNodeType_Text (NodeType_Text x) = wrapText x $ x."Text"

    printNodeType :: NodeType -> String
    printNodeType =
      case _ of
        NodeType__Transcription x -> x."Text"
        NodeType__Comment x -> tagOneline "span" [ Tuple "color" "grey" ] (String.joinWith " " $ map printNodeType x."Markup")
        NodeType__Paragraph x -> String.joinWith " " $ map printNodeType x."Markup"
        NodeType__Text x -> printNodeType_Text x
        NodeType__List x -> tagMultiLine "ul" [] (map printNodeType_ListItem x."Items")
        NodeType__Examples x -> String.joinWith "\n" $ map printNodeType_ExampleItem x."Items"
        NodeType__CardRefs -> ""
        NodeType__CardRefItem -> ""
        NodeType__CardRef _ -> ""
        NodeType__Abbrev x -> x."FullText"
        NodeType__Caption -> ""
        NodeType__Sound _ -> ""
        NodeType__Ref -> ""
        NodeType__Unsupported -> ""
      where
        printNodeType_ListItem :: NodeType_ListItem -> String
        printNodeType_ListItem (NodeType_ListItem x) = tagMultiLine "li" [] (map printNodeType x."Markup")

        printNodeType_ExampleItem :: NodeType_ExampleItem -> String
        printNodeType_ExampleItem (NodeType_ExampleItem x) = String.joinWith " " $ map printNodeType_Example x."Markup"

        printNodeType_Example :: NodeType_Example -> String
        printNodeType_Example (NodeType_Example x) = String.joinWith "" $ map printNodeType_Text x."Markup"

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
  , answer:        String.joinWith ", " (NonEmptyArray.toArray input.fromGoogleTranslate)
  , myContext:     input.sentence <> maybe "" (\x -> " [" <> x <> "]") input.annotation_content
  , body:          printBodyFromAbbyy input.fromAbbyy
  }
