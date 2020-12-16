module Test.Parser.ArticleModel where

import Prelude

import Control.Parallel (parTraverse)
import Data.Traversable (traverse_)
-- | import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Node.Path
import Test.Spec as Test.Spec
import Test.Spec.Reporter as Test.Spec.Reporter
import Test.Spec.Runner as Test.Spec.Runner
import Test.Parser.ArticleModel.Angekommen.Actual as Test.Parser.ArticleModel.Angekommen.Actual
import Test.Parser.ArticleModel.Ankommen.Actual as Test.Parser.ArticleModel.Ankommen.Actual

type GoldenTest =
  { name :: String
  , actualModule :: Module
  }

type GoldenTestWithExpected =
  { name :: String
  , actualModule :: Module
  , expected :: String
  }

goldenTests :: Array GoldenTest
goldenTests =
  [ { name: "Imports", actualModule: Test.Golden.Imports.Actual.actualModule }
  , { name: "Exports", actualModule: Test.Golden.Exports.Actual.actualModule }
  , { name: "DeclData", actualModule: Test.Golden.DeclData.Actual.actualModule }
  , { name: "DeclDataComplex", actualModule: Test.Golden.DeclDataComplex.Actual.actualModule }
  , { name: "DeclType", actualModule: Test.Golden.DeclType.Actual.actualModule }
  , { name: "DeclNewtype", actualModule: Test.Golden.DeclNewtype.Actual.actualModule }
  , { name: "DeclFixity", actualModule: Test.Golden.DeclFixity.Actual.actualModule }
  , { name: "DeclForeign", actualModule: Test.Golden.DeclForeign.Actual.actualModule }
  , { name: "DeclDerive", actualModule: Test.Golden.DeclDerive.Actual.actualModule }
  , { name: "DeclClass", actualModule: Test.Golden.DeclClass.Actual.actualModule }
  , { name: "Boolean", actualModule: Test.Golden.Boolean.Actual.actualModule }
  , { name: "Application", actualModule: Test.Golden.Application.Actual.actualModule }
  , { name: "MultilinePatternMatchingInLet", actualModule: Test.Golden.MultilinePatternMatchingInLet.Actual.actualModule }
  , { name: "MultilinePatternMatchingInLet2", actualModule: Test.Golden.MultilinePatternMatchingInLet2.Actual.actualModule }
  , { name: "MultilinePatternMatchingInWhere", actualModule: Test.Golden.MultilinePatternMatchingInWhere.Actual.actualModule }
  , { name: "MultilinePatternMatchingInWhere2", actualModule: Test.Golden.MultilinePatternMatchingInWhere2.Actual.actualModule }
  , { name: "MultilinePatternMatchingInWhereAndLet2", actualModule: Test.Golden.MultilinePatternMatchingInWhereAndLet2.Actual.actualModule }
  , { name: "Case", actualModule: Test.Golden.Case.Actual.actualModule }
  , { name: "If", actualModule: Test.Golden.If.Actual.actualModule }
  , { name: "Instance", actualModule: Test.Golden.Instance.Actual.actualModule }
  , { name: "InstanceChain", actualModule: Test.Golden.InstanceChain.Actual.actualModule }
  , { name: "ExprRecord", actualModule: Test.Golden.ExprRecord.Actual.actualModule }
  , { name: "ExprRecordAccessor", actualModule: Test.Golden.ExprRecordAccessor.Actual.actualModule }
  , { name: "ExprArray", actualModule: Test.Golden.ExprArray.Actual.actualModule }
  , { name: "Html", actualModule: Test.Golden.Html.Actual.actualModule }
  , { name: "BoundValue", actualModule: Test.Golden.BoundValue.Actual.actualModule }
  ]

addText :: GoldenTest -> Aff GoldenTestWithExpected
addText test = do
  let path = Node.Path.concat ["test", "Golden", test.name, "Expected.txt"]
  absolutePath <- liftEffect $ Node.Path.resolve [] path
  expected <- readTextFile UTF8 absolutePath
  pure $ { name: test.name, actualModule: test.actualModule, expected }

mkAllTests :: Array GoldenTestWithExpected -> Test.Spec.Spec Unit
mkAllTests tests = traverse_ mkTest tests
  where
  mkTest :: GoldenTestWithExpected -> Test.Spec.Spec Unit
  mkTest test = Test.Spec.it test.name do
    let
      actualParsed =
        -- | String.replace (String.unsafeRegex "\\s+$" String.multiline) "" $
        Dodo.print Dodo.plainText Dodo.twoSpaces $ Language.PS.CST.Printers.printModule test.actualModule

    -- | liftEffect $ log actualParsed
    -- | traceM actualParsed
    -- | traceM test.expected
    -- | actualParsed `shouldEqual` test.expected
    actualParsed `textShouldMatch` test.expected

main :: Aff Unit
main = do
  (goldenTestsWithExpected :: Array GoldenTestWithExpected) <- flip parTraverse goldenTests addText

  let
    (allTests :: Test.Spec.Spec Unit) = mkAllTests goldenTestsWithExpected
  Test.Spec.Runner.runSpec' Test.Spec.Runner.defaultConfig [ Test.Spec.Reporter.consoleReporter ] allTests
