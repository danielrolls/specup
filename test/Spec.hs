import Prelude hiding (unlines)
import Test.Hspec (describe, hspec, it, shouldBe, expectationFailure)
import Data.Text (Text, unlines)
import qualified Data.Text.Lazy as LazyText

import SpecUp (invokeTemplateOnSpec)

main = hspec $ do

    it "should pass new" $
      invokeTemplateOnSpec testTmpl sampleTestYaml
        `shouldReturnMarkupOf`
      [ "# value1"
      , "# value2"
      ]

shouldReturnMarkupOf output expectedMarkup =
    either
      (expectationFailure . ("expected Right but got Left " <>))
      (`shouldBe` resultLines expectedMarkup)
      output

sampleTestYaml = "- fieldName: value1\n- fieldName: value2"

testTmpl = "{{# .}}# {{{ fieldName }}}\n{{/ .}}"

resultLines = LazyText.unlines
