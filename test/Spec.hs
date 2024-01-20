

import Prelude hiding (unlines)
import Test.Hspec (describe, hspec, it, shouldBe, expectationFailure)
import Data.Text (Text, unlines)
import qualified Data.Text.Lazy as LazyText

import SpecUp (invokeTemplateOnSpec)

sampleTestYaml = "- name: bbb\n- name: ddd"

testTmpl = "{{# .}}# {{{ name }}}\n{{/ .}}"

expectedResult = LazyText.unlines
  [ "# bbb"
  , "# ddd"
  ]

main = hspec $ do

    it "should pass new" $
      invokeTemplateOnSpec testTmpl sampleTestYaml
        `shouldReturnMarkupOf`
      expectedResult

shouldReturnMarkupOf output expectedMarkup =
    either
      (expectationFailure . ("expected Right but got Left " <>))
      (`shouldBe` expectedMarkup)
      output

