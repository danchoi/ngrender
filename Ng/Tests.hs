{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Ng.Tests where
import Test.HUnit 
import Ng.Expressions
import Data.Aeson
import Data.String.QQ


runTests = runTestTT tests

tests = test [
    "tests1" ~: "eq (1,2)" ~: (1,2) @=? (1,1+1)
  , "tests2" ~: "eq (2,2)" ~: (2,2) @=? (1,1+1)  -- fails
             ]




