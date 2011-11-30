module L03.Parser.Tests where

import Test.HUnit hiding (test, Test)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import L01.Validation
import L03.Person
import L03.Parser

main :: 
  IO ()
main = 
  defaultMain [test]

test :: 
  Test
test =
  testGroup "Parser"
    [
      testCase "No input" testcase_noInput
    , testCase "Age must be a positive integer" testcase_agePositive
    , testCase "First name must start with a capital letter" testcase_firstNameCapital
    , testCase "Surname must have at least 5 characters following the first" testcase_firstNameCapital
    , testCase "Surname must start with a capital letter" testcase_surnameCapital
    , testCase "Gender must be 'm' or 'f'" testcase_gender
    , testCase "Phone number (body) must be digits, dots or hyphens" testcase_phoneBody
    , testCase "Phone number must start with a digit" testcase_phoneStart
    , testCase "Phone number must end with a hash" testcase_phoneEnd
    , testCase "Success with no further input" testcase_successNoMore
    , testCase "Success with further input" testcase_successMore
    ]

testcase_noInput :: 
  Assertion
testcase_noInput =
  assert (isError (parse personParser ""))

testcase_agePositive :: 
  Assertion
testcase_agePositive =
  assert (isError (parse personParser "12x Fred Clarkson m 123-456.789#"))

testcase_firstNameCapital :: 
  Assertion
testcase_firstNameCapital =
  assert (isError (parse personParser "123 fred Clarkson m 123-456.789#"))

testcase_surname5 :: 
  Assertion
testcase_surname5 =
  assert (isError (parse personParser "123 Fred Cla m 123-456.789#"))

testcase_surnameCapital :: 
  Assertion
testcase_surnameCapital =
  assert (isError (parse personParser "123 Fred clarkson m 123-456.789#"))

testcase_gender :: 
  Assertion
testcase_gender =
  assert (isError (parse personParser "123 Fred Clarkson x 123-456.789#"))

testcase_phoneBody :: 
  Assertion
testcase_phoneBody =
  assert (isError (parse personParser "123 Fred Clarkson m 1x3-456.789#"))

testcase_phoneStart :: 
  Assertion
testcase_phoneStart =
  assert (isError (parse personParser "123 Fred Clarkson m -123-456.789#"))

testcase_phoneEnd :: 
  Assertion
testcase_phoneEnd =
  assert (isError (parse personParser "123 Fred Clarkson m 123-456.789"))

testcase_successNoMore :: 
  Assertion
testcase_successNoMore =
  parse personParser "123 Fred Clarkson m 123-456.789#" @?= Value ([], Person 123 "Fred" "Clarkson" 'm' "123-456.789")

testcase_successMore :: 
  Assertion
testcase_successMore =
  parse personParser "123 Fred Clarkson m 123-456.789# rest" @?= Value (" rest", Person 123 "Fred" "Clarkson" 'm' "123-456.789")

