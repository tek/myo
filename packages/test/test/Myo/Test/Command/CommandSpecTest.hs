module Myo.Test.Command.CommandSpecTest where

import Polysemy.Test (UnitTest, assertRight, runTestAuto, unitTest, evalEither)
import Ribosome (fromMsgpack, toMsgpack)
import Test.Tasty (TestTree, testGroup)

import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Command.Data.CommandTemplate (
  CommandSegment (SegmentLit, SegmentParam),
  CommandTemplate (CommandTemplate),
  ParamSegment (ParamFlagTemplate, ParamRequired, ParamTemplate),
  parseCommandTemplate,
  )
import Myo.Command.Data.Param (ParamDefault, ParamId)
import Myo.Command.CommandSpec (compileTemplateWith)

spec :: [Text]
spec =
  [
    "run some command --arg {par1} --arg \\{par2\\} {par4?--verbose}",
    "literal line",
    "do stuff {par1:sub {par1} {par2} \\{par3\\}} {par3}"
  ]

target :: CommandTemplate
target =
  CommandTemplate spec [
    [
      SegmentLit "run some command --arg ",
      SegmentParam "par1" ParamRequired,
      SegmentLit " --arg {par2} ",
      SegmentParam "par4" (ParamFlagTemplate [SegmentLit "--verbose"])
    ],
    [SegmentLit "literal line"],
    [
      SegmentLit "do stuff ",
      SegmentParam "par1" (ParamTemplate [
        SegmentLit "sub ",
        SegmentParam "par1" ParamRequired,
        SegmentLit " ",
        SegmentParam "par2" ParamRequired,
        SegmentLit " {par3}"
      ]),
      SegmentLit " ",
      SegmentParam "par3" ParamRequired
    ]
  ]

test_parseCommandTemplate :: UnitTest
test_parseCommandTemplate =
  runTestAuto do
    assertRight target (parseCommandTemplate (Right spec))

params :: Map ParamId ParamDefault
params =
  [("par1", "val1"), ("par2", "val2")]

targetSpec :: CommandSpec
targetSpec =
  CommandSpec target params

test_decodeCommandSpec :: UnitTest
test_decodeCommandSpec =
  runTestAuto do
    assertRight targetSpec (fromMsgpack (toMsgpack targetSpec))

spec_compile :: Text
spec_compile = "rec: {rec}"

template_compile :: Either Text CommandTemplate
template_compile =
  parseCommandTemplate (Left spec_compile)

target_compile :: [Text]
target_compile = ["rec: sub1: sub2: panda"]

test_compileCommandTemplate :: UnitTest
test_compileCommandTemplate =
  runTestAuto do
    tpl <- evalEither template_compile
    assertRight target_compile (compileTemplateWith tpl mempty values)
  where
    values =
      [
        ("rec", "sub1: {sub1}"),
        ("sub1", "sub2: {sub2}"),
        ("sub2", "panda")
      ]

test_commandSpec :: TestTree
test_commandSpec =
  testGroup "command spec" [
    unitTest "parse template" test_parseCommandTemplate,
    unitTest "decode spec" test_decodeCommandSpec,
    unitTest "compile template" test_compileCommandTemplate
  ]
