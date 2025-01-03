{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Foldable (traverse_)
import System.Console.Haskeline.Completion (Completion(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Cryptol.REPL.Command as REPL
import qualified Cryptol.REPL.Monad as REPL
import Cryptol.REPL.Monad (REPL)
import Cryptol.Utils.Logger (quietLogger)
import Cryptol.Utils.PP (pp)

import REPL.Haskeline (cryptolCommand)

main :: IO ()
main = defaultMain $
  testGroup "Cryptol API tests"
    [ testGroup "Tab completion tests" $
      map (uncurry replTabCompletionTestCase)
        [ (":ch", ":check")
        , (":check", ":check")
        , (":check ", ":check ")
        , (":check rev", ":check reverse")
        , (":check-docstrings", ":check-docstrings ")
        , (":check-docstrings ", ":check-docstrings ")
        , (":t", ":t")
        , (":t rev", ":t reverse")
        , (":time", ":time ")
        , (":time ", ":time ")
        , (":time rev", ":time reverse")
        , (":type", ":type ")
        , (":type ", ":type ")
        , (":type rev", ":type reverse")
        ]
    ]

-- | Assert a property in the 'REPL' monad and turn it into a test case.
replTestCase :: TestName -> REPL () -> TestTree
replTestCase name replAssertion =
  testCase name $
  REPL.runREPL
    False       -- Don't use batch mode
    False       -- Disable call stacks
    quietLogger -- Ignore all log messages
    replAssertion

-- | Assert that the REPL will tab-complete the given input in one specific way.
replTabCompletionTestCase ::
  -- | The input before the cursor just prior to hitting the tab key.
  String ->
  -- | The expected terminal input after hitting the tab key. Note that although
  -- this is specified as a single 'String', this may actually correspond to
  -- multiple Haskeline 'Completion's under the hood. See the comments below for
  -- details on how multiple 'Completion's are resolved to a single 'String'.
  String ->
  TestTree
replTabCompletionTestCase beforeTab afterTabExpected =
  replTestCase (show beforeTab ++ " --> " ++ show afterTabExpected) $
  do -- Load the prelude so that the REPL knows how to tab-complete prelude
     -- definitions.
     REPL.loadPrelude
     -- Perform the actual tab completion. (Oddly, Haskeline requires that the
     -- input to the left of the cursor should be reversed.)
     (_input, completions) <- cryptolCommand (reverse beforeTab, "")
     -- Check that the results match what is expected. We have to do a bit of
     -- additional work that Haskeline does not do:
     --
     -- 1. If there is exactly one Completion and its replacement value is
     --    empty, then Haskeline will add a space character to the end of the
     --    input on the terminal. Oddly, this space character is /not/ added in
     --    the Completion itself. As such, we have to add this ourselves.
     --
     -- 2. If there are multiple completions, then Haskeline will complete the
     --    terminal input up to the longest common prefix of all the
     --    completions' replacement strings. As such, we compute this longest
     --    common prefix ourselves.
     REPL.io $
       case completions of
         [] -> assertFailure "Expected tab completions, but received none"
         [completion] ->
           do assertFinished completion
              let replace = replacement completion
                  afterTabActual
                    | null replace = -- See (1) above
                                     beforeTab ++ " "
                    | otherwise    = beforeTab ++ replace
              afterTabExpected @=? afterTabActual
         _:_ ->
           do traverse_ assertFinished completions
              let replaceLcp =
                    -- See (2) above
                    longestCommonPrefix (map replacement completions)
                  afterTabActual = beforeTab ++ replaceLcp
              afterTabExpected @=? afterTabActual
  `REPL.catch`
    -- If something goes wrong run running the REPL, report it as a test
    -- failure.
    \x -> REPL.io $ assertFailure $ show $ pp x
  where
    assertFinished :: Completion -> Assertion
    assertFinished completion =
      assertBool ("Tab completion for " ++ display completion ++ " finished")
                 (isFinished completion)

-- | Compute the longest common prefix in a list of lists.
longestCommonPrefix :: forall a. Eq a => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix s = foldr1 lcp s
  where
    lcp :: [a] -> [a] -> [a]
    lcp xs ys = map fst $ takeWhile (uncurry (==))
                        $ zip xs ys
