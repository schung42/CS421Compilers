module Autograder where
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary


silentQC p = quickCheckWithResult stdArgs { chatty = False } p

testProp (prop, propName, tests) = do {
                                      ; result <- silentQC prop
                                      ; if (not $ isSuccess result)
                                        then (do {
                                                 ; putStrLn ("Failed QuickChecks for " ++ propName ++ ".")
                                                 ; mapM_ putStrLn (map (\test ->
                                                     if (not ((prop test)))
                                                     then ("\tFailed Test: " ++ (show test))
                                                     else ("\tPassed Test: " ++ (show test))
                                                   ) tests)
                                              })
                                       else putStrLn ("Passed QuickChecks for " ++ propName ++ ".")
                                      }

testCases (function, propName, tests) = do {
                                          ; putStrLn ("Running tests for " ++ propName ++ ".")
                                          ; mapM_ putStrLn (map (\(test, result) ->
                                              if (function test /= result)
                                              then do { ("\tFailed Test: " ++ (show test))
                                                      }
                                              else ("\tPassed Test: " ++ (show test))
                                            ) tests)
                                        }


