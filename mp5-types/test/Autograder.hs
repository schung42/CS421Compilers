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
                                                 ; print result
                                                 ; mapM_ putStrLn (map (\test ->
                                                     if (not ((prop test)))
                                                     then ("\tFailed Test: " ++ (show test))
                                                     else ("\tPassed Test: " ++ (show test))
                                                   ) tests)
                                              })
                                       else putStrLn ("Passed QuickChecks for " ++ propName ++ ".")
                                      }

testPropRet (prop, propName, tests) = do {
                                         ; result <- silentQC prop
                                         ; if (not $ isSuccess result)
                                           then (do {
                                                    ; putStrLn ("\nFailed QuickChecks for " ++ propName ++ ".")
                                                    ; putStrLn ""
                                                    ; putStrLn $ output result
                                                    ; mapM_ putStrLn (map (\test ->
                                                        if (not ((prop test)))
                                                        then ("\tFailed Test: " ++ (show test))
                                                        else ("\tPassed Test: " ++ (show test))
                                                      ) tests)
                                                    ; return False
                                                 })
                                          else (do {
                                                   ; putStrLn ("Passed QuickChecks for " ++ propName ++ ".")
                                                   ; return True
                                                   }
                                               )
                                         }

testPropOnly (prop, propName) = do {
                                   ; result <- silentQC prop
                                   ; if (not $ isSuccess result)
                                     then (do {
                                              ; putStrLn ("Failed QuickChecks for " ++ propName ++ ".")
                                          })
                                     else putStrLn ("Passed QuickChecks for " ++ propName ++ ".")
                                   }

testPropOnlyRet (prop, propName) = do {
                                      ; result <- silentQC prop
                                      ; if (not $ isSuccess result)
                                        then (do {
                                                 ; putStrLn ("Failed QuickChecks for " ++ propName ++ ".")
                                                 ; putStrLn ""
                                                 ; putStrLn $ output result
                                                 ; return False
                                             })
                                        else (do {
                                                 ; putStrLn ("Passed QuickChecks for " ++ propName ++ ".")
                                                 ; return True 
                                                 }
                                             )
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


