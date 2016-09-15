module Tests where

basicIntsSymsOrig :: ([[String]], [String])
basicIntsSymsOrig = (   [ ["435"]
                        , ["a"]
                        , ["a", "555"]
                        , ["555", "a"]
                        ]

                    ,   [ "Do you handle basic integer/symbol parsing correctly?"
                        , "Do you generate an `ExnVal` on failed symbol lookup?"
                        , "Do you continue evaluation in the `repl` even after a parse error?"
                        ]
                    )

primitiveIntsOrig :: ([[String]], [String])
primitiveIntsOrig = (   [ ["+", "-", "*"]
                        , [">", "<", ">=", "<=", "=", "!="]
                        , ["(+ 3 4 2 10)", "(* 3 3 3 2)"]
                        , ["(+ 2 (* 3 4))", "(- 20 1)", "(- 10 5 2)", "(- 10 (+ 4 5 (* 3 5)) (- 14 2) 22)"]
                        , ["(> 5 3)", "(> 6  4 2)", "(> 6 4 2 6)"]
                        , ["(>= 5 5 3)", "(>= 5 6 3)", "(<= 2 3 4)", "(<= 2 2 3)", "(<= 2 1 2)"]
                        , [ "(= 5 5 5 5)", "(= 5 3 5 2)", "(= 5 (+ 2 3) (- 8 3))"
                          , "(= 4 2)", "(= 4 (- 4 1) (+ 3 1))"
                          , "(!= 1 2 1)", "(!= 1 1 2)", "(!= 22 22 22)"
                          , "(!= (+ 1 22) 23 22)", "(!= (+ 1 22) 24 26)"
                          ]
                        ]

                    ,   [ "Do you print out `*primitive*` when a primitive operator is on its own?"
                        , "Does your parer recursively handle nested expressions?"
                        , "Do you handle subtraction (correctly) like this: (- 10 5 2) === (10 - 5) - 2 ?"
                        ]
                    )

primitiveBoolsOrig :: ([[String]], [String])
primitiveBoolsOrig =    (   [ ["and", "or", "not"]
                            , [ "(and 't 't)", "(and 't 'nil)", "(and 't 't 't 't)"
                              , "(and 't 'nil 6 4)", "(and 't 5)", "(and (> 4 2) (> 5 2))"
                              , "(and (> 4 2) (> 2 5))"
                              ]
                            , [ "(or 't 't)", "(or 't 'nil)", "(or 'nil 'nil)", "(or 5 2 6)"
                              , "(or (and 't 't) 'nil)", "(or (and 'nil 't) 6)"
                              ]
                            , ["(not (> 5 3))", "(not (< 5 3))", "(not 't 't)", "(not 'nil 't 't 't)"]
                            ]

                        ,   [ "Do you handle the boolean operators `and` and `or` correctly?"
                            , "Do you handle the unary operator `not` correctl?"
                            ]
                        )

primitiveMiscOrig :: ([[String]], [String])
primitiveMiscOrig = (   [ ["()"]
                        , ["eq?"]
                        , [ "(+)", "(-)", "(*)", "(>)", "(<)", "(>=)", "(<=)", "(=)", "(!=)"
                          , "(eq?)", "(and)", "(or)", "(not)", "(list)", "(car)", "(cdr)"
                          ]
                        , [ "(+ 1)", "(- 1)", "(* 1)", "(> 1)", "(< 1)", "(>= 1)", "(<= 1)", "(= 1)", "(!= 1)"
                          , "(eq? 'a)", "(eq? 1)", "(and 't)", "(or 'nil)", "(not 't)" , "(list 't)"
                          ]
                        , [ "(eq? 5 5)", "(eq? 6 5 6)", "(eq? (+ 3 3) 6)"
                          , "(eq? 'a 5)", "(eq? 'a 'b)", "(eq? 'a 'a 'b)", "(eq? 'a 'a 'a)"
                          ]
                        ]

                    ,   [ "Do you handle the empty form `()` correctly?"
                        , "Do you handle the base-cases of the primitive operators correctly?"
                        , "Do you handle `eq?` correctly?"
                        , "Do you handle singleton cases properly for primitives (single argument)?"
                        ]
                    )

consListCarCdrOrig :: ([[String]], [String])
consListCarCdrOrig =    (   [ ["cons", "list", "car", "cdr"]
                            , [ "(list (> 3 4) 't 15 'nil (< 5 2 3 5))"
                              , "(car (list 'a 'b))", "(cdr (list 'a 'b))", "(car (list 'a 'b 'c))"
                              , "(cdr (list 'a 'b 'c))", "(cdr (list 'a))", "(cdr 'a)"
                              ]
                            , ["(cons 2 3)"]
                            , ["(cons 2 (cons 3 4))", "(cons 2 (cons 3 (cons 4 'nil)))"]
                            , [ "(car (cons 2 (cons 3 (cons 4 'nil))))", "(cdr (cons 2 (cons 3 (cons 4 'nil))))"
                              , "(car (list 2 3 4 'a 'b))", "(cdr (list 'b 'c 3 4 (+ 3 4)))"
                              ]
                            , [ "(car (cons 1 2) (cons 2 3))", "(cdr (cons 2 't) (list 4 2 4 5))" ]
                            ]

                        ,   [ "Are you handling primitives `list`, `car`, and `cdr` correctly?"
                            , "Are you handling the `cons` form correctly?"
                            , "Do you produce an `ExnVal` if `car` or `cdr` is applied to too many arguments?"
                            ]
                        )

defDefineLambdaApplyOrig :: ([[String]], [String])
defDefineLambdaApplyOrig =  (   [ ["(def x (+ 10 20))", "x", "y"]
                                , ["(def x 1)", "(define inc (y) (+ y x))", "(inc 10)", "(def x 2)", "(inc 10)"]
                                , [ "(lambda (x) (+ x 10))", "( (lambda (x) (+ x 10)) 20)"
                                  , "(define mkInc (x) (lambda (y) (+ x y)))", "(def i2 (mkInc 2))", "(i2 10)"
                                  ]
                                , [ "(define f ((+ 3 4) x y) (+ x y))", "(def (+ 3 4) 5)" ]
                                , [ "(define f (x y z) (+ (- x y) z))", "(f 3 4 2 8 0)" ]
                                ]

                            ,   [ "Do you handle constant definition correctly?"
                                , "Do you handle function definition/closure application correctly?"
                                , "Do you handle printing a `Closure` as `*closure*`?"
                                , "Do you handle `def` and `define` having non-symbol parameter names?"
                                , "For a `Closure` applied to extra arguments, are the extra ones thrown away?"
                                ]
                            )

allowsDefOverPrimFormOrig :: ([[String]], [String])
allowsDefOverPrimFormOrig = (   [ [ "(def define 3)", "define" ]
                                , [ "(define let (x y z) (+ x y z))", "(let 3 4 5)"
                                  , "(let ((x 5) (y 100) (z 3)) (+ x (- y z)))"
                                  ]
                                , [ "(def + 3)" , "(- + 2)", "(* + 10)" ]
                                , [ "(define quote (x y) (list x y))", "(quote 'a 'b)", "(quote 'a)" ]
                                ]

                            ,   [ "Do you allow users to `define` or `def` variables with the same name"
                                , "as primitive operators? If you redefine a form (such as `let` or `quote`)"
                                , "as an operator, but it's used correctly as a form, the definition as a"
                                , "form should be used. Otherwise the environment lookup should be used."
                                ]
                            )

condFormOrig :: ([[String]], [String])
condFormOrig =  (   [ [ "(cond ((> 4 3) 'a (> 4 2) 'b))", "(cond ((< 4 3) 'a (> 4 2) 'b))"
                      , "(cond ((< 4 3) 'a (< 4 2) 'b))"
                      ]
                    ]

                ,   [ "Do you handle the `cond` form correctly in `eval`?" ]
                )

letFormOrig :: ([[String]], [String])
letFormOrig =   (   [ [ "(let ((x 5) (y 10)) (+ x y))", "(def x 20)", "(def y 30)"
                      , "(let ((x 11) (y 4)) (- (* x y) 2))", "x", "y"
                      ]
                    ]

                ,   [ "Do you handle the `let` form correctly in `eval`?" ]
                )



everythingElseOrig :: ([[String]], [String])
everythingElseOrig =    (   [ ["435"]
                            , ["(def x 5)", "x", "y"]
                            , ["(f 10 30 x)"]
                            , ["+"]
                            , ["()"]
                            , ["'a", "'5", "(quote a)" , "'a", "'asdf", "'*first-val*"]
                            , [ "(define fact (n) (cond ((< n 1) 1 't (* n (fact (- n 1))))))", "(fact 5)"]
                            , [ "'a", "''a", "(car (quote (a b c)))", "(car '(a b c))"
                              , "(car ''(a b c))", "'(2 3 4)", "(list (+ 2 3))", "'( (+ 2 3))", "'(+ 2 3)"
                              ]
                            , [ "'(+ 1 2)", "(eval '(+ 1 2))", "(eval ''(+ 1 2))", "(eval (eval ''(+ 1 2)))"
                              , "(def a '(+ x 1))", "(def x 5)", "(eval a)"
                              ]
                            , ["(def a 5)", "`(+ a 1)", "`(+ ,a 1)"]
                            , [ "(defmacro if (con then else) `(cond (,con ,then 't ,else)))"
                              , "(def a 5)", "(if (> a 2) 10 20)", "(if (< a 2) 10 20)"
                              , "(define fact (n) (if (< n 1) 1 (* n (fact (- n 1)))))"
                              , "(fact 10)", "(defmacro mkplus (e) (if (eq? (car e) '-) (cons '+ (cdr e)) e))"
                              , "(mkplus (- 5 4))"
                              ]
                            ]

                        ,   [ "Do you handle quotes, unquotes, quasiquotes, and macros correctly?" ]
                        )

nestedQuasiQuotesOrig :: ([[String]], [String])
nestedQuasiQuotesOrig = (   [ [ "(def a 5)", "``(+ ,,a 1)", "``(+ ,,a ,a)", "`(+ a ,,a)", "``(+ a ,,a)"
                              , "(eval ``(+ ,,a 1))", "(eval (eval ``(+ ,,a 1)))"
                              ]
                            , [ "(def a 5)", "```(+ ,,,a ,,a)", "```(+ ,a ,,a)", "```(+ `a `(+ ,,,,a a))" ]
                            , [ "(def a 5)", "```a", "```,a", "```,,a", "```,,,a"
                              , "(eval ```,,,a)", "(eval (eval ```,,,a))"
                              ]
                            ]

                        ,   [ "Do you handle nested quasiquotes?" ]
                        )

