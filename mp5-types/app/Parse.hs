module Parse where

import Lib
import Text.ParserCombinators.Parsec

{- Some parser code lifted from previous MPs -}
tyvar = try $ do {
                 ; spaces
                 ; char 'v'
                 ; num <- many1 digit
                 ; return (TyVar (read num :: Integer))
                 }

pairty = try $ do {
                  ; spaces
                  ; char '('
                  ; tc1 <- tycon
                  ; char ','
                  ; tc2 <- tycon
                  ; char ')'
                  ; return (PairTy tc1 tc2)
                  }

fnty = try $ do {
                ; spaces
                ; char '('
                ; spaces
                ; tc1 <- tycon
                ; spaces
                ; string "->"
                ; spaces
                ; tc2 <- tycon
                ; spaces
                ; char ')'
                ; return (FnTy tc1 tc2)
                }

listty = try $ do {
                  ; spaces
                  ; char '['
                  ; spaces
                  ; tc <- tycon
                  ; spaces
                  ; char ']'
                  ; return (ListTy tc)
                  }

stringty = try $ do {
                    ; spaces
                    ; string "String"
                    ; return StringTy
                    }

intty = try $ do {
                 ; spaces
                 ; string "Integer"
                 ; return IntTy
                 }
boolty = try $ do {
                  ; spaces
                  ; string "Bool"
                  ; return BoolTy
                  }

tycon = listty <|> fnty <|> pairty <|> stringty <|> boolty <|> intty <|> tyvar

parseTy s = case (parse tycon "stdin" s) of
              Right x -> x
              Left x -> error $ show x

listTycon = do {
               ;  tycons <- between (char '[') (char ']') (sepBy1 pairTycon (char ','))
               ;  return tycons
               }

pairTycon = do {
               ; spaces
               ; char '('
               ; ty1 <- tycon
               ; char ','
               ; spaces
               ; ty2 <- tycon
               ; char ')'
               ; return (ty1, ty2)
               }

parseListTy s = case (parse listTycon "stdin" s) of
                  Right x -> x
                  Left x -> error $ show x
