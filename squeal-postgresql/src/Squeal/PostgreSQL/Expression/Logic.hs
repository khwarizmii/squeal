{-|
Module: Squeal.PostgreSQL.Expression.Logic
Description: logical expressions and operators
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

logical expressions and operators
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Logic
  ( -- * Condition
    Condition
  , true
  , false
    -- * Logic
  , not_
  , (.&&)
  , (.||)
    -- * Conditional
  , caseWhenThenElse
  , ifThenElse
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- | A `Condition` is an `Expression`, which can evaluate
-- to `true`, `false` or `Squeal.PostgreSQL.Null.null_`. This is because SQL uses
-- a three valued logic.
type Condition grp lat with db params from =
  Expression grp lat with db params from ('Null 'PGbool)

-- | >>> printSQL true
-- TRUE
true :: Expr (null 'PGbool)
true = UnsafeExpression "TRUE"

-- | >>> printSQL false
-- FALSE
false :: Expr (null 'PGbool)
false = UnsafeExpression "FALSE"

-- | >>> printSQL $ not_ true
-- (NOT TRUE)
not_ :: null 'PGbool --> null 'PGbool
not_ = unsafeLeftOp "NOT"

-- | >>> printSQL $ true .&& false
-- (TRUE AND FALSE)
(.&&) :: Operator (null 'PGbool) (null 'PGbool) (null 'PGbool)
infixr 3 .&&
(.&&) = unsafeBinaryOp "AND"

-- | >>> printSQL $ true .|| false
-- (TRUE OR FALSE)
(.||) :: Operator (null 'PGbool) (null 'PGbool) (null 'PGbool)
infixr 2 .||
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression grp lat with db params from (null 'PGint2)
--   expression = caseWhenThenElse [(true, 1), (false, 2)] 3
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN (1 :: int2) WHEN FALSE THEN (2 :: int2) ELSE (3 :: int2) END
caseWhenThenElse
  :: [ ( Condition grp lat with db params from
       , Expression grp lat with db params from ty
     ) ]
  -- ^ whens and thens
  -> Expression grp lat with db params from ty
  -- ^ else
  -> Expression grp lat with db params from ty
caseWhenThenElse whenThens else_ = UnsafeExpression $ mconcat
  [ "CASE"
  , mconcat
    [ mconcat
      [ " WHEN ", renderSQL when_
      , " THEN ", renderSQL then_
      ]
    | (when_,then_) <- whenThens
    ]
  , " ELSE ", renderSQL else_
  , " END"
  ]

-- | >>> :{
-- let
--   expression :: Expression grp lat with db params from (null 'PGint2)
--   expression = ifThenElse true 1 0
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN (1 :: int2) ELSE (0 :: int2) END
ifThenElse
  :: Condition grp lat with db params from
  -> Expression grp lat with db params from ty -- ^ then
  -> Expression grp lat with db params from ty -- ^ else
  -> Expression grp lat with db params from ty
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_
