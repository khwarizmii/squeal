{-|
Module: Squeal.PostgreSQL.Definition.Constraint
Description: constraint expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

constraint expressions
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Definition.Constraint
  ( -- * Table Constraints
    TableConstraintExpression (..)
  , check
  , unique
  , primaryKey
    -- ** Foreign Keys
  , foreignKey
  , ForeignKeyed
  , OnDeleteClause (..)
  , OnUpdateClause (..)
  ) where

import Control.DeepSeq
import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Data types are a way to limit the kind of data that can be stored in a
-- table. For many applications, however, the constraint they provide is
-- too coarse. For example, a column containing a product price should
-- probably only accept positive values. But there is no standard data type
-- that accepts only positive numbers. Another issue is that you might want
-- to constrain column data with respect to other columns or rows.
-- For example, in a table containing product information,
-- there should be only one row for each product number.
-- `TableConstraint`s give you as much control over the data in your tables
-- as you wish. If a user attempts to store data in a column that would
-- violate a constraint, an error is raised. This applies
-- even if the value came from the default value definition.
newtype TableConstraintExpression
  (sch :: Symbol)
  (tab :: Symbol)
  (db :: SchemasType)
  (constraint :: TableConstraint)
    = UnsafeTableConstraintExpression
    { renderTableConstraintExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL
  (TableConstraintExpression sch tab db constraint) where
    renderSQL = renderTableConstraintExpression

{-| A `check` constraint is the most generic `TableConstraint` type.
It allows you to specify that the value in a certain column must satisfy
a Boolean (truth-value) expression.

>>> :{
type Schema = '[
  "tab" ::: 'Table ('[ "inequality" ::: 'Check '["a","b"]] :=> '[
    "a" ::: 'NoDef :=> 'NotNull 'PGint4,
    "b" ::: 'NoDef :=> 'NotNull 'PGint4
  ])]
:}

>>> :{
let
  definition :: Definition (Public '[]) (Public Schema)
  definition = createTable #tab
    ( (int & notNullable) `as` #a :*
      (int & notNullable) `as` #b )
    ( check (#a :* #b) (#a .> #b) `as` #inequality )
:}

>>> printSQL definition
CREATE TABLE "tab" ("a" int NOT NULL, "b" int NOT NULL, CONSTRAINT "inequality" CHECK (("a" > "b")));
-}
check
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToRow table) subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which are getting checked
  -> (forall t. Condition 'Ungrouped '[] '[] db '[] '[t ::: subcolumns])
  -- ^ a closed `Condition` on those subcolumns
  -> TableConstraintExpression sch tab db ('Check aliases)
check _cols condition = UnsafeTableConstraintExpression $
  "CHECK" <+> parenthesized (renderSQL condition)

{-| A `unique` constraint ensure that the data contained in a column,
or a group of columns, is unique among all the rows in the table.

>>> :{
type Schema = '[
  "tab" ::: 'Table( '[ "uq_a_b" ::: 'Unique '["a","b"]] :=> '[
    "a" ::: 'NoDef :=> 'Null 'PGint4,
    "b" ::: 'NoDef :=> 'Null 'PGint4
  ])]
:}

>>> :{
let
  definition :: Definition (Public '[]) (Public Schema)
  definition = createTable #tab
    ( (int & nullable) `as` #a :*
      (int & nullable) `as` #b )
    ( unique (#a :* #b) `as` #uq_a_b )
:}

>>> printSQL definition
CREATE TABLE "tab" ("a" int NULL, "b" int NULL, CONSTRAINT "uq_a_b" UNIQUE ("a", "b"));
-}
unique
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToRow table) subcolumns )
  => NP Alias aliases
  -- ^ specify subcolumns which together are unique for each row
  -> TableConstraintExpression sch tab db ('Unique aliases)
unique columns = UnsafeTableConstraintExpression $
  "UNIQUE" <+> parenthesized (renderSQL columns)

{-| A `primaryKey` constraint indicates that a column, or group of columns,
can be used as a unique identifier for rows in the table.
This requires that the values be both unique and not null.

>>> :{
type Schema = '[
  "tab" ::: 'Table ('[ "pk_id" ::: 'PrimaryKey '["id"]] :=> '[
    "id" ::: 'Def :=> 'NotNull 'PGint4,
    "name" ::: 'NoDef :=> 'NotNull 'PGtext
  ])]
:}

>>> :{
let
  definition :: Definition (Public '[]) (Public Schema)
  definition = createTable #tab
    ( serial `as` #id :*
      (text & notNullable) `as` #name )
    ( primaryKey #id `as` #pk_id )
:}

>>> printSQL definition
CREATE TABLE "tab" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_id" PRIMARY KEY ("id"));
-}
primaryKey
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToColumns table) subcolumns
     , AllNotNull subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which together form a primary key.
  -> TableConstraintExpression sch tab db ('PrimaryKey aliases)
primaryKey columns = UnsafeTableConstraintExpression $
  "PRIMARY KEY" <+> parenthesized (renderSQL columns)

{-| A `foreignKey` specifies that the values in a column
(or a group of columns) must match the values appearing in some row of
another table. We say this maintains the referential integrity
between two related tables.

>>> :{
type Schema =
  '[ "users" ::: 'Table (
       '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        ])
   , "emails" ::: 'Table (
       '[  "pk_emails" ::: 'PrimaryKey '["id"]
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
        ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
        , "email" ::: 'NoDef :=> 'Null 'PGtext
        ])
   ]
:}

>>> :{
let
  setup :: Definition (Public '[]) (Public Schema)
  setup =
   createTable #users
     ( serial `as` #id :*
       (text & notNullable) `as` #name )
     ( primaryKey #id `as` #pk_users ) >>>
   createTable #emails
     ( serial `as` #id :*
       (int & notNullable) `as` #user_id :*
       (text & nullable) `as` #email )
     ( primaryKey #id `as` #pk_emails :*
       foreignKey #user_id #users #id
         OnDeleteCascade OnUpdateCascade `as` #fk_user_id )
in printSQL setup
:}
CREATE TABLE "users" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_users" PRIMARY KEY ("id"));
CREATE TABLE "emails" ("id" serial, "user_id" int NOT NULL, "email" text NULL, CONSTRAINT "pk_emails" PRIMARY KEY ("id"), CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "users" ("id") ON DELETE CASCADE ON UPDATE CASCADE);

A `foreignKey` can even be a table self-reference.

>>> :{
type Schema =
  '[ "employees" ::: 'Table (
       '[ "employees_pk"          ::: 'PrimaryKey '["id"]
        , "employees_employer_fk" ::: 'ForeignKey '["employer_id"] "employees" '["id"]
        ] :=>
       '[ "id"          :::   'Def :=> 'NotNull 'PGint4
        , "name"        ::: 'NoDef :=> 'NotNull 'PGtext
        , "employer_id" ::: 'NoDef :=>    'Null 'PGint4
        ])
   ]
:}

>>> :{
let
  setup :: Definition (Public '[]) (Public Schema)
  setup =
   createTable #employees
     ( serial `as` #id :*
       (text & notNullable) `as` #name :*
       (integer & nullable) `as` #employer_id )
     ( primaryKey #id `as` #employees_pk :*
       foreignKey #employer_id #employees #id
         OnDeleteCascade OnUpdateCascade `as` #employees_employer_fk )
in printSQL setup
:}
CREATE TABLE "employees" ("id" serial, "name" text NOT NULL, "employer_id" integer NULL, CONSTRAINT "employees_pk" PRIMARY KEY ("id"), CONSTRAINT "employees_employer_fk" FOREIGN KEY ("employer_id") REFERENCES "employees" ("id") ON DELETE CASCADE ON UPDATE CASCADE);

A `foreignKey` can also be cross-schema.

>>> :{
type Pk = '["pk" ::: 'PrimaryKey '["id"]]
type Fk = '["fk" ::: 'ForeignKey '["id"] "here.foo" '["id"]]
type Cols = '["id" ::: 'NoDef :=> 'NotNull 'PGint4]
type DB =
  '[ "here" ::: '["foo" ::: 'Table (Pk :=> Cols)]
   , "there" ::: '["bar" ::: 'Table (Fk :=> Cols)]
   ]
:}

>>> :{
let
  setup :: Definition '[] DB
  setup =
    createSchema #here >>>
    createTable (#here ! #foo)
      (notNullable int4 `as` #id)
      (primaryKey #id `as` #pk) >>>
    createSchema #there >>>
    createTable (#there ! #bar)
      (notNullable int4 `as` #id)
      (foreignKey #id (#here ! #foo) #id
        OnDeleteCascade OnUpdateCascade `as` #fk)
in printSQL setup
:}
CREATE SCHEMA "here";
CREATE TABLE "here"."foo" ("id" int4 NOT NULL, CONSTRAINT "pk" PRIMARY KEY ("id"));
CREATE SCHEMA "there";
CREATE TABLE "there"."bar" ("id" int4 NOT NULL, CONSTRAINT "fk" FOREIGN KEY ("id") REFERENCES "here"."foo" ("id") ON DELETE CASCADE ON UPDATE CASCADE);

-}
foreignKey
  :: ( ForeignKeyed db
        sch0 sch1
        schema0 schema1
        tab0 tab1
        table0 table1
        columns0 columns1
        tys0 tys1 )
  => NP Alias columns1
  -- ^ column or columns in the table
  -> QualifiedAlias sch0 tab0
  -- ^ reference table
  -> NP Alias columns0
  -- ^ reference column or columns in the reference table
  -> OnDeleteClause
  -- ^ what to do when reference is deleted
  -> OnUpdateClause
  -- ^ what to do when reference is updated
  -> TableConstraintExpression sch1 tab1 db
      ('ForeignKey columns1 (References sch0 tab0 sch1) columns0)
foreignKey keys parent refs ondel onupd = UnsafeTableConstraintExpression $
  "FOREIGN KEY" <+> parenthesized (renderSQL keys)
  <+> "REFERENCES" <+> renderSQL parent
  <+> parenthesized (renderSQL refs)
  <+> renderSQL ondel
  <+> renderSQL onupd

-- | A constraint synonym between types involved in a foreign key constraint.
type ForeignKeyed db
  sch0 sch1
  schema0 schema1
  tab0 tab1
  table0 table1
  columns0 columns1
  tys0 tys1 =
    ( Has sch0 db schema0
    , Has sch1 db schema1
    , Has tab0 schema0 ('Table table0)
    , Has tab1 schema1 ('Table table1)
    , HasAll columns0 (TableToColumns table0) tys1
    , HasAll columns1 (TableToColumns table1) tys0
    , SOP.AllZip SamePGType tys0 tys1
    , Uniquely columns0 (TableToConstraints table0)
    )

-- | `OnDeleteClause` indicates what to do with rows that reference a deleted row.
data OnDeleteClause
  = OnDeleteNoAction
    -- ^ if any referencing rows still exist when the constraint is checked,
    -- an error is raised
  | OnDeleteRestrict -- ^ prevents deletion of a referenced row
  | OnDeleteCascade
    -- ^ specifies that when a referenced row is deleted,
    -- row(s) referencing it should be automatically deleted as well
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnDeleteClause
-- | Render `OnDeleteClause`.
instance RenderSQL OnDeleteClause where
  renderSQL = \case
    OnDeleteNoAction -> "ON DELETE NO ACTION"
    OnDeleteRestrict -> "ON DELETE RESTRICT"
    OnDeleteCascade -> "ON DELETE CASCADE"

-- | Analagous to `OnDeleteClause` there is also `OnUpdateClause` which is invoked
-- when a referenced column is changed (updated).
data OnUpdateClause
  = OnUpdateNoAction
  -- ^ if any referencing rows has not changed when the constraint is checked,
  -- an error is raised
  | OnUpdateRestrict -- ^ prevents update of a referenced row
  | OnUpdateCascade
    -- ^ the updated values of the referenced column(s) should be copied
    -- into the referencing row(s)
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnUpdateClause

-- | Render `OnUpdateClause`.
instance RenderSQL OnUpdateClause where
  renderSQL = \case
    OnUpdateNoAction -> "ON UPDATE NO ACTION"
    OnUpdateRestrict -> "ON UPDATE RESTRICT"
    OnUpdateCascade -> "ON UPDATE CASCADE"
