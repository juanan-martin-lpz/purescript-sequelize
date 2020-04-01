{-
 Copyright (c) 2012-2017 "JUSPAY Technologies"
 JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]

 This file is part of JUSPAY Platform.

 JUSPAY Platform is free software: you can redistribute it and/or modify
 it for only educational purposes under the terms of the GNU Affero General
 Public License (GNU AGPL) as published by the Free Software Foundation,
 either version 3 of the License, or (at your option) any later version.
 For Enterprise/Commerical licenses, contact <info@juspay.in>.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 be liable for all damages without limitation, which is caused by the
 ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 The end user has NO right to claim any indemnification based on its use
 of Licensed Software. See the GNU Affero General Public License for more details.

 You should have received a copy of the GNU Affero General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
-}

module Sequelize.Models
  ( makeModelOf
  , sync
  , drop
  , hasOne
  , hasMany
  , belongsTo
  , belongsToMany
  ) where

import Data.Tuple
import Prelude

import Control.Promise (Promise, toAff)
import Data.Bifunctor (rmap)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe, fromJust, isJust)
import Data.Options (Options, options)
import Data.Profunctor (arr)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Sequelize.Class (class Model, modelName, modelCols)
import Sequelize.Types (Alias, Conn, ModelOf, ModelOpts, SEQUELIZE, ModelCols, SyncOpts)
import Sequelize.Types (ColumnOpts, StrMap)
import Type.Proxy (Proxy(..))

foreign import _makeModel
  :: forall a e.
     Fn4
     Conn
     String -- Name
     (StrMap Foreign) -- Schema
     Foreign -- Options for the model
     (Effect (ModelOf a))

makeModelOf
  :: forall a e. Model a
  => Conn
  -> Options (ModelOpts a)
  -> Aff (ModelOf a)
makeModelOf conn opts =
  let opts' = options opts
      -- amap = map (\m -> Tuple $ fst m $ snd m) $ modelCols :: ModelCols a
      columns = arrayToDict ( modelCols :: ModelCols a)
      name = modelName (Proxy :: Proxy a)
   in liftEffect $ runFn4 _makeModel conn name columns opts'


arrayToDict :: forall a. ModelCols a -> StrMap Foreign
arrayToDict model = fromFoldable $ map (rmap options) model

foreign import _sync
  :: forall a b.
     Fn4
     (Maybe b -> Boolean)
     (Partial => Maybe a -> a)
     (ModelOf a)
     SyncOpts
     (Promise Unit)

sync
  :: forall a e. Model a
  => ModelOf a
  -> SyncOpts
  -> Aff Unit
sync mod force = toAff $ runFn4 _sync isJust fromJust mod force

foreign import _drop :: forall a. ModelOf a -> Promise Unit

drop
  :: forall a e. Model a
  => ModelOf a
  -> Aff Unit
drop = toAff <<< _drop

foreign import _hasOne
  :: forall a b alias e.
     Fn3
     (ModelOf a)
     (ModelOf b)
     alias
     (Effect Unit)

-- | HasOne associations are associations where the foreign key for the
-- | one-to-one relation exists on the target model."
hasOne
  :: forall source target e. Model source
  => Model target
  => ModelOf source
  -> ModelOf target
  -> Alias
  -> Aff Unit
hasOne s t a = liftEffect $ runFn3 _hasOne s t a

foreign import _hasMany
  :: forall a b alias e.
     Fn3
     (ModelOf a)
     (ModelOf b)
     alias
     (Effect Unit)

-- | HasOne associations are associations where the foreign key for the
-- | one-to-one relation exists on the target model."
hasMany
  :: forall source target e. Model source
  => Model target
  => ModelOf source
  -> ModelOf target
  -> Alias
  -> Aff Unit
hasMany s t a = liftEffect $ runFn3 _hasMany s t a

foreign import _belongsTo
  :: forall a b alias e.
     Fn3
     (ModelOf a)
     (ModelOf b)
     alias
     (Effect Unit)

-- | #Associations
-- | See: http://docs.sequelizejs.com/manual/tutorial/associations.html

-- | "BelongsTo associations are associations where the foreign key for the
-- | one-to-one relation exists on the source model."
belongsTo
  :: forall source target e. Model source
  => Model target
  => ModelOf target
  -> ModelOf source
  -> Alias
  -> Aff Unit
belongsTo t s a = liftEffect $ runFn3 _belongsTo t s a

foreign import _belongsToMany
  :: forall a b through e.
     Fn3
     (ModelOf a)
     (ModelOf b)
     through
     (Effect Unit)

belongsToMany
  :: forall source target e. Model source
  => Model target
  => ModelOf target
  -> ModelOf source
  -> Alias
  -> Aff Unit
belongsToMany t s a = liftEffect $ runFn3 _belongsToMany t s a
