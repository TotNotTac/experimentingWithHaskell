{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module New where

import Data.Kind (Type)
import Control.Monad (guard)

data Client
  = Bob
  | Alice
  | Charlie
  deriving (Show, Eq)

data ClientMailer (client :: Client) where
  ClientMailer :: String -> ClientMailer client
  deriving (Show)

data Vec :: (f -> Type) -> [f] -> Type where
  VNil :: Vec f '[]
  (:&) :: f x -> Vec f xs -> Vec f (x : xs)

infixr 5 :&

class VecHas (f :: k -> Type) c ml where
  getItem :: ml -> f c

instance VecHas f c (Vec f (c : cs)) where
  getItem (m :& _) = m

instance {-# OVERLAPS #-} (VecHas f c (Vec f css)) => VecHas f c (Vec f (c' : css)) where
  getItem (_ :& ms) = getItem ms

lst :: Vec ClientMailer [Bob, Alice]
lst =
  ClientMailer "Bob"
    :& ClientMailer "Alice"
    :& VNil

getBob :: (VecHas ClientMailer Bob ml) => ml -> ClientMailer Bob
getBob = getItem

test :: ClientMailer Bob
test = getItem lst

-- >>> test
-- ClientMailer "Bob"

data IOClientGetter (client :: Client) where
  IOClientGetter :: {runIOClientGetter :: IO (Maybe Client)} -> IOClientGetter client

getBobIO :: IO (Maybe Client)
getBobIO = do
  str <- getLine
  guard (str == "Bob")
  pure $ Just Bob

ioLst :: Vec IOClientGetter '[Bob]
ioLst = (IOClientGetter getBobIO) :& VNil

testIO :: IO (Maybe Client)
testIO = runIOClientGetter (getItem ioLst :: IOClientGetter Bob)

