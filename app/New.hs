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

data Client
  = Bob
  | Alice
  deriving (Show, Eq)

data ClientMailer (client :: Client) where
  ClientMailer :: String -> ClientMailer client
  deriving (Show)

bobMailer :: ClientMailer 'Bob
bobMailer = ClientMailer "Bob"

aliceMailer :: ClientMailer 'Alice
aliceMailer = ClientMailer "Alice"

data MailerList :: [Client] -> Type where
  MZero :: MailerList '[]
  (:&) :: ClientMailer c -> MailerList cs -> MailerList (c : cs)

infixr 5 :&

class HasClient (c :: Client) ml where
  getMailer :: ml -> ClientMailer c

instance HasClient c (MailerList (c : cs)) where
  getMailer (m :& _) = m

instance {-# OVERLAPS #-} (HasClient c (MailerList css)) => HasClient c (MailerList (c' : css)) where
  getMailer (_ :& ms) = getMailer ms

lst :: MailerList [Bob, Alice]
lst = bobMailer :& aliceMailer :& MZero

getBob :: (HasClient Bob ml) => ml -> ClientMailer 'Bob
getBob ml = getMailer ml

test :: ClientMailer Bob
test = getBob lst

-- >>> test
-- ClientMailer "Bob"

getAlice :: (HasClient Alice ml) => ml -> ClientMailer 'Alice
getAlice ml = getMailer ml

test2 :: ClientMailer Alice
test2 = getAlice lst

-- >>> test2
-- ClientMailer "Alice"
