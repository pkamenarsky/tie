{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Data.Maybe
import Data.Proxy

import Tie

data Object m

data ID
  = ID (TypedRef Subject String) (Proxy Subject)
  | ID2 Int (Proxy Object)
  deriving (Eq, Ord, Show)

data Subject m = Subject
  { uName :: Key m String
  , uParent :: Maybe (Ref m String Subject)
  }

deriving instance Show (Subject Unresolved)
deriving instance Show (Subject Resolved)
deriving instance Eq (Subject Resolved)

data Root m = Root
  { uPeople :: [Subject m]
  , uRefs :: [Ref m String Subject]
  }

deriving instance Show (Root Unresolved)
deriving instance Show (Root Resolved)
deriving instance Eq (Root Resolved)

evalPerson :: Subject Unresolved -> Dep ID (Subject Resolved)
evalPerson pU = put (uName pU) ID $ \_ rName -> do
  rParent <- case uParent pU of
    Nothing -> pure Nothing
    -- Just p -> Just <$> pullCasc (ID p)
    Just p -> pull (ID p)
  pure $ Subject { uName = rName, uParent = rParent }

evalRoot :: Root Unresolved -> Dep ID (Root Resolved)
evalRoot rU = do
  rPeople <- traverseN evalPerson (uPeople rU)
  rRefs <- catMaybes <$> traverse (pull . ID) (uRefs rU)
  pure $ Root { uPeople = rPeople, uRefs = rRefs }

rootU :: Root Unresolved
rootU = Root
  { uPeople =
      [ Subject (RemoveRec "odin") (Just $ mkRef "freya")
      , Subject (Id "thor") (Just $ mkRef "freya")
      , Subject (Id "freya") (Just $ mkRef "modi")
      , Subject (Id "modi") (Just $ mkRef "gimli")
      , Subject (RemoveRec "gimli") Nothing
      ]
  , uRefs = [ mkRef "gimli" ]
  }

rootR :: Either (ResolveError ID) (Root Resolved)
rootR = tie (evalRoot rootU)

main :: IO ()
main
  | rootR == rootExp = putStrLn "OK"
  | otherwise = putStrLn "FAIL"
  where
    rootExp = Right (Root {uPeople = [Subject {uName = "thor", uParent = Just (Subject {uName = "freya", uParent = Just (Subject {uName = "modi", uParent = Nothing})})},Subject {uName = "freya", uParent = Just (Subject {uName = "modi", uParent = Nothing})},Subject {uName = "modi", uParent = Nothing}], uRefs = []})
  

