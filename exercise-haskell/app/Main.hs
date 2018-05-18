{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import Prelude hiding (id)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Network.Wai.Middleware.Cors
import Web.Scotty

import Control.Concurrent ( newMVar, readMVar, takeMVar, putMVar )
import Control.Monad.Trans.Class (lift)

data Member = Member
  { id :: Int
  , name :: String
  , email :: String
  } deriving (Show, Generic)


insertMember :: Member -> IntMap Member -> (Member, IntMap Member)
insertMember member intMap =
    if IntMap.member (id member) intMap then
        (member, IntMap.insert (id member) member intMap)
    else
      let
        m = Member ((IntMap.size intMap) + 1) (name member) (email member)
      in
        (m, IntMap.insert (id m) m intMap)



newMembersRef :: IO(IntMap Member)
newMembersRef = do
    temp <- newMVar IntMap.empty
    takeMVar temp



instance ToJSON Member


instance FromJSON Member



main :: IO ()
main = do
  membersRef <- newMVar $ IntMap.fromList [ (1, Member 1 "Kurt" "kurt@kurt.com")
                                          , (2, Member 2 "Sonja" "Sonja@sonja.com")
                                          ]
  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat [ "<h1>Hello ", name, " from Scotty!</h1><hr/>"]
    get "/member/count" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.size members
    get "/member" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.elems members
    get "/member/:id" $ do
      idText <- param "id"
      members <- lift $ readMVar membersRef
      let id = (read idText) :: Int
      json $ IntMap.lookup id members
    post "/member" $ do
      member <- jsonData
      oldMembers <- lift $ takeMVar membersRef
      let (updatedMember, newMembers) = insertMember member oldMembers
      lift $ putMVar membersRef newMembers
      json updatedMember

