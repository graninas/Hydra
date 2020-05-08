{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hydra.Core.SqlDB.Language where

import           Hydra.Prelude
import qualified Database.Beam as B
import qualified Hydra.Core.Domain as D


data SqlDBAction beM a where
  SqlDBAction :: beM a -> SqlDBAction beM a

select''
  :: (D.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBAction beM [a]
select'' a = SqlDBAction (D.rtSelectReturningList a)

selectOne''
  :: (D.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBAction beM (Maybe a)
selectOne'' a = SqlDBAction (D.rtSelectReturningOne a)

insert''
  :: D.BeamRuntime be beM
  => B.SqlInsert be table
  -> SqlDBAction beM ()
insert'' a = SqlDBAction (D.rtInsert a)

update''
  :: D.BeamRuntime be beM
  => B.SqlUpdate be table
  -> SqlDBAction beM ()
update'' a = SqlDBAction (D.rtUpdate a)

delete''
  :: D.BeamRuntime be beM
  => B.SqlDelete be table
  -> SqlDBAction beM ()
delete'' a = SqlDBAction (D.rtDelete a)


getBeamRunner'
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => D.SqlConn beM
  -> SqlDBAction beM a
  -> ((String -> IO ()) -> IO a)
getBeamRunner' conn (SqlDBAction beM) = D.getBeamDebugRunner conn beM



data SqlDBMethodF beM next where
  SqlDBMethod :: (D.SqlConn beM -> (String -> IO ()) -> IO a) -> (a -> next) -> SqlDBMethodF beM next

instance Functor (SqlDBMethodF beM) where
  fmap f (SqlDBMethod runner next) = SqlDBMethod runner (f . next)

type SqlDBL beM = F (SqlDBMethodF beM)

sqlDBMethod
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => SqlDBAction beM a
  -> SqlDBL beM a
sqlDBMethod act = do
  let runner = \conn -> getBeamRunner' conn act
  liftFC $ SqlDBMethod runner id



select'
  :: (D.BeamRunner beM, D.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBL beM [a]
select' = sqlDBMethod . select''

selectOne'
  :: (D.BeamRunner beM, D.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBL beM (Maybe a)
selectOne' = sqlDBMethod . selectOne''

insert'
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDBL beM ()
insert' = sqlDBMethod . insert''

update'
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDBL beM ()
update' = sqlDBMethod . update''

delete'
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDBL beM ()
delete' = sqlDBMethod . delete''



-- Convenience interface

findRows
  :: (D.BeamRunner beM, D.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBL beM [a]
findRows = select'

findRow
  :: (D.BeamRunner beM, D.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBL beM (Maybe a)
findRow = selectOne'

insertRows
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDBL beM ()
insertRows = insert'

updateRows
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDBL beM ()
updateRows = update'

deleteRows
  :: (D.BeamRunner beM, D.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDBL beM ()
deleteRows = delete'
