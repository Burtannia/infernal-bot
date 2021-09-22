module Infernal.Utils where

import           Calamity     (BotC, Channel (DMChannel'), Member,
                               PermissionsIn' (permissionsIn'), User,
                               manageRoles)
import           Control.Lens ((^.))
import           Data.Flags   (containsAll)
import qualified Df1
import qualified Polysemy     as P

canVerify :: BotC r => Maybe Member -> P.Sem r Bool
canVerify Nothing = return False
canVerify (Just mem) = do
    perms <- permissionsIn' (mem ^. #guildID) mem
    return $ perms `containsAll` manageRoles

channelIsDM :: Channel -> Bool
channelIsDM (DMChannel' _) = True
channelIsDM _              = False

isHuman :: User -> Bool
isHuman user = maybe True not (user ^. #bot)

newtype ShowMsg a = ShowMsg a

instance (Show a) => Df1.ToMessage (ShowMsg a) where
    message (ShowMsg x) = Df1.message $ show x

minsToMicroSeconds :: Int -> Int
minsToMicroSeconds mins = mins * 60 * 1000 * 1000
