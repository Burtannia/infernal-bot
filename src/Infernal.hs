module Infernal where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (useFullContext)
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson                as Aeson
import           Data.Default
import           Data.Flags                ((.+.))
import           Data.Generics.Labels      ()
import           Data.Maybe
import           Data.Text.Lazy            (Text)
import qualified Df1
import qualified Di
import           DiPolysemy
import qualified DiPolysemy as DI (info, error)
import qualified Polysemy                  as P
import System.Exit
import           Options.Generic hiding (Text)
import Data.IntMap.Strict

import Infernal.Challenge
import Infernal.Config

newtype ShowMsg a = ShowMsg a

instance (Show a) => Df1.ToMessage (ShowMsg a) where
    message (ShowMsg x) = Df1.message $ show x

runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useConstantPrefix (cfg ^. #commandPrefix . lazy)
    . useFullContext
    . runBotIO
        (BotToken (cfg ^. #botToken . lazy))
        (defaultIntents .+. intentGuildMembers)
    $ do 
        info @Text "Bot starting up!"
        _ <- react @'GuildMemberAddEvt $ \mem -> do
            info @Text "User joined"
            void . invoke $ AddGuildMemberRole (mem ^. #guildID) (mem ^. #id) (cfg ^. #verifiedRole)
            edms <- invoke $ CreateDM mem
            case edms of
                Left err -> DI.error $ ShowMsg err
                Right dms -> do
                    info @Text "Challenging user"
                    mguild <- upgrade (mem ^. #guildID)
                    let guildName = fromMaybe "the guild" $ fmap (^. #name) mguild
                    challenge <- P.embed $
                        mkChallenge (mem ^. #id) (mem ^. #guildID) (cfg ^. #challengeAttempts)
                    void $ tell dms $ showChallenge guildName challenge

        addCommands $ do
            helpCommand

main :: IO ()
main = do
    opts <- unwrapRecord @_ @CLIOptions "InfernalBot"
    path <- case opts ^. #config of
        Just path -> pure path
        Nothing -> die "Error: no config specified"
    cfg <- Aeson.eitherDecodeFileStrict path >>= either die pure
    runBotWith cfg