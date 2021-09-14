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
import           Data.Text.Lazy            (Text)
import qualified Di
import           DiPolysemy
import qualified Polysemy                  as P
import System.Exit
import           Options.Generic hiding (Text)

import Infernal.Config

runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useFullContext
    . useConstantPrefix "!"
    . runBotIO
        (BotToken (cfg ^. #botToken . lazy))
        (defaultIntents .+. intentGuildMembers)
    $ do
        info @Text "Bot starting up!"
        _ <- react @'GuildMemberAddEvt $ \usr -> do
            info @Text "User joined"
            void . invoke $ AddGuildMemberRole (usr ^. #guildID) (usr ^. #id) (cfg ^. #verifiedRole)
            edms <- invoke $ CreateDM usr
            case edms of
                Right dms -> do
                    info @Text "DMing"
                    void $ tell @Text dms "What's 9 + 10?"
                Left err -> Prelude.error $ show err

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