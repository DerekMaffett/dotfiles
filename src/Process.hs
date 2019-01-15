module Process
    ( runProcess
    , runProcess'
    , runProcessNonStrict
    , runProcessWithDir
    )
where

import           System.IO
import           System.Exit
import           Config
import           Logger
import           Control.Monad.Reader
import qualified System.Directory              as Dir
import           System.Process                 ( readCreateProcessWithExitCode
                                                , shell
                                                )

runProcessWithDir dir shellCommand =
    runProcess ("cd " <> dir <> " && " <> shellCommand)

runProcess' shellCommand = do
    runProcess shellCommand
    return ()

runProcess shellCommand = do
    (_, output, _) <- exitIfFailed shellCommand =<< _runProcess shellCommand []
    return output


runProcessNonStrict :: String -> ReaderT Config IO (ExitCode, String, String)
runProcessNonStrict shellCommand = _runProcess shellCommand []


_runProcess shellCommand std_in = do
    fullCommand               <- makeCommand shellCommand
    (exitCode, output, error) <- liftIO
        $ readCreateProcessWithExitCode (shell fullCommand) std_in
    logDebug $ fullCommand <> "\n" <> output
    return (exitCode, output, error)


makeCommand :: String -> ReaderT Config IO String
makeCommand shellCommand = do
    pathAddition <-
        (\binDir -> "PATH=" <> binDir <> ":$PATH && ") <$> binDir <$> ask
    return $ pathAddition <> shellCommand


exitIfFailed shellCommand commandResult = do
    fullCommand <- makeCommand shellCommand
    case commandResult of
        (ExitFailure _, _, _) -> exitWithError fullCommand commandResult
        _                     -> return commandResult


exitWithError shellCommand (exitCode, output, error) = do
    logError
        ("\n-----------------------------\n\
      \An error has occurred! Debug info is provided below\n\
      \COMMAND: \""
        <> shellCommand
        <> "\"\nSTD_OUT:\n"
        <> output
        <> "\nERROR:\n"
        <> error
        )
    liftIO exitFailure
