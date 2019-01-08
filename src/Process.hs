module Process
    ( runProcess
    , runProcess'
    , runProcessNonStrict
    )
where

import           System.IO
import           System.Exit
import           Config
import           Logger
import           Control.Monad.Reader
import           System.Process                 ( readCreateProcessWithExitCode
                                                , shell
                                                )

runProcess' shellCommand = do
    runProcess shellCommand
    return ()

runProcess shellCommand = do
    (_, output, _) <- exitIfFailed shellCommand =<< _runProcess shellCommand []
    return output


runProcessNonStrict :: String -> ReaderT Config IO (ExitCode, String, String)
runProcessNonStrict shellCommand = _runProcess shellCommand []


_runProcess shellCommand std_in =
    liftIO $ readCreateProcessWithExitCode (shell shellCommand) std_in


exitIfFailed shellCommand commandResult = case commandResult of
    (ExitFailure _, _, _) -> exitWithError shellCommand commandResult
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
