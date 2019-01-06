module Process
    ( runProcess
    , runProcessNonStrict
    )
where

import           System.IO
import           System.Exit
import           System.Log.Logger
import           System.Process                 ( readCreateProcessWithExitCode
                                                , shell
                                                )
logger = "BasicLogger"

runProcess shellCommand std_in = do
    (_, output, _) <- _runProcess shellCommand std_in
        >>= exitIfFailed shellCommand
    return output


runProcessNonStrict = _runProcess


_runProcess shellCommand std_in =
    readCreateProcessWithExitCode (shell shellCommand) std_in


exitIfFailed shellCommand commandResult = case commandResult of
    (ExitFailure _, _, _) -> exitWithError shellCommand commandResult
    _                     -> return commandResult


exitWithError shellCommand (exitCode, output, error) = do
    criticalM
        logger
        ("\n-----------------------------\n\
      \An error has occurred! Debug info is provided below\n\
      \COMMAND: \""
        <> shellCommand
        <> "\"\nSTD_OUT:\n"
        <> output
        <> "\nERROR:\n"
        <> error
        )
    exitFailure
