module Options (
  Options,
  targetPath,
  multiplier,
  optVerbose,
  defaultOptions,
  parseArgs
) where
import Control.Monad         (foldM)
import System.Console.GetOpt
import System.Environment    (getArgs, getProgName)
import Text.Read
import Data.Decimal

{-|
  A data record of the options available
-}
data Options = Options { targetPath :: FilePath,
                         multiplier :: Decimal,
                         optVerbose :: Bool
                       } deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options ".\\*" 2 False
{-|
  List of option descriptors. Used when parsing command-line args.
  OptDescrs are described using:
    Option <list of short option characters (-h)> <list of long option strings (--height=...)> <ArgDescr> <explanation Text>
-}
options :: [OptDescr (Options -> Either String Options)]
options = [ Option ['t'] ["target"]
              (ReqArg
                (\g opts -> Right opts {targetPath = g})
                "FILE PATH"
              )
              "the target path to read and replace",
            Option ['m'] ["multiplier"]
              (ReqArg
                (\g opts -> 
                  (case (readMaybe g) of Just a  -> Right opts {multiplier = a}
                                         Nothing -> Left "Multiplier must be a valid number"
                  )
                )
                "MULTIPLIER"
              )
              "what to multiply the values by",
            Option ['v'] ["verbose"]
              (NoArg
                (\opts -> Right opts {optVerbose = True})
              )
              "enable verbose output"
          ]

{-|
  Reads in the command-line arguments, parses them, and returns an Options data record.
  If there was an error in parsing the input, returns the error message attached to the OptDescr or a general error message.
-}
parseArgs :: Options -> IO Options
parseArgs defaults = do args <- getArgs
                        progName <- getProgName
                        let header = "Usage: " ++ progName ++ " [OPTION...]"
                        let helpMessage = usageInfo header options
                        case getOpt RequireOrder options args of
                          (opts, [], []) -> case foldM (flip id) defaults opts of
                                              Right checkedOpts -> return checkedOpts
                                              Left errorMessage -> ioError (userError (helpMessage ++ ": \n" ++ errorMessage))
                          (_, _, errs)   -> ioError (userError (helpMessage ++ ": \n" ++ concat errs))
