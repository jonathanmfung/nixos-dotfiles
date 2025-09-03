-- ghc screenshot.hs; ./screenshot
import System.Directory
  ( createDirectoryIfMissing,
  )
import System.Exit
  ( ExitCode (..),
  )
import System.FilePath
  ( (<.>),
    (</>),
  )
import System.Process
  ( callProcess,
    readProcess,
    readProcessWithExitCode,
  )

-- TODO: might want to be non-temp, like ~/screenshots
saveDir :: FilePath
saveDir = "/tmp" </> "screenshots"

savePath :: String -> FilePath
savePath n = saveDir </> n <.> "png"

cleanNewline :: String -> String
cleanNewline = filter (/= '\n')

date :: IO String
date = readProcess "date" [dateFormat] ""
  where
    dateFormat = "+%y%m%d_%H%M%S"

slurp :: IO (ExitCode, String, String)
slurp = readProcessWithExitCode "slurp" [] ""

grim :: String -> String -> IO (ExitCode, String, String)
grim geom name = readProcessWithExitCode "grim" ["-g", geom, name] ""

notify :: String -> String -> IO ()
notify title sub = callProcess "notify-send" [title, sub]

emacsclient :: String -> IO ()
emacsclient file = callProcess "emacsclient" ["-c", file]

main :: IO ()
main = do
  createDirectoryIfMissing True saveDir
  d <- cleanNewline <$> date
  let p = savePath d
  (sexit, sout, serr) <- slurp
  case sexit of
    ExitFailure n -> notify "Failed to Screenshot" "Probably slurped a 0-size reigon"
    ExitSuccess -> do
      (gexit, gout, gerr) <- grim (cleanNewline sout) p
      case gexit of
        ExitFailure n -> notify "Failed to Screenshot" ("Exit code " ++ show n)
        ExitSuccess -> notify "Saved Screenshot" ("as " ++ p) *> emacsclient p
