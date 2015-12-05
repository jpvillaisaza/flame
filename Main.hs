{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
  where

-- base
import qualified Control.Monad as Monad
import qualified Data.Bool as Bool
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

-- directory
import qualified System.Directory as Directory

-- filepath
import qualified System.FilePath as FilePath

-- process
import qualified System.Process as Process

lame :: FilePath
lame = "lame"

doesLameExist :: IO Bool
doesLameExist = do
  maybeLame <- Directory.findExecutable lame
  return (Maybe.isJust maybeLame)

data Flame =
  Flame
    { flameHelp :: Bool
    , flameInput :: FilePath
    , flamePreset :: Maybe Preset
    , flameOutput :: FilePath
    , flameVerbose :: Bool
    , flameVersion :: Bool
    }
  deriving Show

defaultFlame :: Flame
defaultFlame =
  Flame
    { flameHelp = False
    , flameInput = ""
    , flamePreset = Just defaultPreset
    , flameOutput = ""
    , flameVerbose = False
    , flameVersion = False
    }

flameDescription :: [GetOpt.OptDescr (Flame -> Flame)]
flameDescription =
  [ GetOpt.Option
      ['h']
      ["help"]
      (GetOpt.NoArg (\flame -> flame { flameHelp = True }))
      ""

  , GetOpt.Option
      ['i']
      ["input"]
      (GetOpt.ReqArg (\input flame -> flame { flameInput = input }) "S")
      "sd"

  , GetOpt.Option
      ['o']
      ["output"]
      (GetOpt.ReqArg (\output flame -> flame { flameOutput = output }) "")
      "sd"

  , GetOpt.Option
      ['v']
      ["verbose"]
      (GetOpt.NoArg (\flame -> flame { flameVerbose = True }))
      "sd"

  , GetOpt.Option
      []
      ["version"]
      (GetOpt.NoArg (\flame -> flame { flameVersion = True }))
      "version"

  ]

parseFlame :: [String] -> Either String Flame
parseFlame args =
  case GetOpt.getOpt GetOpt.Permute flameDescription args of
    (o,[],[]) -> Right (foldl (flip id) defaultFlame o)
    (_,n,[]) -> Left (concat n)
    (_,_,e) -> Left (concat e)

sd errs =
  ioError (userError (errs ++ GetOpt.usageInfo header flameDescription))
  where
    header = "Usage: ic [OPTION...] files..."

data Preset
  = Preset
  deriving Show

defaultPreset :: Preset
defaultPreset =
  Preset

data Album =
  Album
    { albumArtist :: String
    , albumTitle :: String
    , albumTracks :: String
    , albumYear :: String
    }
  deriving Show

data Song =
  Song
    { songAlbum :: String
    , songArtist :: String
    , songTitle :: String
    , songTrack :: String
    , songYear :: String
    , songWav :: FilePath
    , songMp3 :: FilePath
    }
  deriving Show

compressAlbumSong
  :: Flame
  -> Album
  -> Song
  -> IO ()
compressAlbumSong Flame{..} Album{..} Song{..} = do
  putStrLn ("compressing " ++ songTitle)

  let options =
        [ "--preset", "extreme"
        , "--tt", songTitle
        , "--ta", songArtist
        , "--tl", songAlbum
        , "--ty", songYear
        , "--tn", songTrack ++ "/" ++ albumTracks
        , songWav
        , songMp3
        ]

  (exitCode, output, errors) <-
    Process.readCreateProcessWithExitCode (Process.proc "lame" options) ""

  case exitCode of
    Exit.ExitFailure _ ->
      Monad.void (putStrLn errors)
    Exit.ExitSuccess ->
      return ()

compressAlbumSongs
  :: Flame
  -> Album
  -> [Song]
  -> IO ()
compressAlbumSongs flame album =
  Monad.mapM_ (compressAlbumSong flame album)

compressAction :: Flame -> IO ()
compressAction _ = do
  putStrLn "compressing"
  Exit.exitSuccess

helpAction :: IO ()
helpAction = do
  putStrLn "help"
  Exit.exitSuccess

versionAction :: IO ()
versionAction = do
  putStrLn "version"
  Exit.exitSuccess

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  IO.hSetBuffering IO.stdout IO.NoBuffering

  progName <- Environment.getProgName
  putStrLn progName

  args <- Environment.getArgs

  flame@Flame{..} <- either Exit.die return (parseFlame args)

  Monad.when flameHelp helpAction

  Monad.when flameVersion versionAction

  currentDirectory <- Directory.getCurrentDirectory
  inputDirectory <- readWithDefault "input directory" currentDirectory
  inputFilePaths <- Directory.getDirectoryContents inputDirectory
  let inputWavs = List.sort (filter ((==) ".wav" . FilePath.takeExtension) inputFilePaths)
  Monad.when (null inputWavs) Exit.exitFailure

  let albumTracks = show (length inputWavs)

  outputDirectory <- readWithDefault "output directory" inputDirectory

  albumTitle <- readWithDefault "album title" unknown
  albumArtist <- readWithDefault "album artist" unknown
  albumYear <- readWithDefault "album year" unknown
  albumTracks <- readWithDefault "album tracks" albumTracks

  let
    album =
      Album
        { albumArtist = albumArtist
        , albumTitle = albumTitle
        , albumTracks = albumTracks
        , albumYear = albumYear
        }

  songs <-
    Monad.forM inputWavs $ \inputWav -> do
      putStrLn inputWav
      songTitle <- readWithDefault "song title" unknown
      songArtist <- readWithDefault "song artist" albumArtist
      songYear <- readWithDefault "song year" albumYear
      let defTrack "" = unknown
          defTrack n = show (read n :: Int)
      songTrack <- readWithDefault "song track" (defTrack (filter Char.isDigit inputWav))
      outF <- readWithDefault "song file" (FilePath.replaceExtension inputWav "mp3")
      let
        song =
          Song
            { songAlbum = albumTitle
            , songArtist = songArtist
            , songTitle = songTitle
            , songTrack = songTrack
            , songYear = songYear
            , songWav = FilePath.combine inputDirectory inputWav
            , songMp3 = FilePath.combine outputDirectory outF
            }
      return song

  lameExists <- doesLameExist
  Monad.unless lameExists Exit.exitFailure

  compressAlbumSongs flame album songs

unknown :: String
unknown = "Unknown"

readWithDefault :: String -> String -> IO String
readWithDefault m d = do
  putStr (m ++ " (default: " ++ d ++ "): ")
  line <- getLine
  return (Bool.bool line d (null line))

