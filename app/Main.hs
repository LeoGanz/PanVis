module Main where

import Graphics.UI.Gtk hiding (get)
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Lifted (fork, threadDelay, killThread, ThreadId)
import Control.Monad.Trans.State.Lazy
import qualified Data.Vector as Vec
import qualified Data.ByteString.Char8 as C
import System.Directory
import Data.Functor ( (<&>) )
import Data.Time.Calendar
import Data.Maybe

import GUIData
import GenerateSVG
import qualified Lib


data Env = Env  {
                      mainMenu      :: MainMenu
                    , animThread    :: MVar ThreadId
                    , animFrameId   :: MVar Int
                    , frames        :: MVar (Vec.Vector (String, Pixbuf))
                    , paused        :: MVar Bool
                }


main :: IO()
main = do
    initGUI
    mainMenu <- loadMainMenu
    widgetShowAll $ main_window mainMenu

    animThread  <- newEmptyMVar
    animFrameId <- newMVar 0
    frames      <- newMVar Vec.empty
    paused      <- newMVar True
    let env = Env mainMenu animThread animFrameId frames paused

    dayToLoad <- newMVar =<< Lib.firstDateOfPandemic
    loading <- newEmptyMVar
    forkIO $ do
        Lib.fetchAndSaveData
        putMVar loading True
        day <- readMVar dayToLoad
        when (isNothing day) $ void $ swapMVar dayToLoad =<< Lib.firstDateOfPandemic
        day <- readMVar dayToLoad
        evalStateT (loadFrames day) env
        void $ takeMVar loading
    forkIO $ do
        putMVar loading True
        day <- readMVar dayToLoad
        evalStateT (loadFrames day) env
        void $ takeMVar loading

    on (play_button mainMenu) buttonActivated $ evalStateT startStopAnimation env
    on (time_scale mainMenu)  adjustBounds    $ \pos -> evalStateT (selectFrame pos) env
    on (main_window mainMenu) objectDestroy     mainQuit
    mainGUI


-- abbreviations for readability
getMenu = get <&> mainMenu
takeMVarE var       = get >>= liftIO . takeMVar . var
putMVarE  var val   = get >>= liftIO . (\env -> putMVar (var env) val)
readMVarE var       = get >>= liftIO . readMVar . var
swapMVarE var val   = get >>= liftIO . (\env -> swapMVar (var env) val)
modifyMVarE var f   = do {val <- takeMVarE var; putMVarE var (f val)}


loadFrames :: Maybe Day -> StateT Env IO ()
loadFrames maybeDay = do
    forM_ maybeDay $ \day -> do
        result <- liftIO $ Lib.dataInFrontendFormat day
        forM_ result $ \valsForOneDay -> do
            loadSVG valsForOneDay
            loadFrames $ Just (addDays 1 day)


loadSVG :: (String, [(C.ByteString, Double)]) -> StateT Env IO ()
loadSVG (date, districtValList) = do
    let filePath = "app/images/buffer/" ++ date ++ ".svg"
    fileExists <- liftIO $ doesFileExist filePath
    unless fileExists $
        liftIO $ writeSVGFile districtValList filePath
    pixbuf <- liftIO $ pixbufNewFromFile filePath
    void $ modifyMVarE frames (\v -> Vec.snoc v (date, pixbuf))
    v <- readMVarE frames
    menu <- getMenu
    liftIO $ postGUIAsync $ rangeSetRange (time_scale menu) (0::Double) (fromIntegral (Vec.length v))


startStopAnimation :: StateT Env IO ()
startStopAnimation = do
    isPaused <- takeMVarE paused
    if isPaused then do
        putMVarE paused False
        fork doAnimation >>= putMVarE animThread
    else do
        takeMVarE animThread >>= killThread
        putMVarE paused True


doAnimation :: StateT Env IO ()
doAnimation = do
    isPaused <- takeMVarE paused
    if not isPaused then do
        v <- readMVarE frames
        id <- takeMVarE animFrameId
        if id < Vec.length v then do
            putMVarE paused False
            putMVarE animFrameId (id + 1)
            let (date, pixbuf) = v Vec.! id
            display id date pixbuf
            threadDelay 100000
            doAnimation
        else do
            putMVarE animFrameId 0
            t <- takeMVarE animThread
            putMVarE paused True
            killThread t
    else
        putMVarE paused True


selectFrame :: Double -> StateT Env IO ()
selectFrame pos = do
    isPaused <- takeMVarE paused
    unless isPaused (takeMVarE animThread >>= killThread)
    putMVarE paused True
    menu <- getMenu
    let id = round pos
    v <- readMVarE frames
    when (id >= 0 && id < Vec.length v) $ do
        swapMVarE animFrameId id
        let (date, pixbuf) = v Vec.! id
        display id date pixbuf


display :: Int -> String -> Pixbuf -> StateT Env IO ()
display sliderPos date pixbuf = do
    menu <- getMenu
    liftIO $ postGUIAsync $ do
        imageSetFromPixbuf (map_image menu) pixbuf
        set (time_scale menu) [rangeValue := fromIntegral sliderPos]
        set (time_label menu) [labelLabel := date]