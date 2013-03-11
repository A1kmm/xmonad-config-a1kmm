{-# LANGUAGE TypeSynonymInstances,MultiParamTypeClasses,DeriveDataTypeable,FlexibleContexts #-}
import XMonad
import XMonad.Config.Gnome
import XMonad.Config.Desktop
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ThreeColumns
import Data.Monoid
import Control.Monad
import System.Exit
import qualified Data.Map as M 
import qualified XMonad.StackSet as W
import XMonad.Layout.BoringWindows as BW
import Data.List

myModMask       = mod4Mask
 
myLayout        = boringAuto . minimize $ (ThreeColMid 1 (3/100) (1/2) ||| Tall 1 (3/100) (1/2)) ||| Full

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  [
    ((modm, xK_slash), withFocused (\f -> sendMessage (MinimizeWin f))),
    ((modm .|. shiftMask, xK_slash), sendMessage RestoreNextMinimizedWin)
  ]
allKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))


data Minimize a = Minimize [Window] deriving ( Read, Show )
minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize []

data MinimizeMsg = MinimizeWin Window
                    | RestoreMinimizedWin Window
                    | RestoreNextMinimizedWin
                    | ToggleMinimizedWin Window
                    deriving (Typeable, Eq)
instance Message MinimizeMsg

instance LayoutModifier Minimize Window where
    modifierDescription (Minimize _) = "Minimize"

    modifyLayout (Minimize minimized) wksp rect = do
        let stack = W.stack wksp
            filtStack = stack >>=W.filter (\w -> not (w `elem` minimized))
        runLayout (wksp {W.stack = filtStack}) rect

    handleMess (Minimize minimized) m = case fromMessage m of
        Just (MinimizeWin w)
          | not (w `elem` minimized) -> do
                BW.focusDown
                return $ Just $ Minimize (w:minimized)
          | otherwise               -> return Nothing
        Just (RestoreMinimizedWin w) ->
            return $ Just $ Minimize (minimized \\ [w])
        Just (RestoreNextMinimizedWin)
          | not (null minimized)    -> do
                focus (head minimized)
                return $ Just $ Minimize (tail minimized)
          | otherwise               -> return Nothing
        Just (ToggleMinimizedWin w)
          | (w `elem` minimized)    ->
                return . Just . Minimize $ delete w minimized
          | otherwise               -> do
                BW.focusDown
                return $ Just $ Minimize (w:minimized)
        _ -> return Nothing

toggleMinimizedEventHook (ClientMessageEvent {ev_window = w,
                                                ev_message_type = mt}) = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    a_cs <- getAtom "WM_CHANGE_STATE"
    when (mt == a_aw || mt == a_cs) $ do
        sendMessage (ToggleMinimizedWin w)
    return (All True)

myHandleEventHook = toggleMinimizedEventHook `mappend` handleEventHook gnomeConfig

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad (gnomeConfig { modMask = myModMask, 
                             layoutHook = desktopLayoutModifiers $ myLayout, 
                             keys = allKeys, 
                             handleEventHook = myHandleEventHook })
