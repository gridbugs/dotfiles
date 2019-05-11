{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.NoBorders
import XMonad.Layout.IndependentScreens(countScreens)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import System.IO
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe
import XMonad.Layout.Fullscreen

main :: IO ()
main = do
    numScreens <- countScreens
    xmobars <- mapM xmobarScreen [0 .. (numScreens - 1)]
    xmonad $ myConfig xmobars

xmobarScreen :: Int -> IO Handle
xmobarScreen = spawnPipe . ("xmobar -x " ++) . show

myBorderWidth = 2

myTerminal = "terminator"
myWebBrowser = "firefox"

myWorkspaceNames = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceNamesSet = Set.fromList myWorkspaceNames

myXmobarHiddenNoWindowsFilter ws = if Set.member ws myWorkspaceNamesSet then ws else ""

myNormalBorderColour    = "#000000"
myFocusedBorderColour   = "#bb0000"
myNavBorderColour       = "#660000"
myXmobarTitle           = xmobarColor "#aaaaaa" "" . shorten 50
myXmobarCurrent         = xmobarColor "#00ff00" ""
myXmobarVisible         = xmobarColor "#ffff00" ""
myXmobarUrgent          = xmobarColor "#ffffff" "#ff0000"
myXmobarHidden          = xmobarColor "#aaaaaa" ""
myXmobarHiddenNoWindows = xmobarColor "#666666" "" . myXmobarHiddenNoWindowsFilter
myXmobarLayout          = xmobarColor "#666666" ""
myXmobarSep             = " | "

hPutStrLnMulti :: [Handle] -> String -> IO ()
hPutStrLnMulti handles string = mapM_ (`hPutStrLn` string) handles

myConfig xmobars =
    let c = def {
          terminal        = myTerminal
        , modMask         = mod1Mask
        , manageHook      = manageDocks <+> myManageHook <+> manageHook def
        , layoutHook      = avoidStruts myLayout
        , handleEventHook = ewmhDesktopsEventHook <+> docksEventHook <+> handleEventHook def
        , startupHook     = docksStartupHook <+> startupHook def
        , logHook         = dynamicLogWithPP xmobarPP
            { ppOutput    = hPutStrLnMulti xmobars
            , ppTitle     = myXmobarTitle
            , ppCurrent   = myXmobarCurrent
            , ppHidden    = myXmobarHidden
            , ppHiddenNoWindows = myXmobarHiddenNoWindows
            , ppLayout    = myXmobarLayout
            , ppVisible   = myXmobarVisible
            , ppUrgent    = myXmobarUrgent
            , ppSep       = myXmobarSep
            } >> ewmhDesktopsLogHook
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        , borderWidth        = myBorderWidth
        , XMonad.workspaces  = myWorkspaceNames
        , keys            = const M.empty
        }
    in
    let c1 = additionalKeysP c $ myKeys c in
    additionalKeys c1 $ myKeysExtra c1

myManageHook = composeAll [
      isFullscreen --> doFullFloat
    , fullscreenManageHook
    ]

myKeys :: XConfig a -> [(String, X ())]
myKeys c =
    [ ("M-<Return>"     , spawn myWebBrowser)
    , ("M-S-<Return>"   , spawn $ terminal c)
    , ("M-p"            , spawn "rofi -show run")
    , ("M-b"            , sendMessage ToggleStruts)
    , ("M-q"            , spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    , ("M-S-q"          , io exitSuccess)
    , ("M-S-c"          , kill)
    , ("M-h"            , sendMessage $ WN.Go L)
    , ("M-s"            , sendMessage $ WN.Go R)
    , ("M-n"            , sendMessage $ WN.Go U)
    , ("M-t"            , sendMessage $ WN.Go D)
    , ("M-C-h"          , sendMessage $ ExpandTowards L)
    , ("M-C-s"          , sendMessage $ ShrinkFrom L)
    , ("M-C-n"          , sendMessage $ ExpandTowards U)
    , ("M-C-t"          , sendMessage $ ShrinkFrom U)
    , ("M-S-h"          , sendMessage $ ShrinkFrom R)
    , ("M-S-s"          , sendMessage $ ExpandTowards R)
    , ("M-S-n"          , sendMessage $ ShrinkFrom D)
    , ("M-S-t"          , sendMessage $ ExpandTowards D)
    , ("M-r"            , sendMessage Rotate)
    , ("M-c"            , sendMessage Swap)
    , ("M-S-C-b"        , sendMessage FocusParent)
    , ("M-C-b"          , sendMessage SelectNode)
    , ("M-S-b"          , sendMessage MoveNode)
    , ("M-<Tab>"        , focusDown)
    , ("M-S-<Tab>"      , focusUp)
    , ("M-<Space>"      , sendMessage $ Toggle TABBED)
    , ("M-x"            , sendMessage NextLayout)
    , ("M-S-<Space>"    , O.windows S.swapMaster)
    , ("M-w"            , sendMessage $ IncMasterN 1)
    , ("M-v"            , sendMessage $ IncMasterN (-1))
    , ("M-a"            , spawn "setxkbmap en_US; xmodmap $HOME/.Xmodmap")
    , ("M-m"            , spawn "setxkbmap dvorak; xmodmap $HOME/.Xmodmap")
    , ("M-y"            , withFocused $ windows . S.sink)
    , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 2%+")
    , ("<XF86AudioMute>", spawn "amixer sset Master toggle; amixer sset Headphone unmute; amixer sset Speaker unmute")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 4")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 4")
    , ("M-<XF86MonBrightnessDown>", spawn "kb-light.py --down 1")
    , ("M-<XF86MonBrightnessUp>", spawn "kb-light.py --up 1")
    ]
    ++
    [ ("M-" ++ s ++ show k, O.windows $ f w)
        | (w, k) <- zip (workspaces c) [1..]
        , (s, f) <- zip ["", "S-"] [S.greedyView, S.shift]
    ]
    ++
    [ ("M-" ++ s ++ k, screenWorkspace sc >>= flip whenJust (O.windows . f))
        | (sc, k) <- zip [1, 0, 0] ["'", ",", "."]
        , (s, f) <- zip ["", "S-"] [S.view, S.shift]
    ]

myKeysExtra c =
    [ ((modMask c, xK_slash),      spawn "xeyes")
    ]

myTabConfig = defaultTheme {
    fontName = "xft:xos4 Terminus:pixelsize=14:antialias=true:hinting=true",
    decoHeight = 20,
    activeColor = "#FFFFFF",
    inactiveColor = "#333333",
    activeTextColor = "#000000",
    inactiveTextColor = "#bbbbbb"
  }

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
    transform _ x k = k (noBorders (tabbed shrinkText myTabConfig)) (const x)

myLayout =
    -- for changes to navBorderColour to take effect, one must
    -- toggle the following line
    WN.configurableNavigation (WN.navigateColor myNavBorderColour) $
    WN.windowNavigation $
    smartBorders $
    mkToggle (single TABBED) $
    mouseResizableTile
    ||| emptyBSP
