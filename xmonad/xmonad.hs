import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.NoBorders
import XMonad.Layout.IndependentScreens(countScreens)
import XMonad.Layout.Simplest
import XMonad.Layout.PerWorkspace
import XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
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

myTerminal = "urxvt"
myWebBrowser = "firefox"

myWorkspaceNames = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceNamesSet = Set.fromList myWorkspaceNames

myXmobarHiddenNoWindowsFilter ws = if Set.member ws myWorkspaceNamesSet then ws else ""
noScratchPad ws = if ws == "NSP" then "" else ws

lightGrey = "#aaaaaa"
darkGrey = "#666666"

myNormalBorderColour    = "#000000"
myFocusedBorderColour   = lightGrey
myNavBorderColour       = darkGrey
myXmobarTitle           = xmobarColor lightGrey "" . shorten 50
myXmobarCurrent         = xmobarColor "#00ff00" ""
myXmobarVisible         = xmobarColor "#ffff00" ""
myXmobarUrgent          = xmobarColor "#ffffff" "#ff0000"
myXmobarHidden          = xmobarColor lightGrey "" . noScratchPad
myXmobarHiddenNoWindows = xmobarColor darkGrey "" . myXmobarHiddenNoWindowsFilter
myXmobarLayout          = xmobarColor darkGrey "" . noScratchPad
myXmobarSep             = " | "

hPutStrLnMulti :: [Handle] -> String -> IO ()
hPutStrLnMulti handles string = mapM_ (`hPutStrLn` string) handles

myConfig xmobars =
    let c = def {
          terminal        = myTerminal
        , modMask         = mod1Mask
        , manageHook      = manageDocks <+> manageScratchPad <+> myManageHook <+> manageHook def
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


toWorkspace name = doF(S.shift name)

myManageHook = composeAll [
      className =? "Firefox"     --> toWorkspace "1"
    , className =? "Thunderbird" --> toWorkspace "3"
    , className =? "Gimp"        --> toWorkspace "5"
    , className =? "Steam"       --> toWorkspace "4"
    , isFullscreen --> doFullFloat
    , fullscreenManageHook
    ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (S.RationalRect l t w h)
  where
    h = 0.25
    w = 1
    t = 1 - h
    l = 1 - w

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
    , ("M-S-h"          , sendMessage $ ExpandTowards L)
    , ("M-S-s"          , sendMessage $ ExpandTowards R)
    , ("M-S-n"          , sendMessage $ ExpandTowards U)
    , ("M-S-t"          , sendMessage $ ExpandTowards D)
    , ("M-C-h"          , sendMessage $ ShrinkFrom L)
    , ("M-C-s"          , sendMessage $ ShrinkFrom R)
    , ("M-C-n"          , sendMessage $ ShrinkFrom U)
    , ("M-C-t"          , sendMessage $ ShrinkFrom D)
    , ("M-r"            , sendMessage Rotate)
    , ("M-c"            , sendMessage Swap)
    , ("M-S-C-b"        , sendMessage FocusParent)
    , ("M-C-b"          , sendMessage SelectNode)
    , ("M-S-b"          , sendMessage MoveNode)
    , ("M-<Tab>"        , focusUp)
    , ("M-S-<Tab>"      , focusDown)
    , ("M-<Space>"      , sendMessage NextLayout)
    , ("M-S-<Space>"    , O.windows S.swapMaster)
    , ("M-w"            , sendMessage $ IncMasterN 1)
    , ("M-v"            , sendMessage $ IncMasterN (-1))
    , ("M-a"            , spawn "setxkbmap en_US; xmodmap $HOME/.Xmodmap")
    , ("M-m"            , spawn "setxkbmap dvorak; xmodmap $HOME/.Xmodmap")
    , ("M-y"            , withFocused $ windows . S.sink)
    , ("M-e"            , scratchpadSpawnActionTerminal myTerminal)
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

myLayout =
    let gameWorkspace = "4" in
    let fullScreenLayout = onWorkspace gameWorkspace gameFull Full in
    let splitLayout = emptyBSP in
    -- for changes to navBorderColour to take effect, one must
    -- toggle the following line
    WN.configurableNavigation (WN.navigateColor myNavBorderColour) $
    WN.windowNavigation $
    smartBorders $
    fullScreenLayout
    ||| splitLayout
    where
        gameFull = noBorders ((fullscreenFloat . fullscreenFull) Full)
