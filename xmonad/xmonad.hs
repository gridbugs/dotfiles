import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import System.IO
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ myConfig xmproc

myTerminal = "urxvt"
myWebBrowser = "firefox"

myWorkspaceNames = ["web", "code", "mail", "gimp", "steam", "scratch"]
myWorkspaceDisplayNames = map (\(name, num) -> show num ++ ":" ++ name) $ zip myWorkspaceNames [1..]
myWorkspaceDisplayNameSet = Set.fromList myWorkspaceDisplayNames
myWorkspaceTable =
    M.fromList $ zip myWorkspaceNames myWorkspaceDisplayNames
workspace name = fromJust $ M.lookup name myWorkspaceTable

myXmobarHiddenNoWindowsFilter ws = if Set.member ws myWorkspaceDisplayNameSet then ws else ""
noScratchPad ws = if ws == "NSP" then "" else ws

myNormalBorderColour    = "#000000"
myFocusedBorderColour   = "#cc0000"
myNavBorderColour       = "#444488"
myXmobarTitle           = xmobarColor "#6688ff" "" . shorten 50
myXmobarCurrent         = xmobarColor "#ffff00" ""
myXmobarHidden          = xmobarColor "#aaaaaa" "" . noScratchPad
myXmobarHiddenNoWindows = xmobarColor "#666666" "" . myXmobarHiddenNoWindowsFilter
myXmobarLayout          = xmobarColor "#ff8866" "" . noScratchPad


myConfig xmproc =
    let c = def {
          terminal        = myTerminal
        , modMask         = mod1Mask
        , manageHook      = manageDocks <+> manageScratchPad <+> myManageHook <+> manageHook def
        , layoutHook      = avoidStruts myLayout
        , handleEventHook = docksEventHook <+> handleEventHook def
        , startupHook     = docksStartupHook <+> startupHook def
        , logHook         = dynamicLogWithPP xmobarPP
            { ppOutput    = hPutStrLn xmproc
            , ppTitle     = myXmobarTitle
            , ppCurrent   = myXmobarCurrent
            , ppHidden    = myXmobarHidden
            , ppHiddenNoWindows = myXmobarHiddenNoWindows
            , ppLayout    = myXmobarLayout
            }
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        , borderWidth        = 4
        , XMonad.workspaces  = myWorkspaceDisplayNames
        , keys            = const M.empty
        }
    in additionalKeysP c $ myKeys c

toWorkspace name = doF(S.shift $ workspace name)

myManageHook = composeAll [
      className =? "Firefox"     --> toWorkspace "web"
    , className =? "Thunderbird" --> toWorkspace "mail"
    , className =? "Gimp"        --> toWorkspace "gimp"
    , className =? "Steam"       --> toWorkspace "steam"
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
    , ("M-S-s"          , sendMessage Expand)
    , ("M-S-h"          , sendMessage Shrink)
    , ("M-<Space>"      , sendMessage NextLayout)
    , ("M-S-<Space>"    , O.windows S.swapMaster)
    , ("M-S-n"          , O.windows S.swapUp)
    , ("M-S-t"          , O.windows S.swapDown)
    , ("M-,"            , sendMessage $ IncMasterN 1)
    , ("M-."            , sendMessage $ IncMasterN (-1))
    , ("M-a"            , spawn "setxkbmap en_US")
    , ("M-m"            , spawn "setxkbmap dvorak")
    , ("M-e"            , scratchpadSpawnActionTerminal myTerminal)
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 4")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 4")
    , ("M-<XF86MonBrightnessDown>", spawn "kb-light.py --down 1")
    , ("M-<XF86MonBrightnessUp>", spawn "kb-light.py --up 1")
    ]
    ++
    [ ("M-" ++ s ++ show k, O.windows $ f w)
        | (w, k) <- zip (workspaces c) [1..]
        , (s, f) <- zip ["", "S-"] [S.view, S.shift]
    ]

myLayout = WN.configurableNavigation (WN.navigateColor myNavBorderColour) $
    WN.windowNavigation $
        noBorders Full
    ||| Tall 1 (4/100) (3/4)
    ||| Mirror (Tall 1 (4/100) (3/4))
