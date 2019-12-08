-- The xmonad configuration of Derek Taylor (DistroTube)
-- http://www.youtube.com/c/DistroTube
-- http://www.gitlab.com/dwt1/

------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work

    -- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

------------------------------------------------------------------------
---CONFIG
------------------------------------------------------------------------
myFont          = "xft:Fira Mono for Powerline:regular:pixelsize=12"
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "st"      -- Sets default terminal
myTextEditor    = "emacs"     -- Sets default text editor
myBorderWidth   = 2         -- Sets border width for windows

main = do
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
	, handleEventHook = fullscreenEventHook <+> handleEventHook desktopConfig
        , normalBorderColor  = "#292d3e"
        , focusedBorderColor = "#bbc5ff"
        } `additionalKeysP`         myKeys

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
          spawnOnce "emacs --daemon &"
	  spawnOnce "/home/hainguyen/.config/polybar/launch.sh"
          spawnOnce "nitrogen --restore &"
          spawnOnce "compton --config /home/hainguyen/.config/compton.conf &"
          setWMName "LG3D"
          --spawnOnce "exec /usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --transparent true --alpha 0 --tint 0x292d3e --height 19 &"
          --spawnOnce "/home/dt/.xmonad/xmonad.start" -- Sets our wallpaper

------------------------------------------------------------------------
---GRID SELECT
------------------------------------------------------------------------

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys =
    --- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile;xmonad --restart")      -- Recompiles xmonad
        , ("M-S-<backspace>", spawn "prompt \"Do you want to shutdown\" \"shutdown -h now \"")                  -- Quits xmonad

    --- Windows
        , ("M-q", kill1)                           -- Kill the currently focused client
        , ("M-S-q", killAll)                         -- Kill all the windows on current workspace

    --- Floating windows
        , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.


    --- Grid Select
        , (("M-S-t"), spawnSelected'
          [ ("Audacity", "audacity")
          , ("Emacs", "emacs")
          , ("Firefox", "firefox")
          , ("Simple Terminal", "st")
          , ("Surf Browser",    "surf suckless.org")
          ])

        , ("M-S-g", goToSelected $ mygridConfig myColorizer)
        , ("M-S-b", bringSelected $ mygridConfig myColorizer)
 --- Windows navigation
        , ("M-o", windows W.focusMaster)             -- Move focus to the master window
        , ("M-e", windows W.focusDown)               -- Move focus to the next window
        , ("M-u", windows W.focusUp)                 -- Move focus to the prev window
        , ("M-S-o", windows W.swapMaster)            -- Swap the focused window and the master window
        , ("M-S-u", windows W.swapDown)              -- Swap the focused window with the next window
        , ("M-S-e", windows W.swapUp)                -- Swap the focused window with the prev window
        , ("M-<Backspace>", promote)                 -- Moves focused window to master, all others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
        , ("M-S-s", windows copyToAll)
        , ("M-C-s", killAllOtherCopies)

    --- Layouts
        , ("M-<Space>", sendMessage NextLayout)                              -- Switch to next layout
        , ("M-S-<Space>", sendMessage ToggleStruts)                          -- Toggles struts
        , ("M-S-n", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
        , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-f", sendMessage (T.Toggle "float"))
        , ("M-S-x", sendMessage $ Toggle REFLECTX)
        , ("M-S-y", sendMessage $ Toggle REFLECTY)
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

        , ("M-<Left>", sendMessage Shrink)
        , ("M-<Right>", sendMessage Expand)
        , ("M-C-<Right>", sendMessage MirrorShrink)
        , ("M-C-<Left>", sendMessage MirrorExpand)

    --- Workspaces
        , ("M-[", moveTo Next nonNSP)                                -- Go to next workspace
        , ("M-]", moveTo Prev nonNSP)                           -- Go to previous workspace
        , ("M-S-[", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
        , ("M-S-]", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

    --- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
        , ("M-k", namedScratchpadAction myScratchPads "spotify")

    --- Open Terminal
        -- , ("M-<Return>", spawn myTerminal)
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [
      (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where
                 myDefaultLayout = tall ||| grid ||| threeCol ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats


tall       = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol   = renamed [Replace "threeCol"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2)
threeRow   = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "oneBig"]   $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 $ Full
space      = renamed [Replace "space"]    $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "floats"]   $ limitWindows 20 $ simplestFloat

------------------------------------------------------------------------
---SCRATCHPADS
------------------------------------------------------------------------

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "spotify" spawnCmus findCmus manageCmus
                ]

    where
    spawnTerm  = myTerminal ++  " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCmus  = myTerminal ++  " -n spt 'spt'"
    findCmus   = resource =? "spt"
    manageCmus = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.8
                 w = 0.8
                 t = 0.85 -h
                 l = 0.85 -w
