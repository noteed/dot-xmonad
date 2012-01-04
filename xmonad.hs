import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.NoBorders
import XMonad.Util.Paste
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Char
import System.IO
import System.Exit

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#00ffff"

myTerminal = "xterm -fa MiscFixed -fs 9 -b 2"
--myTerminal = "urxvt -fn "xft:DejaVu Sans Mono:pixelsize=12" -b 2 +sb"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    [
    -- launch a terminal
      ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu -b` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_F12   ), spawn "sudo halt")
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)

    -- send some keys
    , ((mod3Mask             , xK_k     ), withFocused $ sendKeyWindow noModMask xK_Delete)
    , ((mod3Mask             , xK_l     ), withFocused $ sendKeyWindow noModMask xK_BackSpace)
    , ((mod3Mask             , xK_p     ), pasteString "hello")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

main = do
  xmonad $ defaultConfig {
           normalBorderColor  = myNormalBorderColor
         , focusedBorderColor = myFocusedBorderColor

         , modMask = mod4Mask     -- Rebind Mod to the Windows key
         , terminal = myTerminal
         , keys = myKeys

         , layoutHook = smartBorders (layoutHook defaultConfig)
         , manageHook = manageHook defaultConfig
                          <+> (title =? "GLUT" --> doFloat)
--                          <+> (className =? "feh" --> doFloat)
                          <+> (title =? "AEDPY" --> doFloat)
         }

