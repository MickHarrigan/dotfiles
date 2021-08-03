-- Main import of the module
import XMonad

-- System required imports
import Data.Monoid
import System.Exit

-- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Cursor

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.Minimize

-- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Magnifier
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.Renamed
import XMonad.Layout.LimitWindows
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS, FULL))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Etc.
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe


-- Currently used terminal
myTerminal      = "alacritty"

-- Current font
myFont          = "xft:SauceCodePro Nerd Font Mono:regular:size=12:antialias=true:hinting=true"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 3

-- Sets the main modifier key 
    -- mod1Mask = left alt
    -- mod2Mask = ???
    -- mod3Mask = right alt
    -- mod4Mask = winkey (Super)
myModMask       = mod4Mask

-- Workspaces and their names, amount is determined by the literal amount, its really confusing
myWorkspaces    = ["web","code","docs","disc","steam","game","music","cmd"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Border colors for unfocused and focused windows, respectively.
-- Current theme is Solarized Dark, which is going to change sometime soon
myNormalBorderColor  = "#839496"
myFocusedBorderColor = "#cb4b16"

------------------------------------------------------------------------
-- Key bindings 
myKeys conf@(XConfig {XMonad.modMask = win}) = M.fromList $
    -- win is the current main modMask

    -- launch a terminal
    [ ((win,               xK_t     ), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((win,               xK_p     ), spawn "dmenu_run")
    
    -- close focused window
    , ((win,               xK_Escape), kill)

     -- Rotate through the available layout algorithms
    , ((win,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((win .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Toggles the floating layout
    , ((win,               xK_f     ), sendMessage (T.Toggle "floats"))

    -- Resize viewed windows to the correct size
    , ((win,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((mod1Mask,          xK_Tab   ), windows W.focusDown)

    -- Move focus to the previous window
    , ((mod1Mask .|. shiftMask,  xK_Tab     ), windows W.focusUp  )

    -- Focuses the Main window
    , ((win,               xK_Return), windows W.focusMaster)

    -- Promotes the focused window to the Main window
    , ((win .|. shiftMask, xK_Return), promote)

    -- Swap the focused window with the next window
    , ((win .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((win .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((win,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((win,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((win .|. shiftMask, xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((win              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((win              , xK_period), sendMessage (IncMasterN (-1)))

    -- Moves the focus to the next workspace
    , ((win              , xK_Tab   ), nextWS)

    -- Moves the focus back to the previous workspace
    , ((win .|. shiftMask, xK_Tab   ), prevWS)

    -- Removes the xmobar
    , ((win              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((win .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((win              , xK_q     ), spawn "xmonad --recompile; pkill xmobar; xmonad --restart")

    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. win, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. win, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = win}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((win, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((win, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((win, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ layouts
    where
        layouts = withBorder myBorderWidth tall
            ||| smartBorders tabs
            ||| magnify

-- Defining the actual layouts themselves
tall    = renamed [Replace "tall"]
          $ ResizableTall 1 (3/100) (1/2) []

magnify = renamed [Replace "magnify"]
          $ smartBorders
          $ addTabs shrinkText myTabTheme
          $ magnifier
          $ limitWindows 4
          $ mySpacing 8
          $ ResizableTall 1 (3/100) (1/2) []

tabs    = renamed [Replace "tabs"]
          $ tabbed shrinkText myTabTheme

-- Spacing function for creating an "empty" border around the windows
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- same as the above function, but removes the border
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Tab theme
myTabTheme = def { fontName             = myFont 
                 , activeColor          = "#073642"
                 , inactiveColor        = "#002b36"
                 , activeBorderColor    = "#839496"
                 , inactiveBorderColor  = "#839496"
                 , activeTextColor      = "#839496"
                 , inactiveTextColor     = "#839496"
                 }

-- Shows the WS name when switching
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font      = "xft:SauceCodePro Nerd Font Bold:size=45"
    , swn_fade      = 1.0
    , swn_bgcolor   = "#073642"
    , swn_color     = "#839496"
    }

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = docksEventHook
------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmonad $ docks def 
        {
        -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = dynamicLogWithPP $ xmobarPP
                                { ppOutput          = hPutStrLn xmproc
                                , ppCurrent         = xmobarColor "#cb4b16" "" . wrap "[" "]"
                                , ppVisible         = xmobarColor "#268bd2" "" . clickable
                                , ppHidden          = xmobarColor "#2aa198" "" . wrap "(" ")" . clickable
                                , ppHiddenNoWindows = xmobarColor "#839496" "" . clickable
                                , ppTitle           = xmobarColor "#859900" "" . shorten 60
                                , ppSep             = "<fc=#839496>|</fc>"
                                , ppWsSep           = "<fc=#839496>:</fc>"
                                , ppUrgent          = xmobarColor "#d33682" "" . wrap "!" "!"
                                , ppExtras          = [windowCount]
                                , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                }
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
