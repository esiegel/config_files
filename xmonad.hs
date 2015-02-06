import XMonad 
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Roledex
import XMonad.Layout.Reflect
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import Data.Ratio ((%))

myWorkspaces = ["1:web","2:code","3:eclipse","4:other","5:chat","6:email"]   

chatLayout = withIM (1%7) (Role "buddy_list") Grid

commonLayout = avoidStruts(smartBorders tiled) ||| 
            noBorders Full ||| 
            spiral (6/7) |||
            noBorders(Mirror tiled)
   where 
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio = 1/2
      delta = 3/100

defaultLayout = Tall 1 (1/2) (3/100)

myLayoutHook = onWorkspaces ["1:web","2:code","3:eclipse","4:other","6:email"] commonLayout $
               onWorkspace "5:chat" chatLayout defaultLayout

myManageHook :: [ManageHook]
myManageHook =  
    [ className =? "vmware"      --> doFloat
	 , resource  =? "Do"          --> doIgnore
	 , className =? "Eclipse"     --> doShift "3:eclipse"
	 , title     =? "Eclipse"     --> doShift "3:eclipse"
	 , className =? "Empathy"     --> doShift "5:chat"
	 , className =? "Pidgin"      --> doShift "5:chat"
	 , className =? "Mail"        --> doShift "6:email"
	 , className =? "Thunderbird" --> doShift "6:email"
	]

main = xmonad $ gnomeConfig
        { 
            {-handles java awt/swing ui-}
            startupHook = setWMName "LG3D",

            {-specify terminal executable-}
            terminal = "gnome-terminal",

            {-mod1Mask - left alt key-}
            {-mod2Mask - ?-}
            {-mod3Mask - right alt key-}
            {-mod4Mask - windows key-}
            modMask = mod1Mask,

            {-activating windows accidentally sucks-}
            focusFollowsMouse = False,

            {-use my named workspaces-}
            workspaces = myWorkspaces,

            {-use all my awesome layouts defined above-}
            layoutHook = myLayoutHook,

            manageHook = manageHook gnomeConfig <+> composeAll myManageHook
        }
