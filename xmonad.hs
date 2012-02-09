import XMonad 
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Layout.NoBorders
{-import XMonad.Layout.HintedGrid-}
import XMonad.Layout.Grid
import XMonad.Layout.Roledex
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import Data.Ratio ((%))

myWorkspaces = ["1:web","2:code","3:eclipse","4:other","5:chat","6:email"]   

gridLayout = spacing 8 $ Grid

empathyLayout = withIM (18/100) (Role "buddy_list") gridLayout

commonLayout = avoidStruts(smartBorders(tiled)) ||| 
            noBorders(Full) ||| 
            noBorders(Mirror tiled)
   where 
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio = 1/2
      delta = 3/100

defaultLayout = Tall 1 (1/2) (3/100)

myLayoutHook = onWorkspaces ["1:web","2:code","3:eclipse","4:other","6:email"] commonLayout $
               onWorkspace "5:chat" empathyLayout $ defaultLayout

myManageHook :: [ManageHook]
myManageHook =  
    [ className =? "vmware" --> doFloat
	 , className =? "Empathy"--> doShift "8:chat" 
	 , resource  =? "Do"     --> doIgnore 
	]

main = xmonad gnomeConfig
    { 
       startupHook = setWMName "LG3D",
       focusFollowsMouse = False,
       layoutHook = myLayoutHook,
       workspaces = myWorkspaces,
	    manageHook = manageHook gnomeConfig <+> composeAll myManageHook 
    }
