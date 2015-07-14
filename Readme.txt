This plugin imports bones, skined meshes, vertex colors, creates a character assembly and exports the animations.

The script is based on bmdview2pre3 by thakis (homepage at http://amnoid.de/gc/ download source at http://www.emutalk.net/showthread.php?t=26919&page=39).
The main changes are in the BModel.ms script that generates the screen.

To install 

To run BmdView.exe you might need to install on of the following packages
32 bit system  Microsoft Visual C++ 2005 Redistributable Package (x86).  http://go.microsoft.com/fwlink/?linkid=65127
64 bit system  Microsoft Visual C++ 2005 Redistributable Package (ia64).   http://www.microsoft.com/downloads/details.aspx?familyid=90548130-4468-4bbc-9673-d6acabd5d13b&displaylang=en
Install the kW x-port plugin from http://kwxport.sourceforge.net/.

IMPORTANT: If upgrading from an older version delete the BmdView folder and ImporterUI.ms in max's plugins directory.
unzip MaxBMD.zip
place BmdView.exe into 3ds max's root folder e.g. C:\Program Files\Autodesk\3ds Max 2008\BmdView.exe
Click the MAXScript item in the Main Menu, select run Script... select "UnzipedPath\MaxBMD.ms"
Go to Customize > Customize User Interface and select the Toolbars tab.
Search for "MaxBMD" in the category dropdown.
Drag the script to any toolbar. Click the button to start the importer.
Once it's in the toolbar it will be automatically evaluated on startup and will be available to the user in future sessions. 

To uninstall
Delete the MaxBMD-MaxBMD.mcr file from [MAX_PATH]\UI\MacroScripts. 
Note: in vista this might be located in C:\Users\[USERNAME]\AppData\Local\Autodesk\3dsmax\2008 - 32bit\enu\UI\usermacros

IMPORTANT VISTA NOTE: If your using vista the files you delete still exist and 3dsMax will still load them (Volume Shadow Copy).
If you get an error about "Math.ms" not found when you start 3ds max then download TextPad. From textpad open file and search for the MaxBMD folder 
and delete it from the file search popup.

I've only tested this with a few Zelda models so it might not work with other games.

I'm using the old (slightly modifed) version of BmdView.exe to extract the textures so not all textures are automatically extracted.
Use BmdView2 to extract any missing tga files, delete the files in the textures folder with a *.ERROR extension and rerun the script to continue.
If anyone wants to update the exe to process all files formats (and maybe also directly export the images in tga format) 
then create a command line app that accepts two args (bmd filepath and destination path) and also name any invalid files 
with the original filename and an extension of ".ERROR". 

The importer creates a folder matching the bmd filename in the same directory and automatically saves the files.
e.g. import "C:\Zelda\bmdr\link.bmd" creates
"C:\Zelda\bmdr\link\link.max"
"C:\Zelda\bmdr\link\Textures\test.tga"
"C:\Zelda\bmdr\link\Animations\test.anm" -- these animations are loaded from "C:\Zelda\bck\link*.bck"

Character bones are setup as a character group. To open the character for editing select the character and from the main menu 
select Group/Assembly/Open. 
The animations can be merged in using the "Insert Animation" button from the "character Assembly" panel. You might also want to
tick the "Adjust Current Time Range" checkbox to match the animation length. 
You can view the bones by selecting the "All Objects" in the "Display" section of the character assembly tab.
Note that the model is frozen by default. Right click in a viewport and select "unfrezze all" to edit the mesh / skinning.

Bone notes:
Extra dummy objects have been added inbetween each bone that use scaling animations. When the parent bone is scaled a controller will remove the scaling from the child bones.
This could cause problems exporting to other 3D programs and also issues with the IK.
The bck files store scaling as absolute values that have no effect on the children e.g. arm = 200% then hand still = 100%.
I haven't been able to get scaling working properly in 3DS max, whenever the parent bone is scaled the chlld bones become skewed. 
Should hide the dummy helpers when animating the bones but be sure to unhide them for .x exporting. 
The bck files also animate positions which squish the parent bone, this normally happens in face animations. To fix this any bone with position animations is flaged as
not being turned on (Animation/bone tools.../Object Properties/Bone On)

UI:
Save Animations: I disable it on the first few loads until I get a decent bone size
Export textures: if the BmdView.exe isn't working or if some image types aren't exporting it might be easier to disable exporting textures.
	Run the importer once then run BmdView2.exe and extract the tga images to the "Textures" folder. Enter "_" for the filename. Then run the importer again.
Include scaling: creates dummy objects inbetween bones to allow for scaling. Can cause issues when exporting to another 3D editing program and it increases the number of bones in a .x file.
.X export (game): creates a .x file in the same path as the .max file which contains all the models, textures and animations. Requires the kW x-port plugin from http://kwxport.sourceforge.net/.
character export (modeling): creates the character assembly and animation files.

If you find any issues please post them at http://www.emutalk.net/threads/44490
