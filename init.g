#############################################################################
##
#W  init.g
#Y  Copyright (C) 2011                                     James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPkg("citrus/gap/convenience.gd");
ReadPkg("citrus/gap/orbits.gd");
ReadPkg("citrus/gap/greens.gd");
ReadPkg("citrus/gap/properties.gd");
ReadPkg("citrus/gap/transform.gd");

DeclareInfoClass("InfoCitrus");;

#JDM remove

#SetInfoLevel(InfoMonoidAutos, 4);

#dir:=Concatenation(PackageInfo("MONOID")[1]!.InstallationPath,"/dev/gap");
#files:=DirectoryContents(dir);
#dir:=Directory(dir);
#for x in files do 
#	if not (x="." or x=".." or x=".DS_Store") and SplitString(x, ".")[2]="gd" then 
#		x:=Filename(dir, x);
#		Read(x);
#	fi;
#od;

#JDM remove
