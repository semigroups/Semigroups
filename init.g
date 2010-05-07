#############################################################################
##
#W  init.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

ReadPkg("MONOID/gap/general.gd");
ReadPkg("MONOID/gap/semigroups.gd");
ReadPkg("MONOID/gap/semihomo.gd");
ReadPkg("MONOID/gap/orbits.gd");
ReadPkg("MONOID/gap/greens.gd");
ReadPkg("MONOID/gap/properties.gd");
ReadPkg("MONOID/gap/transform.gd");
ReadPkg("MONOID/gap/autos.gd");

###########################################################################
##
#M	InfoMonoid
##
##	the info class for the MONOID package, which can be used to obtain 
##	information about computations as they are being performed. 
##

InfoMonoid:=NewInfoClass("InfoMonoid");;

#JDM remove

SetInfoLevel(InfoMonoidAutos, 4);

dir:=Concatenation(PackageInfo("MONOID")[1]!.InstallationPath,"/dev/gap");
files:=DirectoryContents(dir);
dir:=Directory(dir);
for x in files do 
	if not (x="." or x=".." or x=".DS_Store") and SplitString(x, ".")[2]="gd" then 
		x:=Filename(dir, x);
		Read(x);
	fi;
od;

#JDM remove