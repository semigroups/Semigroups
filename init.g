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

ReadPkg("monoid/gap/general.gd");
ReadPkg("monoid/gap/semigroups.gd");
ReadPkg("monoid/gap/semihomo.gd");
ReadPkg("monoid/gap/orbits.gd");
ReadPkg("monoid/gap/greens.gd");
ReadPkg("monoid/gap/properties.gd");
ReadPkg("monoid/gap/transform.gd");
ReadPkg("monoid/gap/autos.gd");
ReadPkg("monoid/gap/isomorph.gd");
ReadPkg("monoid/gap/congruences.gd");

###########################################################################
##
#M	InfoMonoid
##
##	the info class for the MONOID package, which can be used to obtain 
##	information about computations as they are being performed. 
##

InfoMonoid:=NewInfoClass("InfoMonoid");;

#JDM remove

#SetInfoLevel(InfoMonoidAutos, 4);

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