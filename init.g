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


ReadPkg("monoid/gap/convenience.gd");
ReadPkg("monoid/gap/general.gd");
ReadPkg("monoid/gap/orbits.gd");
ReadPkg("monoid/gap/greens.gd");
ReadPkg("monoid/gap/properties.gd");
ReadPkg("monoid/gap/transform.gd");
ReadPkg("monoid/gap/closure.gd");

ReadPkg("monoid/gap/legacy.gd");



###########################################################################
DeclareInfoClass("InfoMonoid");;

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