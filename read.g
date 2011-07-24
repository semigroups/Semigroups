#############################################################################
##
#W  read.g
#Y  Copyright (C) 2011                                     James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPkg("citrus/gap/convenience.gi");
ReadPkg("citrus/gap/orbits.gi");
ReadPkg("citrus/gap/greens.gi");
ReadPkg("citrus/gap/transform.gi");
ReadPkg("citrus/gap/h.gi");
ReadPkg("citrus/gap/r.gi");
ReadPkg("citrus/gap/l.gi");
ReadPkg("citrus/gap/d.gi");
ReadPkg("citrus/gap/properties.gi");

#JDM remove

#dir:=Concatenation(PackageInfo("MONOID")[1]!.InstallationPath,"/dev/gap");
#files:=DirectoryContents(dir);
#dir:=Directory(dir);
#for x in files do 
#	if not (x="." or x=".." or x=".DS_Store") and SplitString(x, ".")[2]="gi" then 
#		x:=Filename(dir, x);
#		Read(x);
#	fi;
#od;

#JDM remove
