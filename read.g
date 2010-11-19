#############################################################################
##
#W  read.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

ReadPkg("monoid/gap/convenience.gi");
ReadPkg("monoid/gap/general.gi");
ReadPkg("monoid/gap/orbits.gi");
ReadPkg("monoid/gap/greens.gi");
ReadPkg("monoid/gap/transform.gi");
ReadPkg("monoid/gap/h.gi");
ReadPkg("monoid/gap/r.gi");
ReadPkg("monoid/gap/closure.gi");
ReadPkg("monoid/gap/l.gi");
ReadPkg("monoid/gap/d.gi");
ReadPkg("monoid/gap/properties.gi");
ReadPkg("monoid/gap/ideals.gi");


ReadPkg("monoid/gap/legacy.gi");

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