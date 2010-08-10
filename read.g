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

ReadPkg("monoid/gap/general.gi");
ReadPkg("monoid/gap/semigroups.gi");
ReadPkg("monoid/gap/semihomo.gi");
ReadPkg("monoid/gap/orbits.gi");
ReadPkg("monoid/gap/greens.gi");
ReadPkg("monoid/gap/transform.gi");
ReadPkg("monoid/gap/greens_r_orb.gi");
ReadPkg("monoid/gap/greens_l_orb.gi");
ReadPkg("monoid/gap/greens_d_orb.gi");
ReadPkg("monoid/gap/properties.gi");
ReadPkg("monoid/gap/autos.gi");
ReadPkg("monoid/gap/isomorph.gi");
ReadPkg("monoid/gap/congruences.gi");

#check that grape is loaded and fully installed

if not ARCH_IS_UNIX() or ForAny( ["drcanon4", "dreadnautB", "drtogap4", "gap4todr"], 
 file -> Filename(DirectoriesPackagePrograms("grape"), file) = fail ) or not 
  IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) or not 
   Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then
	Print("\n");
	Info(InfoWarning, 1, "the `grape' package is not fully installed and so", 
	" some of the functions");
	Info(InfoWarning, 1, "in MONOID will not work. Type `?the monoid package'", 
	" for more infomation.");
else 
	ReadPkg("monoid/gap/grape.gi");
fi;

#check that the orb package is loaded

if IsBound(GAPInfo.PackagesInfo.orb) and CompareVersionNumbers("3.4", "orb") 
 then 
	ReadPkg("monoid/gap/orbits_orb.gi");
else
	Print("\n");
	Info(InfoWarning, 1, 
	 "the `orb' package is not fully installed and so MONOID will not be as");
	Info(InfoWarning, 1, "efficient as if `orb' were loaded.");
	ReadPkg("monoid/gap/orbits_no_orb.gi");
fi;

ReadPkg("monoid/gap/compat.g");

#JDM remove

dir:=Concatenation(PackageInfo("MONOID")[1]!.InstallationPath,"/dev/gap");
files:=DirectoryContents(dir);
dir:=Directory(dir);
for x in files do 
	if not (x="." or x=".." or x=".DS_Store") and SplitString(x, ".")[2]="gi" then 
		x:=Filename(dir, x);
		Read(x);
	fi;
od;

#JDM remove