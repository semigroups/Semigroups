##
## read.g
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##


ReadPkg("monoid/gap/general.gi");
ReadPkg("monoid/gap/semigroups.gi");
ReadPkg("monoid/gap/semihomo.gi");
#ReadPkg("monoid/gap/orbits.gi");
ReadPkg("monoid/gap/greens.gi");
ReadPkg("monoid/gap/transform.gi");
ReadPkg("monoid/gap/properties.gi");
ReadPkg("monoid/gap/autos.gi");

#check that grape is loaded and fully installed

if not ARCH_IS_UNIX() or ForAny( ["drcanon4", "dreadnautB", "drtogap4", "gap4todr"], file -> Filename(DirectoriesPackagePrograms("grape"), file) = fail ) or not IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) or not Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then
	Info(InfoWarning, 1, "the `grape' package is not fully installed and so some of the functions");
	Info(InfoWarning, 1, "in MONOID will not work. Type `?the monoid package' for more infomation.");
else 
	ReadPkg("monoid/gap/grape.gi");
fi;

#check that the orb package is loaded

if  Filename(DirectoriesPackagePrograms("orb"), "orb.so") = fail then 
	Info(InfoWarning, 1, "the `orb' package is not fully installed and so some functions");
	Info(InfoWarning, 1, "in MONOID will not be as efficient as they would be if `orb' were loaded.");
	ReadPkg("monoid/gap/orbits_no_orb.gi");
else
	ReadPkg("monoid/gap/orbits_orb.gi");
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