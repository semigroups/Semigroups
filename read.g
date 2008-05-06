##
## read.g
## Version 3.1
## Fri May  2 17:42:56 BST 2008
##


ReadPkg("monoid/gap/general.gi");
ReadPkg("monoid/gap/semigroups.gi");
ReadPkg("monoid/gap/semihomo.gi");
ReadPkg("monoid/gap/orbits.gi");
ReadPkg("monoid/gap/greens.gi");
ReadPkg("monoid/gap/transform.gi");
ReadPkg("monoid/gap/properties.gi");
ReadPkg("monoid/gap/autos.gi");

if IsBound(AutGroupGraph) and IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then 
	ReadPkg("monoid/gap/grape.gi");
else
	Print("#I It appears that the `grape' package is not fully installed. As a \n#I consequence some of the functions in MONOID will not work. Please\n#I refer to the MONOID manual for further details.\n");
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