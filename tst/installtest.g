##
## installtest.g
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##

#Read( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "installtest.g" ) );;

dirs := DirectoriesPackageLibrary( "monoid", "tst" );;
ReadTest( Filename( dirs, "install_no_grape.tst" ) );

if ARCH_IS_UNIX() and ForAll( ["drcanon4", "dreadnautB", "drtogap4", "gap4todr"], file -> not Filename(DirectoriesPackagePrograms("grape"), file) = fail ) and IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then
	ReadTest( Filename( dirs, "install_with_grape.tst" ) );
fi;