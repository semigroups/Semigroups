##
## testall.g
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##

#LoadPackage( "monoid" );;
#dirs := DirectoriesPackageLibrary( "monoid", "tst" );;
#Read( Filename( dirs, "testall.g" ) );;

dirs := DirectoriesPackageLibrary( "monoid", "tst" );;
ReadTest( Filename( dirs, "autos1.tst" ) );

if IsBound(AutGroupGraph) and IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then 
	ReadTest( Filename( dirs, "autos2.tst" ) );
fi;

ReadTest( Filename( dirs, "autos3.tst" ) );
ReadTest( Filename( dirs, "greens.tst" ) );
ReadTest( Filename( dirs, "orbits.tst" ) );
ReadTest( Filename( dirs, "properties.tst" ) );
ReadTest( Filename( dirs, "semigroups.tst" ) );
ReadTest( Filename( dirs, "semihomo.tst" ) );
ReadTest( Filename( dirs, "transform.tst" ) );