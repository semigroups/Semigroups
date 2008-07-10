##
## installtest.g
## Version 3.1.2
## Thu 10 Jul 2008 20:25:38 BST
##

#Read( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "installtest.g" ) );;

dirs := DirectoriesPackageLibrary( "monoid", "tst" );;
ReadTest( Filename( dirs, "install_no_grape.tst" ) );

if IsBound(AutGroupGraph) and IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then 
	ReadTest( Filename( dirs, "install_with_grape.tst" ) );
fi;