##
## installtest.g
## Version 3.1.1
## Mon Jun  9 17:02:20 BST 2008
##

#Read( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "installtest.g" ) );;

dirs := DirectoriesPackageLibrary( "monoid", "tst" );;
ReadTest( Filename( dirs, "install_no_grape.tst" ) );

if IsBound(AutGroupGraph) and IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then 
	ReadTest( Filename( dirs, "install_with_grape.tst" ) );
fi;