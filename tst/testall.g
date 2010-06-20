#############################################################################
##
#W  testall.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#Read( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "testall.g" ) );;

#SizeScreen([80]); SetInfoLevel(InfoWarning, 0); TestManualExamples(DirectoriesPackageLibrary("monoid","doc")[1]![1], "monoid.xml", ["../gap/autos.gd", "../gap/general.gd", "../gap/greens.gd", "../gap/orbits.gd", "../gap/properties.gd", "../gap/semigroups.gd", "../gap/semihomo.gd", "../gap/transform.gd"] );

LoadPackage( "monoid" );;

dirs := DirectoriesPackageLibrary( "monoid", "tst" );;

if IsBound(GAPInfo.PackagesInfo.orb) and CompareVersionNumbers("3.4", "orb") 
 then 
	ReadTest( Filename( dirs, "orbits_orb.tst" ) );
else
	ReadTest( Filename ( dirs, "orbits_no_orb.tst"));
fi;

ReadTest( Filename( dirs, "autos1.tst" ) );

if IsBound(AutGroupGraph) and IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then 
	ReadTest( Filename( dirs, "autos2.tst" ) );
fi;

if IsBound(GAPInfo.PackagesInfo.smallsemi) and CompareVersionNumbers("0.6", 
 "smallsemi") then 
  ReadTest( Filename( dirs, "cong_no_smallsemi.tst" ) );
fi;

ReadTest( Filename( dirs, "autos3.tst" ) );
ReadTest( Filename( dirs, "greens.tst" ) );
ReadTest( Filename( dirs, "properties.tst" ) );
ReadTest( Filename( dirs, "semigroups.tst" ) );
ReadTest( Filename( dirs, "semihomo.tst" ) );
ReadTest( Filename( dirs, "transform.tst" ) );

Read(Filename(dirs, "installtest.g"));
