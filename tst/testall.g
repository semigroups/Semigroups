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

ReadTest( Filename( dirs, "d.tst" ) );
ReadTest( Filename( dirs, "greens.tst" ) );
ReadTest( Filename( dirs, "h.tst" ) );
ReadTest( Filename( dirs, "l.tst" ) );
ReadTest( Filename( dirs, "orbits.tst" ) );
ReadTest( Filename( dirs, "properties.tst" ) ); 
ReadTest( Filename( dirs, "r.tst" ) );
ReadTest( Filename( dirs, "transform.tst" ) );
