#
# gapbind_demo: Minimal demo of how to use gapbind14
#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage( "gapbind_demo" );

TestDirectory(DirectoriesPackageLibrary( "gapbind_demo", "tst" ),
  rec(exitGAP := true));

FORCE_QUIT_GAP(1); # if we ever get here, there was an error
