#############################################################################
##
#W  installtest.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#Read( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "installtest.g" ) );;


ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "install_no_grape.tst" ) );

if ARCH_IS_UNIX() and ForAll( ["drcanon4", "dreadnautB", "drtogap4", "gap4todr"], file -> not 
 Filename(DirectoriesPackagePrograms("grape"), file) = fail ) and 
  IsIsomorphicGraph( JohnsonGraph(7,3), JohnsonGraph(7,4) ) and 
   Size(AutGroupGraph( JohnsonGraph(4,2) ) )=48 then
	
	ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), 
	 "install_with_grape.tst" ) );
fi;