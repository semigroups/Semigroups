#############################################################################
##
#W  make_doc.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: makedoc.g 52 2010-06-20 13:12:39Z jamesm $
##

## gap -q -b < makedoc.g

LoadPackage("GAPDoc");

files:=[ "../gap/general.gd", "../gap/autos.gd", "../gap/transform.gd", 
 "../gap/greens.gd", "../gap/orbits.gd", "../gap/properties.gd",  
 "../gap/semigroups.gd", "../gap/semihomo.gd", "../gap/isomorph.gd", 
 "../gap/congruences.gd"];;

MakeGAPDocDoc(Concatenation(PackageInfo("MONOID")[1]!.InstallationPath, "/doc"), 
"monoid.xml", files, "manual");;

quit;

