#############################################################################
##
#W  make_doc.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

## gap -q -b < makedoc.g

LoadPackage("GAPDoc");

files:=[ "../gap/general.gd", "../gap/autos.gd", "../gap/transform.gd", "../gap/greens.gd",
 "../gap/orbits.gd", "../gap/properties.gd",  "../gap/semigroups.gd", "../gap/semihomo.gd"];;

MakeGAPDocDoc(Concatenation(PackageInfo("MONOID")[1]!.InstallationPath, "/doc"), 
"monoid.xml", files, "manual");;

quit;

