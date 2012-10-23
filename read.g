#############################################################################
##
#W  read.g
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

if IsBound(SEMIGROUPSC) then ReadPackage("semigroups/gap/pperm.gi"); fi;
ReadPackage("semigroups/gap/partition.gi");
ReadPackage("semigroups/gap/binary.gi");
ReadPackage("semigroups/gap/matrix.gi");

ReadPackage("semigroups/gap/acting.gi");
ReadPackage("semigroups/gap/setup.gi");

ReadPackage("semigroups/gap/semigroups.gi");
ReadPackage("semigroups/gap/greens.gi");
ReadPackage("semigroups/gap/slp.gi");
ReadPackage("semigroups/gap/regular.gi");
if IsBound(SEMIGROUPSC) then ReadPackage("semigroups/gap/inverse.gi"); fi;
ReadPackage("semigroups/gap/enums.gi");
ReadPackage("semigroups/gap/iterators.gi");
ReadPackage("semigroups/gap/properties.gi");
ReadPackage("semigroups/gap/attributes.gi");
ReadPackage("semigroups/gap/orbits.gi");

ReadPackage("semigroups/gap/examples.gi");

ReadPackage("semigroups/gap/utils.gi");


