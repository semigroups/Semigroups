#############################################################################
##
#W  read.g
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPackage("citrus/gap/transform.gi");
if IsBound(CITRUSC) then ReadPackage("citrus/gap/pperm.gi"); fi;
ReadPackage("citrus/gap/partition.gi");
ReadPackage("citrus/gap/binary.gi");
ReadPackage("citrus/gap/matrix.gi");

ReadPackage("citrus/gap/acting.gi");
ReadPackage("citrus/gap/setup.gi");

ReadPackage("citrus/gap/semigroups.gi");
ReadPackage("citrus/gap/greens.gi");
ReadPackage("citrus/gap/slp.gi");
ReadPackage("citrus/gap/regular.gi");
if IsBound(CITRUSC) then ReadPackage("citrus/gap/inverse.gi"); fi;
ReadPackage("citrus/gap/enums.gi");
ReadPackage("citrus/gap/iterators.gi");
ReadPackage("citrus/gap/properties.gi");
ReadPackage("citrus/gap/attributes.gi");
ReadPackage("citrus/gap/orbits.gi");

ReadPackage("citrus/gap/examples.gi");

ReadPackage("citrus/gap/utils.gi");


