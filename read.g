#############################################################################
##
#W  read.g
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPackage("citrus/gap/utils.gi");
ReadPackage("citrus/gap/orbits.gi");
ReadPackage("citrus/gap/greens.gi");
ReadPackage("citrus/gap/transform.gi");
ReadPackage("citrus/gap/h.gi");
ReadPackage("citrus/gap/r.gi");
ReadPackage("citrus/gap/l.gi");
ReadPackage("citrus/gap/d.gi");
ReadPackage("citrus/gap/properties.gi");
ReadPackage("citrus/gap/semigroups.gi");
ReadPackage("citrus/gap/examples.gi");

if IsBound(CITRUSC) then 
  ReadPackage("citrus/gap/pperm.gi");
  ReadPackage("citrus/gap/inverse.gi");
fi;
