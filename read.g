#############################################################################
##
#W  read.g
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPackage("semigroups", "gap/smallestimage.g");

ReadPackage("semigroups", "gap/hash.gi");

ReadPackage("semigroups", "gap/elements/star.gi");
ReadPackage("semigroups", "gap/elements/pbr.gi");
ReadPackage("semigroups", "gap/elements/bipart.gi");
ReadPackage("semigroups", "gap/elements/blocks.gi");
ReadPackage("semigroups", "gap/elements/semiringmat.gi");
ReadPackage("semigroups", "gap/elements/maxplusmat.gi");
ReadPackage("semigroups", "gap/elements/boolmat.gi");
ReadPackage("semigroups", "gap/elements/ffmat.gi");
ReadPackage("semigroups", "gap/elements/trans.gi");
ReadPackage("semigroups", "gap/elements/elements.gi");
ReadPackage("semigroups", "gap/elements/pperm.gi");

ReadPackage("semigroups", "gap/main/fropin.gi");
ReadPackage("semigroups", "gap/main/setup.gi");
ReadPackage("semigroups", "gap/main/acting.gi");
ReadPackage("semigroups", "gap/main/lambda-rho.gi");
ReadPackage("semigroups", "gap/main/graded.gi");
ReadPackage("semigroups", "gap/main/orbits.gi");
ReadPackage("semigroups", "gap/main/semiact.gi");

ReadPackage("semigroups", "gap/semigroups/semigrp.gi");
ReadPackage("semigroups", "gap/semigroups/grpperm.gi");
ReadPackage("semigroups", "gap/semigroups/semirms.gi");
ReadPackage("semigroups", "gap/semigroups/semibipart.gi");
ReadPackage("semigroups", "gap/semigroups/semipperm.gi");
ReadPackage("semigroups", "gap/semigroups/semitrans.gi");
ReadPackage("semigroups", "gap/semigroups/semipbr.gi");
ReadPackage("semigroups", "gap/semigroups/semimaxplus.gi");
ReadPackage("semigroups", "gap/semigroups/semiringmat.gi");
ReadPackage("semigroups", "gap/semigroups/semiboolmat.gi");
ReadPackage("semigroups", "gap/semigroups/semifp.gi");
ReadPackage("semigroups", "gap/semigroups/semiex.gi");
ReadPackage("semigroups", "gap/semigroups/semicons.gi");
ReadPackage("semigroups", "gap/semigroups/semigraph.gi");
ReadPackage("semigroups", "gap/semigroups/semiffmat.gi");
ReadPackage("semigroups", "gap/semigroups/grpffmat.gi");
ReadPackage("semigroups", "gap/semigroups/semiquo.gi");

ReadPackage("semigroups", "gap/ideals/ideals.gi");
ReadPackage("semigroups", "gap/ideals/idealact.gi");
ReadPackage("semigroups", "gap/ideals/ideallam.gi");
ReadPackage("semigroups", "gap/ideals/idealenum.gi");

ReadPackage("semigroups", "gap/greens/gree.gi");
ReadPackage("semigroups", "gap/greens/gren.gi");
ReadPackage("semigroups", "gap/greens/grac.gi");
ReadPackage("semigroups", "gap/greens/gracreg.gi");
ReadPackage("semigroups", "gap/greens/gracinv.gi");

ReadPackage("semigroups", "gap/tools/display.gi");
ReadPackage("semigroups", "gap/tools/io.gi");
ReadPackage("semigroups", "gap/tools/utils.gi");
ReadPackage("semigroups", "gap/tools/enums.gi");
ReadPackage("semigroups", "gap/tools/iterators.gi");

ReadPackage("semigroups", "gap/attributes/attr.gi");
ReadPackage("semigroups", "gap/attributes/attract.gi");
ReadPackage("semigroups", "gap/attributes/attrinv.gi");
ReadPackage("semigroups", "gap/attributes/factor.gi");
ReadPackage("semigroups", "gap/attributes/isomorph.gi");
ReadPackage("semigroups", "gap/attributes/maximal.gi");
ReadPackage("semigroups", "gap/attributes/normalizer.gi");
ReadPackage("semigroups", "gap/attributes/properties.gi");
ReadPackage("semigroups", "gap/attributes/isorms.gi");

ReadPackage("semigroups", "gap/congruences/congpairs.gi");
ReadPackage("semigroups", "gap/congruences/congrms.gi");
ReadPackage("semigroups", "gap/congruences/conguniv.gi");
ReadPackage("semigroups", "gap/congruences/conginv.gi");
ReadPackage("semigroups", "gap/congruences/congsimple.gi");
ReadPackage("semigroups", "gap/congruences/congrees.gi");
ReadPackage("semigroups", "gap/congruences/cong.gi");
ReadPackage("semigroups", "gap/congruences/conglatt.gi");
ReadPackage("semigroups", "gap/congruences/congfpmon.gi");

ReadPackage("semigroups", "gap/fp/freeinverse.gi");
ReadPackage("semigroups", "gap/fp/freeband.gi");

ReadPackage("semigroups", "gap/obsolete.gi");

MakeReadOnlyGlobal("SEMIGROUPS");
