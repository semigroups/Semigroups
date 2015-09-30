#############################################################################
##
#W  init.g
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

if not IsBound(ORBC) then
  BindGlobal("HTAdd_TreeHash_C", fail);
  BindGlobal("HTValue_TreeHash_C", fail);
else # only do this if ORBC is available
# FIXME WTF: only load our kernel module when orb is available!!!
  _PATH_SO := Filename(DirectoriesPackagePrograms("semigroups"),
                       "semigroups.so");
  if _PATH_SO <> fail then
    LoadDynamicModule(_PATH_SO);
  fi;
  Unbind(_PATH_SO);
fi;

ReadPackage("semigroups/gap/options.g");

ReadPackage("semigroups/gap/elements/star.gd");
ReadPackage("semigroups/gap/elements/pbr.gd");
ReadPackage("semigroups/gap/elements/bipartition.gd");
ReadPackage("semigroups/gap/elements/blocks.gd");
ReadPackage("semigroups/gap/elements/semiringmat.gd");
ReadPackage("semigroups/gap/elements/maxplusmat.gd");
ReadPackage("semigroups/gap/elements/boolmat.gd");
ReadPackage("semigroups/gap/elements/pfmat.gd");

ReadPackage("semigroups/gap/semigroups/semigroups.gd");
ReadPackage("semigroups/gap/semigroups/grpperm.gd");
ReadPackage("semigroups/gap/semigroups/reesmat.gd");
ReadPackage("semigroups/gap/semigroups/semibipart.gd");
ReadPackage("semigroups/gap/semigroups/semipperm.gd");
ReadPackage("semigroups/gap/semigroups/semitrans.gd");
ReadPackage("semigroups/gap/semigroups/semipbr.gd");
ReadPackage("semigroups/gap/semigroups/semimaxplus.gd");
ReadPackage("semigroups/gap/semigroups/semiringmat.gd");
ReadPackage("semigroups/gap/semigroups/semiboolmat.gd");
ReadPackage("semigroups/gap/semigroups/semipfmat.gd");
ReadPackage("semigroups/gap/semigroups/examples.gd");

ReadPackage("semigroups/gap/main/froidure-pin.gd");
ReadPackage("semigroups/gap/main/setup.gd");
ReadPackage("semigroups/gap/main/acting.gd");
ReadPackage("semigroups/gap/main/lambda-rho.gd");
ReadPackage("semigroups/gap/main/graded.gd");
ReadPackage("semigroups/gap/main/orbits.gd");

ReadPackage("semigroups/gap/ideals/ideals.gd");
ReadPackage("semigroups/gap/ideals/ideals-acting.gd");
ReadPackage("semigroups/gap/ideals/ideals-lambda-rho.gd");

ReadPackage("semigroups/gap/greens/greens-generic.gd");
ReadPackage("semigroups/gap/greens/greens-acting.gd");

ReadPackage("semigroups/gap/tools/display.gd");
ReadPackage("semigroups/gap/tools/io.gd");
ReadPackage("semigroups/gap/tools/utils.gd");
ReadPackage("semigroups/gap/tools/enums.gd");
ReadPackage("semigroups/gap/tools/iterators.gd");

ReadPackage("semigroups/gap/attributes/attributes.gd");
ReadPackage("semigroups/gap/attributes/attributes-acting.gd");
ReadPackage("semigroups/gap/attributes/attributes-inverse.gd");
ReadPackage("semigroups/gap/attributes/factor.gd");
ReadPackage("semigroups/gap/attributes/isomorph.gd");
ReadPackage("semigroups/gap/attributes/maximal.gd");
ReadPackage("semigroups/gap/attributes/normalizer.gd");
ReadPackage("semigroups/gap/attributes/properties.gd");
ReadPackage("semigroups/gap/attributes/reesmat-iso.gd");

ReadPackage("semigroups/gap/congruences/pairs.gd");
ReadPackage("semigroups/gap/congruences/reesmat.gd");
ReadPackage("semigroups/gap/congruences/univ.gd");
ReadPackage("semigroups/gap/congruences/inverse.gd");
ReadPackage("semigroups/gap/congruences/simple.gd");
ReadPackage("semigroups/gap/congruences/rees.gd");
ReadPackage("semigroups/gap/congruences/quotients.gd");
ReadPackage("semigroups/gap/congruences/semilattice.gd");
ReadPackage("semigroups/gap/congruences/congruences.gd");

ReadPackage("semigroups/gap/fp/fpsemi.gd");
ReadPackage("semigroups/gap/fp/freeinverse.gd");
ReadPackage("semigroups/gap/fp/freeband.gd");

DeclareInfoClass("InfoSemigroups");
