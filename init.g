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
  _PATH_SO := Filename(DirectoriesPackagePrograms("semigroups"),
                       "semigroups.so");
  if _PATH_SO <> fail then
    LoadDynamicModule(_PATH_SO);
  fi;
  Unbind(_PATH_SO);
fi;

ReadPackage("semigroups/gap/options.g");

ReadPackage("semigroups/gap/Elements/star.gd");
ReadPackage("semigroups/gap/Elements/pbr.gd");
ReadPackage("semigroups/gap/Elements/bipartition.gd");
ReadPackage("semigroups/gap/Elements/blocks.gd");
ReadPackage("semigroups/gap/Elements/semiringmat.gd");
ReadPackage("semigroups/gap/Elements/maxplusmat.gd");
ReadPackage("semigroups/gap/Elements/boolmat.gd");
ReadPackage("semigroups/gap/Elements/pfmat.gd");

ReadPackage("semigroups/gap/Semigroups/semigroups.gd");
ReadPackage("semigroups/gap/Semigroups/froidure-pin.gd");
ReadPackage("semigroups/gap/Semigroups/grpperm.gd");
ReadPackage("semigroups/gap/Semigroups/reesmat.gd");
ReadPackage("semigroups/gap/Semigroups/semibipart.gd");
ReadPackage("semigroups/gap/Semigroups/semipperm.gd");
ReadPackage("semigroups/gap/Semigroups/semitrans.gd");
ReadPackage("semigroups/gap/Semigroups/semipbr.gd");
ReadPackage("semigroups/gap/Semigroups/semimaxplus.gd");
ReadPackage("semigroups/gap/Semigroups/semiringmat.gd");
ReadPackage("semigroups/gap/Semigroups/semiboolmat.gd");
ReadPackage("semigroups/gap/Semigroups/semipfmat.gd");
ReadPackage("semigroups/gap/Semigroups/examples.gd");

ReadPackage("semigroups/gap/Acting/setup.gd");
ReadPackage("semigroups/gap/Acting/acting.gd");
ReadPackage("semigroups/gap/Acting/lambda-rho.gd");
ReadPackage("semigroups/gap/Acting/graded.gd");
ReadPackage("semigroups/gap/Acting/orbits.gd");

ReadPackage("semigroups/gap/Ideals/ideals.gd");
ReadPackage("semigroups/gap/Ideals/ideals-acting.gd");
ReadPackage("semigroups/gap/Ideals/ideals-lambda-rho.gd");

ReadPackage("semigroups/gap/Greens/greens-generic.gd");
ReadPackage("semigroups/gap/Greens/greens-acting.gd");

ReadPackage("semigroups/gap/Tools/display.gd");
ReadPackage("semigroups/gap/Tools/io.gd");
ReadPackage("semigroups/gap/Tools/utils.gd");
ReadPackage("semigroups/gap/Tools/enums.gd");
ReadPackage("semigroups/gap/Tools/iterators.gd");

ReadPackage("semigroups/gap/Attributes/attributes.gd");
ReadPackage("semigroups/gap/Attributes/attributes-acting.gd");
ReadPackage("semigroups/gap/Attributes/attributes-inverse.gd");
ReadPackage("semigroups/gap/Attributes/factor.gd");
ReadPackage("semigroups/gap/Attributes/isomorph.gd");
ReadPackage("semigroups/gap/Attributes/maximal.gd");
ReadPackage("semigroups/gap/Attributes/normalizer.gd");
ReadPackage("semigroups/gap/Attributes/properties.gd");
ReadPackage("semigroups/gap/Attributes/reesmat-iso.gd");

ReadPackage("semigroups/gap/Congruences/pairs-cong.gd");
ReadPackage("semigroups/gap/Congruences/reesmat-cong.gd");
ReadPackage("semigroups/gap/Congruences/univ-cong.gd");
ReadPackage("semigroups/gap/Congruences/inverse-cong.gd");
ReadPackage("semigroups/gap/Congruences/simple-cong.gd");
ReadPackage("semigroups/gap/Congruences/rees-cong.gd");
ReadPackage("semigroups/gap/Congruences/quotients.gd");

ReadPackage("semigroups/gap/Fp/fpsemi.gd");
ReadPackage("semigroups/gap/Fp/freeinverse.gd");
ReadPackage("semigroups/gap/Fp/freeband.gd");

DeclareInfoClass("InfoSemigroups");
