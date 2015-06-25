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

  _PATH_SO:=Filename(DirectoriesPackagePrograms("semigroups"), "semigroups.so");
  if _PATH_SO <> fail then
    LoadDynamicModule(_PATH_SO);
  fi;
  Unbind(_PATH_SO);

fi;

ReadPackage("semigroups/gap/grpperm.gd");
ReadPackage("semigroups/gap/fpsemi.gd");

ReadPackage("semigroups/gap/utils.gd");
ReadPackage("semigroups/gap/io.gd");
ReadPackage("semigroups/gap/options.g");

ReadPackage("semigroups/gap/setup.gd");
ReadPackage("semigroups/gap/acting.gd");
ReadPackage("semigroups/gap/ideals-acting.gd");
ReadPackage("semigroups/gap/ideals-generic.gd");
ReadPackage("semigroups/gap/semigroups.gd");
ReadPackage("semigroups/gap/semigroups-acting.gd");

ReadPackage("semigroups/gap/pbr.gd");

ReadPackage("semigroups/gap/matrix-semiring.gd");
ReadPackage("semigroups/gap/matrix-max-plus.gd");
ReadPackage("semigroups/gap/matrix-boolean.gd");
ReadPackage("semigroups/gap/matrix-prime-field.gd");

ReadPackage("semigroups/gap/bipartition.gd");
ReadPackage("semigroups/gap/blocks.gd");

ReadPackage("semigroups/gap/semigroups-matrix-semiring.gd");
ReadPackage("semigroups/gap/semigroups-matrix-max-plus.gd");
ReadPackage("semigroups/gap/semigroups-matrix-boolean.gd");
ReadPackage("semigroups/gap/semigroups-matrix-prime-field.gd");

ReadPackage("semigroups/gap/semibipart.gd");
ReadPackage("semigroups/gap/semipbr.gd");
ReadPackage("semigroups/gap/semitrans.gd");
ReadPackage("semigroups/gap/semipperm.gd");

ReadPackage("semigroups/gap/greens-generic.gd");
ReadPackage("semigroups/gap/greens-acting.gd");
ReadPackage("semigroups/gap/lambda-rho.gd");
ReadPackage("semigroups/gap/ideals-lambda-rho.gd");
ReadPackage("semigroups/gap/orbits.gd");
ReadPackage("semigroups/gap/graded.gd");
ReadPackage("semigroups/gap/enums.gd");
ReadPackage("semigroups/gap/iterators.gd");
ReadPackage("semigroups/gap/properties.gd");
ReadPackage("semigroups/gap/attributes.gd");
ReadPackage("semigroups/gap/attributes-acting.gd");
ReadPackage("semigroups/gap/attributes-inverse.gd");
ReadPackage("semigroups/gap/factor.gd");
ReadPackage("semigroups/gap/examples.gd");

ReadPackage("semigroups/gap/ideals.gd");

ReadPackage("semigroups/gap/freeinverse.gd");
ReadPackage("semigroups/gap/freeband.gd");

ReadPackage("semigroups/gap/display.gd");

ReadPackage("semigroups/gap/isomorph.gd");
ReadPackage("semigroups/gap/reesmat.gd");
ReadPackage("semigroups/gap/reesmat-iso.gd");
ReadPackage("semigroups/gap/maximal.gd");
ReadPackage("semigroups/gap/normalizer.gd");

ReadPackage("semigroups/gap/quotients.gd");

ReadPackage("semigroups/gap/cong-pairs.gd");
ReadPackage("semigroups/gap/cong-reesmat.gd");
ReadPackage("semigroups/gap/cong-univ.gd");
ReadPackage("semigroups/gap/cong-inverse.gd");
ReadPackage("semigroups/gap/cong-simple.gd");
ReadPackage("semigroups/gap/cong-rees.gd");
ReadPackage("semigroups/gap/cong-semilattice.gd");
ReadPackage("semigroups/gap/congruences.gd");

ReadPackage("semigroups/gap/semigroups-generic.gd");

DeclareInfoClass("InfoSemigroups");
