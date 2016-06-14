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
fi;

# The following are for GAP internal objects of type T_SEMI
BindGlobal("TSemiObjFamily", NewFamily("TSemiObjFamily"));
DeclareCategory("IsTSemiObj", IsObject);
BindGlobal("TheTypeTSemiObj", NewType(TSemiObjFamily, IsTSemiObj));

# the kernel module makes use of the c functions HTAdd_TreeHash_C and
# HTValue_TreeHash_C and so we should only use the part of the kernel module
# using these functions if Orb is compiled.
_SEMIGROUPS_SO := Filename(DirectoriesPackagePrograms("semigroups"),
                           "semigroups.so");
if _SEMIGROUPS_SO <> fail then
  LoadDynamicModule(_SEMIGROUPS_SO);
fi;
Unbind(_SEMIGROUPS_SO);


if not IsBound(UserHomeExpand) then
  BindGlobal("UserHomeExpand", USER_HOME_EXPAND);
fi;

BindGlobal("SEMIGROUPS", rec());
MakeReadWriteGlobal("SEMIGROUPS");
SEMIGROUPS.GENERATORS := rec();
SEMIGROUPS.PackageDir := PackageInfo("semigroups")[1]!.InstallationPath;

ReadPackage("semigroups/gap/options.g");

ReadPackage("semigroups/gap/elements/star.gd");
ReadPackage("semigroups/gap/elements/pbr.gd");
ReadPackage("semigroups/gap/elements/bipart.gd");
ReadPackage("semigroups/gap/elements/blocks.gd");
ReadPackage("semigroups/gap/elements/semiringmat.gd");
ReadPackage("semigroups/gap/elements/maxplusmat.gd");
ReadPackage("semigroups/gap/elements/boolmat.gd");
ReadPackage("semigroups/gap/elements/trans.gd");

ReadPackage("semigroups/gap/semigroups/semi.gd");
ReadPackage("semigroups/gap/semigroups/grpperm.gd");
ReadPackage("semigroups/gap/semigroups/semirms.gd");
ReadPackage("semigroups/gap/semigroups/semibipart.gd");
ReadPackage("semigroups/gap/semigroups/semipperm.gd");
ReadPackage("semigroups/gap/semigroups/semitrans.gd");
ReadPackage("semigroups/gap/semigroups/semipbr.gd");
ReadPackage("semigroups/gap/semigroups/semimaxplus.gd");
ReadPackage("semigroups/gap/semigroups/semiringmat.gd");
ReadPackage("semigroups/gap/semigroups/semiboolmat.gd");
ReadPackage("semigroups/gap/semigroups/semiex.gd");
ReadPackage("semigroups/gap/semigroups/semicons.gd");
ReadPackage("semigroups/gap/semigroups/semigraph.gd");
ReadPackage("semigroups/gap/semigroups/semifp.gd");

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
ReadPackage("semigroups/gap/attributes/translat.gd");

ReadPackage("semigroups/gap/congruences/congpairs.gd");
ReadPackage("semigroups/gap/congruences/congrms.gd");
ReadPackage("semigroups/gap/congruences/conguniv.gd");
ReadPackage("semigroups/gap/congruences/conginv.gd");
ReadPackage("semigroups/gap/congruences/congsimple.gd");
ReadPackage("semigroups/gap/congruences/congrees.gd");
ReadPackage("semigroups/gap/congruences/quotients.gd");
ReadPackage("semigroups/gap/congruences/congruences.gd");
ReadPackage("semigroups/gap/congruences/conglattice.gd");

ReadPackage("semigroups/gap/fp/fpsemi.gd");
ReadPackage("semigroups/gap/fp/freeinverse.gd");
ReadPackage("semigroups/gap/fp/freeband.gd");

ReadPackage("semigroups/gap/obsolete.gd");

DeclareInfoClass("InfoSemigroups");
