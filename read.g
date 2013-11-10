#############################################################################
##
#W  read.g
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPackage("semigroups/gap/bipartitions/bipartition.gi");
ReadPackage("semigroups/gap/bipartitions/semibipart.gi");
ReadPackage("semigroups/gap/bipartitions/blocks.gi");

ReadPackage("semigroups/gap/semigroups-acting/setup.gi");
ReadPackage("semigroups/gap/semigroups-acting/lambda-rho.gi");
ReadPackage("semigroups/gap/semigroups-acting/acting.gi");
ReadPackage("semigroups/gap/semigroups-acting/graded.gi");
ReadPackage("semigroups/gap/semigroups-acting/semigroups.gi");
ReadPackage("semigroups/gap/semigroups-acting/greens.gi");
ReadPackage("semigroups/gap/semigroups-acting/factor.gi");
ReadPackage("semigroups/gap/semigroups-acting/regular.gi");
ReadPackage("semigroups/gap/semigroups-acting/inverse.gi");
ReadPackage("semigroups/gap/semigroups-acting/enums.gi");
ReadPackage("semigroups/gap/semigroups-acting/iterators.gi");
ReadPackage("semigroups/gap/semigroups-acting/properties.gi");
ReadPackage("semigroups/gap/semigroups-acting/attributes.gi");
ReadPackage("semigroups/gap/semigroups-acting/examples.gi");
ReadPackage("semigroups/gap/semigroups-acting/orbits.gi");

ReadPackage("semigroups/gap/inverse-semigroups-acting/attributes-inverse.gi");

ReadPackage("semigroups/gap/ideals-acting/ideals.gi");

ReadPackage("semigroups/gap/inverse-semigroups-general/freeinverse.gi");

ReadPackage("semigroups/gap/utils.gi");

ReadPackage("semigroups/gap/semigroups-general/display.gi");

if TestPackageAvailability("grape")=fail then 
  Add(SemigroupsOmitFromTestManualExamples, "MaximalSubsemigroups");
elif ExternalFilename(DirectoriesPackagePrograms("grape"), "dreadnautB")=fail
   then 
  Add(SemigroupsOmitFromTestManualExamples, "MunnSemigroup");
fi;

if not CompareVersionNumbers(GAPInfo.PackagesInfo.semigroups[1].Version, "2.0")
 then 
  Add(SemigroupsOmitFromTestManualExamples, "partition");
  Add(SemigroupsOmitFromTestManualExamples, "Partition");
fi;
