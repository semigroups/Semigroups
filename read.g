#############################################################################
##
#W  read.g
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

ReadPackage("semigroups/gap/setup.gi");
ReadPackage("semigroups/gap/lambda-rho.gi");
ReadPackage("semigroups/gap/acting.gi");
ReadPackage("semigroups/gap/graded.gi");
ReadPackage("semigroups/gap/semigroups.gi");
ReadPackage("semigroups/gap/greens.gi");
ReadPackage("semigroups/gap/factor.gi");
ReadPackage("semigroups/gap/regular.gi");
ReadPackage("semigroups/gap/inverse.gi");
ReadPackage("semigroups/gap/enums.gi");
ReadPackage("semigroups/gap/iterators.gi");
ReadPackage("semigroups/gap/properties.gi");
ReadPackage("semigroups/gap/attributes.gi");
ReadPackage("semigroups/gap/attributes-inverse.gi");
ReadPackage("semigroups/gap/examples.gi");
ReadPackage("semigroups/gap/pictures.gi");

ReadPackage("semigroups/gap/ideals.gi");

ReadPackage("semigroups/gap/freeinverse.gi");

ReadPackage("semigroups/gap/orbits.gi");
ReadPackage("semigroups/gap/utils.gi");

ReadPackage("semigroups/gap/display.gi");

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
