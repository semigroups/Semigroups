#############################################################################
##
#W  read.g
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# deal with GRAPE being loaded or not, compiled or not
BindGlobal("IsGrapeLoaded", IsPackageMarkedForLoading("grape", "4.5"));
BindGlobal("IsGrapeCompiled",
ExternalFilename(DirectoriesPackagePrograms("grape"), "dreadnautB")<>fail);

if not IsGrapeLoaded then
  Add(SemigroupsOmitFromTestManualExamples, "SmallestMultiplicationTable");
  BindGlobal("GrapeIsNotLoadedString",
  Concatenation("the GRAPE package is not loaded and",
  " so this function does not work"));
fi;


if not IsGrapeCompiled then
  Add(SemigroupsOmitFromTestManualExamples, "MaximalSubsemigroups");
  Add(SemigroupsOmitFromTestManualExamples, "MunnSemigroup");
  Add(SemigroupsOmitFromTestManualExamples, "IsIsomorphicSemigroup");
  Add(SemigroupsOmitFromTestManualExamples, "IsomorphismSemigroups");
  Add(SemigroupsOmitFromTestManualExamples, "RZMSInducedFunction");
  Add(SemigroupsOmitFromTestManualExamples, "RZMStoRZMSInducedFunction");
  BindGlobal("GrapeIsNotCompiledString",
  Concatenation("the nauty/dreadnaut binaries for the GRAPE package are",
  " not loaded\n#I  and so this function does not work"));
fi;

#

if TestPackageAvailability("genss")=fail then
  Add(SemigroupsOmitFromTestManualExamples, "Normalizer");
fi;

# Issue 5 for Orb:

if not IsBound( MappingPermListList_C ) then
  BIND_GLOBAL( "MappingPermListList_C", function( src, dst )
    local src_tab, dst_tab, d, out, next, i;

    if not IsList(src) or not IsList(dst) or Length(src) <> Length(dst)  then
       Error("usage: MappingPermListList( <lst1>, <lst2> )");
    fi;

    if IsEmpty( src )  then
      return ();
    fi;
    src_tab:=[];
    dst_tab:=[];
    d:=Maximum(Maximum(src), Maximum(dst));
    for i in [1..Length(src)] do
      src_tab[src[i]]:=i;
    od;
    for i in [1..Length(dst)] do
      dst_tab[dst[i]]:=i;
    od;
    out:=EmptyPlist(d);
    next:=1;
    for i in [1..d] do
      if IsBound(src_tab[i]) then
        out[i]:=dst[src_tab[i]];
      else
        while IsBound(dst_tab[next]) do
          next:=next+1;
        od;
        out[i]:=next;
        next:=next+1;
      fi;
    od;

    return PermList(out);
  end);
fi;

# skip examples including partitions if we're in version less than 2.0

if not CompareVersionNumbers(GAPInfo.PackagesInfo.semigroups[1].Version, "2.0")
 then
  Add(SemigroupsOmitFromTestManualExamples, "partition");
  Add(SemigroupsOmitFromTestManualExamples, "Partition");
fi;

#

ReadPackage("semigroups/gap/grpperm.gi");

ReadPackage("semigroups/gap/bipartition.gi");
ReadPackage("semigroups/gap/semibipart.gi");
ReadPackage("semigroups/gap/semitrans.gi");
ReadPackage("semigroups/gap/semipperm.gi");
ReadPackage("semigroups/gap/blocks.gi");

ReadPackage("semigroups/gap/setup.gi");
ReadPackage("semigroups/gap/lambda-rho.gi");
ReadPackage("semigroups/gap/ideals-lambda-rho.gi");
ReadPackage("semigroups/gap/acting.gi");
ReadPackage("semigroups/gap/ideals-acting.gi");
ReadPackage("semigroups/gap/graded.gi");
ReadPackage("semigroups/gap/semigroups.gi");
ReadPackage("semigroups/gap/greens.gi");
ReadPackage("semigroups/gap/ideals-greens.gi");
ReadPackage("semigroups/gap/factor.gi");
ReadPackage("semigroups/gap/regular.gi");
ReadPackage("semigroups/gap/inverse.gi");
ReadPackage("semigroups/gap/enums.gi");
ReadPackage("semigroups/gap/iterators.gi");
ReadPackage("semigroups/gap/properties.gi");
ReadPackage("semigroups/gap/attributes.gi");
ReadPackage("semigroups/gap/examples.gi");
ReadPackage("semigroups/gap/orbits.gi");

ReadPackage("semigroups/gap/attributes-inverse.gi");

ReadPackage("semigroups/gap/ideals.gi");

ReadPackage("semigroups/gap/freeinverse.gi");
ReadPackage("semigroups/gap/freeband.gi");

ReadPackage("semigroups/gap/utils.gi");
ReadPackage("semigroups/gap/io.gi");

ReadPackage("semigroups/gap/display.gi");

ReadPackage("semigroups/gap/fpsemi.gi");
ReadPackage("semigroups/gap/isomorph.gi");
ReadPackage("semigroups/gap/reesmat.gi");
ReadPackage("semigroups/gap/reesmat-iso.gi");
ReadPackage("semigroups/gap/maximal.gi");
ReadPackage("semigroups/gap/normalizer.gi");

ReadPackage("semigroups/gap/quotients.gi");

ReadPackage("semigroups/gap/pairs-cong.gi");
ReadPackage("semigroups/gap/reesmat-cong.gi");
ReadPackage("semigroups/gap/univ-cong.gi");
ReadPackage("semigroups/gap/inverse-cong.gi");
ReadPackage("semigroups/gap/simple-cong.gi");
