#############################################################################
##
#W  read.g
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

BindGlobal("IsGrapeLoaded", IsPackageMarkedForLoading("grape", "4.5"));
BindGlobal("IsGrapeCompiled",
           ExternalFilename(DirectoriesPackagePrograms("grape"), "dreadnautB")
           <> fail);

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
             Concatenation("the nauty/dreadnaut binaries for the GRAPE ",
                           "package are not loaded\n#I  and so this function ",
                           "does not work"));
fi;

if TestPackageAvailability("genss") = fail then
  Add(SemigroupsOmitFromTestManualExamples, "Normalizer");
fi;

# Issue 5 for Orb:

if not IsBound(MappingPermListList_C) then
  BindGlobal("MappingPermListList_C",
  function(src, dst)
    local src_tab, dst_tab, d, out, next, i;

    if not IsList(src) or not IsList(dst) or Length(src) <> Length(dst)  then
       Error("Semigroups: MappingPermListList_C: usage,\n",
             "the arguments must be lists of equal length,");
       return;
    fi;

    if IsEmpty(src)  then
      return ();
    fi;
    src_tab := [];
    dst_tab := [];
    d := Maximum(Maximum(src), Maximum(dst));
    for i in [1 .. Length(src)] do
      src_tab[src[i]] := i;
    od;
    for i in [1 .. Length(dst)] do
      dst_tab[dst[i]] := i;
    od;
    out := EmptyPlist(d);
    next := 1;
    for i in [1 .. d] do
      if IsBound(src_tab[i]) then
        out[i] := dst[src_tab[i]];
      else
        while IsBound(dst_tab[next]) do
          next := next + 1;
        od;
        out[i] := next;
        next := next + 1;
      fi;
    od;

    return PermList(out);
  end);
fi;

ReadPackage("semigroups/gap/elements/star.gi");
ReadPackage("semigroups/gap/elements/pbr.gi");
ReadPackage("semigroups/gap/elements/bipartition.gi");
ReadPackage("semigroups/gap/elements/blocks.gi");
ReadPackage("semigroups/gap/elements/semiringmat.gi");
ReadPackage("semigroups/gap/elements/maxplusmat.gi");
ReadPackage("semigroups/gap/elements/boolmat.gi");
ReadPackage("semigroups/gap/elements/pfmat.gi");

ReadPackage("semigroups/gap/semigroups/semigroups.gi");
ReadPackage("semigroups/gap/semigroups/grpperm.gi");
ReadPackage("semigroups/gap/semigroups/reesmat.gi");
ReadPackage("semigroups/gap/semigroups/semibipart.gi");
ReadPackage("semigroups/gap/semigroups/semipperm.gi");
ReadPackage("semigroups/gap/semigroups/semitrans.gi");
ReadPackage("semigroups/gap/semigroups/semipbr.gi");
ReadPackage("semigroups/gap/semigroups/semimaxplus.gi");
ReadPackage("semigroups/gap/semigroups/semiringmat.gi");
ReadPackage("semigroups/gap/semigroups/semiboolmat.gi");
ReadPackage("semigroups/gap/semigroups/semipfmat.gi");
ReadPackage("semigroups/gap/semigroups/examples.gi");

ReadPackage("semigroups/gap/main/froidure-pin.gi");
ReadPackage("semigroups/gap/main/setup.gi");
ReadPackage("semigroups/gap/main/acting.gi");
ReadPackage("semigroups/gap/main/lambda-rho.gi");
ReadPackage("semigroups/gap/main/graded.gi");
ReadPackage("semigroups/gap/main/orbits.gi");
ReadPackage("semigroups/gap/main/semigroups-acting.gi");

ReadPackage("semigroups/gap/ideals/ideals.gi");
ReadPackage("semigroups/gap/ideals/ideals-acting.gi");
ReadPackage("semigroups/gap/ideals/ideals-lambda-rho.gi");
ReadPackage("semigroups/gap/ideals/ideals-generic.gi");


ReadPackage("semigroups/gap/greens/greens-generic.gi");
ReadPackage("semigroups/gap/greens/greens-acting.gi");
ReadPackage("semigroups/gap/greens/greens-regular.gi");
ReadPackage("semigroups/gap/greens/greens-inverse.gi");

ReadPackage("semigroups/gap/tools/display.gi");
ReadPackage("semigroups/gap/tools/io.gi");
ReadPackage("semigroups/gap/tools/utils.gi");
ReadPackage("semigroups/gap/tools/enums.gi");
ReadPackage("semigroups/gap/tools/iterators.gi");

ReadPackage("semigroups/gap/attributes/attributes.gi");
ReadPackage("semigroups/gap/attributes/attributes-acting.gi");
ReadPackage("semigroups/gap/attributes/attributes-inverse.gi");
ReadPackage("semigroups/gap/attributes/factor.gi");
ReadPackage("semigroups/gap/attributes/isomorph.gi");
ReadPackage("semigroups/gap/attributes/maximal.gi");
ReadPackage("semigroups/gap/attributes/normalizer.gi");
ReadPackage("semigroups/gap/attributes/properties.gi");
ReadPackage("semigroups/gap/attributes/reesmat-iso.gi");

ReadPackage("semigroups/gap/congruences/pairs-cong.gi");
ReadPackage("semigroups/gap/congruences/reesmat-cong.gi");
ReadPackage("semigroups/gap/congruences/univ-cong.gi");
ReadPackage("semigroups/gap/congruences/inverse-cong.gi");
ReadPackage("semigroups/gap/congruences/simple-cong.gi");
ReadPackage("semigroups/gap/congruences/rees-cong.gi");
ReadPackage("semigroups/gap/congruences/quotients.gi");

ReadPackage("semigroups/gap/fp/fpsemi.gi");
ReadPackage("semigroups/gap/fp/freeinverse.gi");
ReadPackage("semigroups/gap/fp/freeband.gi");
