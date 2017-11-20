############################################################################
##
#W  trans.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

InstallMethod(CanonicalTransformation, "for a transformation",
[IsTransformation],
function(f)
  return CanonicalTransformation(f, DegreeOfTransformation(f));
end);

InstallMethod(CanonicalTransformation, "for a transformation",
[IsTransformation, IsInt],
function(f, n)
  local digraph;
  if n < DegreeOfTransformation(f) then
    ErrorNoReturn("Semigroups: CanonicalTransformation: usage,\n",
                  "the second argument (an integer) must be at least ",
                  "the degree of the first argument (a transformation),");
  fi;

  digraph := AsDigraph(f, n);
  return AsTransformation(OnDigraphs(digraph,
                                     BlissCanonicalLabelling(digraph)));
end);

InstallMethod(TransformationByImageAndKernel, "for an image and partition",
[IsHomogeneousList, IsCyclotomicCollColl],
function(im, ker)
  local flat, i;

  if not ForAll(ker, class -> ForAll(class, IsPosInt)) then
    ErrorNoReturn("Semigroups: TransformationByImageAndKernel: usage,\n",
                  "the argument must be a list of lists of pos ints,");
  fi;

  flat := Union(ker);

  if flat <> [1 .. Length(flat)] then
    ErrorNoReturn("Semigroups: TransformationByImageAndKernel: usage,\n",
                  "the union of the second argument (a partition) must ",
                  "be [1 .. ", Length(flat), "],");
  fi;

  for i in [1 .. Length(ker)] do
    flat{ker[i]} := [1 .. Length(ker[i])] * 0 + i;
  od;
  return TransformationByImageAndKernel(im, flat);
end);

InstallMethod(IndexPeriodOfSemigroupElement, "for a transformation",
[IsTransformation], IndexPeriodOfTransformation);
