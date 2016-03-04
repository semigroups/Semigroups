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
  local digraph;

  digraph := AsDigraph(f);
  return AsTransformation(OnDigraphs(digraph,
                                     DigraphCanonicalLabelling(digraph)));
end);

InstallMethod(TransformationByImageAndKernel, "for an image and partition",
[IsHomogeneousList, IsCyclotomicCollColl],
function(im, ker)
  local flat, i;

  if not ForAll(ker, class -> ForAll(class, IsPosInt)) then
    ErrorNoReturn();
  fi;

  flat := Union(ker);

  if flat <> [1 .. Length(flat)] then
    ErrorNoReturn();
  fi;

  for i in [1 .. Length(ker)] do
    flat{ker[i]} := [1 .. Length(ker[i])] * 0 + i;
  od;
  return TransformationByImageAndKernel(im, flat);
end);
