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
