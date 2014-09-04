#############################################################################
##
#W  digraph.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Some functions for directed graphs, that are represented as, for example,
# [[1,2,3], [1,3], [2]] (i.e. 1 is adjacent to 1, 2, and 3, and 2 is adjacent to
# 1 and 3). 


# in case we want to make a more elaborate data structure in the future 

DeclareSynonym("IsDirectedGraph", IsList);

DeclareOperation("DirectedGraphRelabel", [IsDirectedGraph, IsPerm]);
DeclareOperation("DirectedGraphRemoveLoops", [IsDirectedGraph]);
DeclareOperation("DirectedGraphTopologicalSort", [IsDirectedGraph]);
DeclareOperation("DirectedGraphTopologicalSort", [IsDirectedGraph, IsBool]);
DeclareOperation("DirectedGraphReflexiveTransitiveClosure", [IsDirectedGraph]);
DeclareOperation("DirectedGraphTransitiveClosure", [IsDirectedGraph]);

DeclareProperty("IsStronglyConnectedDirectedGraph", IsDirectedGraph);
DeclareProperty("IsAcyclicDirectedGraph", IsDirectedGraph);

