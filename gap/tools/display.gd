#############################################################################
##
##  tools/display.gd
##  Copyright (C) 2013-2024                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("TikzString", [IsObject]);
DeclareOperation("TikzString", [IsObject, IsRecord]);
DeclareOperation("DotString", [IsObject]);
DeclareOperation("DotString", [IsObject, IsRecord]);
DeclareOperation("TexString", [IsObject]);
DeclareOperation("TexString", [IsObject, IsObject]);

DeclareOperation("TikzLeftCayleyDigraph", [IsSemigroup]);
DeclareOperation("TikzRightCayleyDigraph", [IsSemigroup]);

DeclareOperation("GraphvizDClasses", [IsSemigroup, IsRecord]);
DeclareOperation("GraphvizDClasses", [IsSemigroup]);

DeclareOperation("GraphvizLeftCayleyDigraph", [IsSemigroup, IsHomogeneousList]);
DeclareOperation("GraphvizLeftCayleyDigraph", [IsSemigroup]);
DeclareOperation("GraphvizRightCayleyDigraph", [IsSemigroup, IsHomogeneousList]);
DeclareOperation("GraphvizRightCayleyDigraph", [IsSemigroup]);
DeclareOperation("DotLeftCayleyDigraph", [IsSemigroup, IsHomogeneousList]);
DeclareOperation("DotLeftCayleyDigraph", [IsSemigroup]);
DeclareOperation("DotRightCayleyDigraph", [IsSemigroup, IsHomogeneousList]);
DeclareOperation("DotRightCayleyDigraph", [IsSemigroup]);

# TODO homogeneous list?
DeclareOperation("GraphvizWordGraph", [IsDigraph]);
DeclareOperation("GraphvizWordGraph", [IsDigraph, IsList, IsList]);

DeclareOperation("DotWordGraph", [IsDigraph]);
DeclareOperation("DotWordGraph", [IsDigraph, IsList, IsList]);

DeclareAttribute("GraphvizSemilatticeOfIdempotents", IsInverseSemigroup);
DeclareAttribute("DotSemilatticeOfIdempotents", IsInverseSemigroup);
