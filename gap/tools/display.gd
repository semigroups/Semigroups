#############################################################################
##
##  tools/display.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
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

DeclareOperation("DotLeftCayleyDigraph", [IsSemigroup]);
DeclareOperation("DotRightCayleyDigraph", [IsSemigroup]);

DeclareAttribute("DotSemilatticeOfIdempotents", IsInverseSemigroup);
