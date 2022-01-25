#############################################################################
##
##  fp/word.gd
##  Copyright (C) 2020-2022                                 Maria Tsalakou
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("WordToString", [IsString, IsList]);
DeclareOperation("RandomWord", [IsInt, IsInt]);
DeclareOperation("StandardiseWord", [IsHomogeneousList]);
DeclareSynonym("StandardizeWord", StandardiseWord);
DeclareOperation("StringToWord", [IsString]);
