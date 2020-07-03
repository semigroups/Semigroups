#############################################################################
##
##  word.gd
##  Copyright (C) 2020                                      Maria Tsalakou
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("WordToString", [IsString, IsList]);
DeclareOperation("RandomWord", [IsInt, IsInt]);
DeclareOperation("StandardiseWord", [IsList]);
DeclareSynonym("StandardizeWord", StandardiseWord);
DeclareOperation("StringToWord", [IsString]);
