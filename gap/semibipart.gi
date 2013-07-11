

#

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> DegreeOfBipartition(Representative(s)));

#

InstallMethod(Print, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(s)
  Print(Generators(s));
end);
