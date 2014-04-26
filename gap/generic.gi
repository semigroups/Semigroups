
# code for generic semigroups, ties in the Froidure-Pin Semigroupe algorithm to
# GAP methods

InstallMethod(Size, "for a finite semigroup with generators", 
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(Enumerate(S, infinity, ReturnFalse).elts)-1;
end);


