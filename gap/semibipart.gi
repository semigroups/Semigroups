


#

InstallMethod(ViewString, "for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local str, nrgens;
  
  str:="\><";

  if HasIsTrivial(s) and IsTrivial(s) then 
    Append(str, "\>trivial\< ");
  else 
    if HasIsCommutative(s) and IsCommutative(s) then 
      Append(str, "\>commutative\< ");
    fi;
  fi;

  if HasIsTrivial(s) and IsTrivial(s) then 
  elif HasIsZeroSimpleSemigroup(s) and IsZeroSimpleSemigroup(s) then 
    Append(str, "\>0-simple\< ");
  elif HasIsSimpleSemigroup(s) and IsSimpleSemigroup(s) then 
    Append(str, "\>simple\< ");
  fi;

  if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    Append(str, "\>inverse\< ");
  elif HasIsRegularSemigroup(s) 
   and not (HasIsSimpleSemigroup(s) and IsSimpleSemigroup(s)) then 
    if IsRegularSemigroup(s) then 
      Append(str, "\>regular\< ");
    else
      Append(str, "\>non-regular\< ");
    fi;
  fi;
  
  Append(str, "\>bipartition\< ");
 
  if HasIsMonoid(s) and IsMonoid(s) then 
    Append(str, "\>monoid\< ");
    nrgens:=Length(GeneratorsOfMonoid(s));
  else 
    Append(str, "\>semigroup\< ");
    nrgens:=Length(GeneratorsOfSemigroup(s));
  fi;
  
  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s) and Size(s)<2^64 then 
    Append(str, "\>of size\> ");
    Append(str, String(Size(s)));
    Append(str, ",\<\< ");
  fi;
 
  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfBipartitionSemigroup(s)));
  Append(str, "\< pts with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens>1 or nrgens=0 then 
    Append(str, "s\<");
  else 
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(DegreeOfBipartitionSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> DegreeOfBipartition(Representative(s)));

#

InstallMethod(PrintObj, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(s)
  Print(Generators(s));
end);
