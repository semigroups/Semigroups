############################################################################# 
## 
#W  semibipart.gi 
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

InstallMethod(IsBipartitionSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x-> IsBipartitionSemigroup(Parent(x)));

InstallImmediateMethod(GeneratorsOfSemigroup, IsBipartitionCollection and IsSemigroup and HasGeneratorsOfInverseSemigroup, 0, 
function(s)
  local gens, f;

  gens:=ShallowCopy(GeneratorsOfInverseSemigroup(s));
  for f in gens do 
    if not IsPermBipartition(f) then 
      f:=f^-1;
      if not f in gens then 
        Add(gens, f);
      fi;
    fi;
  od;
  MakeImmutable(gens);
  return gens;
end);

#

InstallMethod(ViewString, "for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup], 8, # to trump IsGroup
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
  if not IsGroup(s) then 
    if (HasIsTrivial(s) and IsTrivial(s)) or IsGroup(s) then 
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
  fi;
  
  Append(str, "\>bipartition\< ");
 
  if IsGroup(s) then 
    Append(str, "\>group\< ");
    nrgens:=Length(GeneratorsOfGroup(s));
  elif HasIsMonoid(s) and IsMonoid(s) then 
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

#
