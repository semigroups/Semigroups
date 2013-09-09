############################################################################# 
## 
#W  ideals.gi
#Y  Copyright (C) 2013                                    James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

InstallMethod(ViewObj, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 1,
function(S)
  Print(ViewString(S));
end);

#

InstallMethod(ViewString, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(S)
  local str;
  
  str:="<ideal of ";
  Append(str, ViewString(Parent(S)));
  Append(str, " with ");
  Append(str, String(Length(GeneratorsOfSemigroupIdeal(S))));
  Append(str, " generator");
  if Length(GeneratorsOfSemigroupIdeal(S))>1 then 
    Append(str, "s");
  fi;
  Append(str, ">");
  return str;
end);

#

InstallMethod(\., "for a semigroup ideal with generators and pos int",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal, IsPosInt],
function(S, n)
  S:=GeneratorsOfSemigroupIdeal(S);
  n:=NameRNam(n);
  n:=Int(n);
  if n=fail or Length(S)<n then
    Error("usage: the second argument <n> should be a positive integer\n",
     "not greater than the number of generators of the semigroup <S> in\n", 
     "the first argument,");
    return;
  fi;
  return S[n];
end);

# a convenience, similar to the function <Semigroup>...

InstallGlobalFunction(SemigroupIdeal, 
function( arg )
  local out, i;

  if not IsSemigroup(arg[1]) then 
    Error("usage: the first argument should be a semigroup,");
    return;
  fi;

  if Length(arg)=1 then 
    Error("usage: there must be a second argument, which specifies\n",
    "the ideal you are trying to create,");
    return;
  fi;

  # special case for matrices, because they may look like lists
  if Length( arg ) = 2 and IsMatrix( arg[2] )  then
    return SemigroupIdealByGenerators(arg[1],  [arg[2]]);

  # list of generators
  elif Length(arg)=2 and IsList(arg[2]) and 0 < Length(arg[2]) then
    return SemigroupIdealByGenerators(arg[1], arg[2]);
  
  # generators and collections of generators
  elif IsAssociativeElement(arg[2]) 
   or IsAssociativeElementCollection(arg[2]) then
    out:=[];
    for i in [2..Length(arg)] do
      if IsAssociativeElement(arg[i]) then
        Add(out, arg[i]);
      elif IsAssociativeElementCollection(arg[i]) then
        if HasGeneratorsOfSemigroup(arg[i]) then
          Append(out, GeneratorsOfSemigroup(arg[i]));
        elif HasGeneratorsOfSemigroupIdeal(arg[i]) then 
          Append(out, GeneratorsOfSemigroupIdeal(arg[i]));
        elif IsList(arg[i]) then 
          Append(out, arg[i]);
        else 
          Append(out, AsList(arg[1])); #JDM should use this in Semigroup too
        fi;
      #so that we can pass the options record in the Semigroups package 
      #elif i=Length(arg[2]) and IsRecord(arg[2][i]) then
      #  return SemigroupIdealByGenerators(out, arg[2][i]);
      else
        Error( "usage: the second argument should be some\n",
        "combination of generators, lists of generators, or semigroups,");
        return;
      fi;
    od;
    return SemigroupIdealByGenerators(arg[1], out);
  # no argument given, error
  else
    Error( "usage: the second argument should be some\n",
    "combination of generators, lists of generators, or semigroups,");
    return;
  fi;
end);
#

InstallMethod(MagmaIdealByGenerators,
"for an acting semigroup and collection of its elements", 
IsIdenticalObj, 
[IsActingSemigroup, IsAssociativeElementCollection],
SemigroupIdealByGenerators);

#

IdealOfSemilattice:=function(semilattice, parents)
  local children;
  children:=Union(semilattice{parents});
  if parents=children then 
    return parents;
  fi;
  return IdealOfSemilattice(semilattice, children);
end;

#

InstallMethod(SemigroupIdealByGenerators,
"for an acting semigroup and associative element collection", 
IsIdenticalObj, 
[IsActingSemigroup, IsAssociativeElementCollection],
function( M, gens )
  local S, data;
    
  S:= Objectify( NewType( FamilyObj( gens ), IsMagmaIdeal and
   IsAttributeStoringRep and IsActingSemigroup ),
   rec(opts:=SemigroupsOptionsRec));
  
  SetGeneratorsOfMagmaIdeal( S, AsList( gens ) );
  SetIsSemigroupIdeal(S, true);
  SetParent(S, M);
  if HasPartialOrderOfDClasses(M) then 
    data:=SemigroupData(M);
    SetIdealOfDClasses(S, IdealOfSemilattice( PartialOrderOfDClasses(M), 
     List(gens, x->OrbSCCLookup(data)[Position(data, x)]-1)));
  fi;
  return S;
end );

# JDM move to lib

InstallMethod(\=, "for semigroup ideals", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal, 
 IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(I, J)
  
  if Parent(I)=Parent(J) then 
    return ForAll(GeneratorsOfMagmaIdeal(I), x-> x in J) and
    ForAll(GeneratorsOfMagmaIdeal(J), x-> x in I);
  elif HasGeneratorsOfSemigroup(I) and HasGeneratorsOfSemigroup(J) then 
    return ForAll(GeneratorsOfSemigroup(I), x-> x in J) and
     ForAll(GeneratorsOfSemigroup(J), x-> x in I); 
  else
    return AsSSortedList(I)=AsSSortedList(J);
  fi;

end);

#

InstallTrueMethod(IsSemigroupIdeal, IsMagmaIdeal and IsActingSemigroup);

#

InstallMethod(Representative, "for a semigroup ideal", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(I)
  return Representative(GeneratorsOfMagmaIdeal(I));
end);

# methods for ideals with ideals of D-classes

InstallMethod(Size, "for acting semigroup ideal with ideal of D-classes", 
[IsActingSemigroup and IsSemigroupIdeal and HasIdealOfDClasses],
function(I)
  return Sum(List(IdealOfDClasses(I), i-> Size(DClasses(Parent(I))[i])));
end);

#

InstallMethod(\in, 
"for acting semigroup ideal with ideal of D-classes and associative element",
[IsAssociativeElement, IsActingSemigroup and IsSemigroupIdeal and HasIdealOfDClasses],
function(f, I)
  local pos;
  
  pos:=Position(SemigroupData(Parent(I)), f);
  
  if pos=fail then 
    return false;
  else
    return OrbSCCLookup(SemigroupData(Parent(I)))[pos] in IdealOfDClasses(I);
  fi;

end);

#



#JDM here



