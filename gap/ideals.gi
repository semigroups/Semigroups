############################################################################# 
## 
#W  ideals.gi
#Y  Copyright (C) 2013-14                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

InstallImmediateMethod(IsSemigroupIdeal, IsSemigroup, 0, IsMagmaIdeal);
InstallTrueMethod(IsSemigroupIdeal, IsMagmaIdeal and IsSemigroup);

#

InstallMethod(PrintObj, 
"for a semigroup ideal with ideal generators", 
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I) 
  Print(PrintString(I));
end);

#

InstallMethod(PrintString, 
"for a semigroup ideal with ideal generators", 
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I) 
  local str;

  str:="SemigroupIdeal(";
  Append(str, PrintString(SupersemigroupOfIdeal(I)));
  Append(str, ", ");
  Append(str, PrintString(GeneratorsOfSemigroupIdeal(I)));
  Append(str, " )");
  return str;
end);

# JDM this is required since there is a method for ViewObj of a semigroup ideal
# with a higher rank than the default method which delegates from ViewObj to
# ViewString. Hence the method for ViewString is never invoked without the
# method below.

InstallMethod(ViewObj, 
"for a semigroup ideal with ideal generators", 
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 1,
function(I) 
  Print(ViewString(I));
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

# JDM move to lib?

InstallMethod(\=, "for semigroup ideals", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal, 
 IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(I, J)
  
  if SupersemigroupOfIdeal(I)=SupersemigroupOfIdeal(J) then 
    return ForAll(GeneratorsOfMagmaIdeal(I), x-> x in J) and
    ForAll(GeneratorsOfMagmaIdeal(J), x-> x in I);
  elif HasGeneratorsOfSemigroup(I) and HasGeneratorsOfSemigroup(J) then 
    return ForAll(GeneratorsOfSemigroup(I), x-> x in J) and
     ForAll(GeneratorsOfSemigroup(J), x-> x in I); 
  else #JDM: a better way??
    return AsSSortedList(I)=AsSSortedList(J);
  fi;

end);

#

InstallMethod(\=, "for a semigroup ideal and semigroup with generators", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal, 
 IsSemigroup and HasGeneratorsOfSemigroup],
function(I, S)
  if ForAll(GeneratorsOfSemigroup(S), x-> x in I) then 
    if S=Parent(I) then 
      return true;
    elif HasGeneratorsOfSemigroup(I) then 
      return ForAll(GeneratorsOfSemigroup(I), x-> x in I);
    else 
      return Size(I)=Size(S);
    fi;
  else 
    return false;
  fi;
end);

#

InstallMethod(\=, "for a semigroup with generators and a semigroup ideal", 
[IsSemigroup and HasGeneratorsOfSemigroup, 
IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal], 
function(S, I)
  return I=S;
end);

#

InstallTrueMethod(IsSemigroupIdeal, IsMagmaIdeal and IsActingSemigroup);

#

InstallMethod(Representative, "for a semigroup ideal", 
[IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(I)
  return Representative(GeneratorsOfMagmaIdeal(I));
end);

# a convenience, similar to the functions <Semigroup>, <Monoid>, etc

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
      elif i=Length(arg[2]) and IsRecord(arg[2][i]) then
        return SemigroupIdealByGenerators(out, arg[2][i]);
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

InstallMethod(SemigroupIdealByGenerators, "for an associative element collection",
[IsActingSemigroup, IsAssociativeElementCollection], 
function(S, gens)
  return SemigroupIdealByGenerators(S, gens, S!.opts);
end);

#

InstallMethod(SemigroupIdealByGenerators, 
"for an acting semigroup, associative element collection and record",
[IsActingSemigroup, IsAssociativeElementCollection, IsRecord],
function(S, gens, opts)
  local filts, I;

  #JDM: is this a good idea?
  if not ForAll(gens, x-> x in S) then 
    Error("usage: the generators do not belong to the semigroup,");
    return fail;
  fi;

  #JDM: check if the ideal is actually the whole semigroup?

  opts:=SemigroupOptions(opts);
  gens:=AsList(gens);
  
  filts:=IsMagmaIdeal and IsAttributeStoringRep;

  if opts.acting then 
    filts:=filts and IsActingSemigroup;
  fi;

  I:=Objectify( NewType( FamilyObj( gens ), filts ), rec(opts:=opts));
  
  if IsActingSemigroupWithInverseOp(S) then 
    SetFilterObj(I, IsActingSemigroupWithInverseOp);
  elif (HasIsRegularSemigroup(S) and IsRegularSemigroup(S)) or opts.regular then 
    SetIsRegularSemigroup(I, true);
  fi;
  
  if  HasSupersemigroupOfIdeal(S) then 
    SetSupersemigroupOfIdeal(I, SupersemigroupOfIdeal(S));
  elif HasGeneratorsOfSemigroup(S) then 
    SetSupersemigroupOfIdeal(I, S);
  fi;
  
  SetParent(I, S); 
  SetGeneratorsOfMagmaIdeal(I, gens);

  return I;
end);

#EOF
