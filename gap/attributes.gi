#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for finding various attributes of finite semigroups
# that can satisfy either IsNonExhaustiveSemigroup or IsExhaustiveSemigroup.

# Note about the difference between One and MultiplicativeNeutralElement 
# (the same goes for Zero and MultplicativeZero):
#
# One(s) returns One(Representative(s)) if it belongs to s, so that 
# One(s)=Transformation([1..DegreeOfTransformationSemigroup(s)]) if s is a
# transformation semigroup and it returns fail otherwise, or it returns 
# PartialPerm([1..DegreeOfPartialPermSemigroup]) if this belongs to s. 
#
# MultiplicativeNeutralElement on the other hand returns the element of s that
# acts as the identity, note that this can be equal to One(s) but it can also
# not be equal t One(s). 
#
# A semigroup satisfies IsMonoidAsSemigroup(s) if
# MultiplicativeNeutralElement(x)<>fail, so it could be that One(s) returns
# fail but IsMonoidAsSemigroup is still true. 

# same method for ideals

InstallMethod(IsomorphismFpMonoid, "for a finite monoid",
[IsMonoid and IsFinite],
function(S)
  local rules, F, A, rels, Q, B;
 
  rules:=Enumerate(ExhaustiveData(S))!.rules;
  
  F:=FreeMonoid(Length(GeneratorsOfMonoid(S)));
  A:=GeneratorsOfMonoid(F);
  rels:=List(rules, x-> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);
  
  Q:=F/rels; 
  B:=GeneratorsOfMonoid(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, Factorization(S, x)), 
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfMonoid(S)));

end);

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a finite semigroup",
[IsFinite and IsSemigroup],
function(S)
  local rules, F, A, rels, Q, B;
  
  rules:=Enumerate(ExhaustiveData(S))!.rules;
  
  F:=FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A:=GeneratorsOfSemigroup(F);
  rels:=List(rules, x-> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);
  
  Q:=F/rels; 
  B:=GeneratorsOfSemigroup(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, Factorization(S, x)), 
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S)));
end);

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a finite monoid",
[IsMonoid and IsFinite],
function(S)
  local rules, lookup, convert, F, A, rels, one, Q, B, i;
 
  if GeneratorsOfSemigroup(S)=GeneratorsOfMonoid(S) then 
    return IsomorphismFpMonoid(S);
  fi;

  lookup:=List(GeneratorsOfMonoid(S), x-> Position(GeneratorsOfSemigroup(S), x));
  one:=Position(GeneratorsOfSemigroup(S), One(S));
  # if One(S) appears more than once in the generators of S, then this causes
  # problems here... JDM
  convert:=function(w)
    if not IsEmpty(w) then 
      return List(w, i-> lookup[i]); 
    else
      return [one];
    fi;
  end;
  #convert words in generators of monoid to words in generators of semigroup

  rules:=Enumerate(ExhaustiveData(S))!.rules;
  
  F:=FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A:=GeneratorsOfSemigroup(F);
  rels:=Set(rules, x-> [EvaluateWord(A, convert(x[1])), 
   EvaluateWord(A, convert(x[2]))]);

  # add relations for the identity
  AddSet(rels, [A[one]^2, A[one]]);
  for i in [1..Length(GeneratorsOfMonoid(S))] do 
    AddSet(rels, [A[lookup[i]]*A[one], A[lookup[i]]]);
    AddSet(rels, [A[one]*A[lookup[i]], A[lookup[i]]]);
  od;
  
  Q:=F/rels; 
  B:=GeneratorsOfSemigroup(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, convert(Factorization(S, x))), 
  # Factorization returns a word in the monoid generators of S
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S)));
end);

# same method for ideals

InstallMethod(RightCayleyGraphSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Enumerate(ExhaustiveData(S))!.right;
end);

# same method for ideals

InstallMethod(LeftCayleyGraphSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Enumerate(ExhaustiveData(S))!.left;
end);

# same method for ideals 

InstallMethod(IdempotentGeneratedSubsemigroup, "for finite semigroup",
[IsNonExhaustiveSemigroup], S-> Semigroup(Idempotents(S), rec(small:=true)));

# same method for ideals 

InstallMethod(IdempotentGeneratedSubsemigroup, 
"for a finite inverse op semigroup", 
[IsSemigroupWithInverseOp and IsFinite],
S-> InverseSemigroup(Idempotents(S), rec(small:=true)));

# same method for ideals

InstallMethod(IsomorphismReesMatrixSemigroup, "for a D-class", [IsGreensDClass],
InjectionPrincipalFactor);

# same method for ideal

InstallMethod(IrredundantGeneratingSubset,
"for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
  local gens, nrgens, deg, out, redund, i, f;
    
   if (IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll)) or 
   (HasIsSemigroupIdeal(coll) and IsSemigroupIdeal(coll)) then
    coll:=ShallowCopy(GeneratorsOfSemigroup(coll));
  fi;
  
  if Size(coll)=1 then 
    return coll;
  fi;

  gens:=Set(ShallowCopy(coll)); 
  nrgens:=Length(gens);

  if IsNonExhaustiveSemigroup(coll) or IsGeneratorsOfNonExhaustiveSemigroup(coll) then 
    deg:=ActionDegree(coll);
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
    Sort(coll, function(x, y) return ActionRank(x, deg)>ActionRank(y, deg); end);
  fi;
 
  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;

  repeat
    i:=i+1; f:=coll[i];
    if InfoLevel(InfoSemigroups)>=3 then 
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund),
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then
      if f in Semigroup(Difference(gens, [f])) then
        AddSet(redund, f); gens:=Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund)+Length(out)=nrgens;

  if InfoLevel(InfoSemigroups)>1 then
    Print("\n");
  fi;
  return out;
end);

#

InstallMethod(IsomorphismReesMatrixSemigroup, 
"for a finite simple or 0-simplesemigroup", [IsFinite and IsSemigroup],
function(S)
  local D, iso, inv;
  if not (IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S)) then 
    TryNextMethod();
  fi;
  D:=GreensDClasses(S)[1];
  if IsZeroSimpleSemigroup(S) 
   and IsMultiplicativeZero(S, Representative(D)) then 
    D:=GreensDClasses(S)[2];
  fi;
  iso:=IsomorphismReesMatrixSemigroup(D);
  inv:=InverseGeneralMapping(iso);
  return MagmaIsomorphismByFunctionsNC(S, Range(iso), x-> x^iso, x->x^inv);
end);

# same method for ideals

InstallMethod(InversesOfSemigroupElement, 
"for a finite semigroup and associative element",
[IsNonExhaustiveSemigroup, IsAssociativeElement],
function(S, x)

  if x in S then
    return InversesOfSemigroupElementNC(S, x);
  fi;

  return fail;
end);

# same method for ideals

InstallMethod(MinimalIdeal, "for a finite semigroup", [IsFinite and IsSemigroup],
function(S)
  local I;  
  I:=SemigroupIdeal(S, Representative(MinimalDClass(S)));
  SetIsSimpleSemigroup(I, true);
  return I;
end);

#

InstallMethod(PrincipalFactor, "for a Green's D-class", 
[IsGreensDClass], D-> Range(InjectionPrincipalFactor(D)));

#

InstallMethod(PrincipalFactor, "for a D-class",
[IsGreensDClass], AssociatedReesMatrixSemigroupOfDClass);

# different method for ideals, not yet implemented 

InstallMethod(SmallSemigroupGeneratingSet, 
"for an associative element collection", 
[IsAssociativeElementCollection],
function(coll)
  if Length(coll)<2 then 
    return coll;
  else
    return GeneratorsOfSemigroup(Semigroup(coll, rec(small:=true)));
  fi;
end);

# different method for ideals, not yet implemented 

InstallMethod(SmallSemigroupGeneratingSet, 
"for a finite semigroup", [IsSemigroup and IsFinite],
function(S)
  return SmallSemigroupGeneratingSet(GeneratorsOfSemigroup(S));
end);

#

InstallMethod(SmallMonoidGeneratingSet, 
"for an associative element with one collection", 
[IsAssociativeElementCollection and IsMultiplicativeElementWithOneCollection],
function(coll)
  if Length(coll)<2 then 
    return coll;
  else
    return GeneratorsOfMonoid(Monoid(coll, rec(small:=true)));
  fi;
end);

# same method for ideals

InstallMethod(SmallMonoidGeneratingSet, "for a finite monoid", 
[IsFinite and IsMonoid],
function(S)
  if IsEmpty(GeneratorsOfMonoid(S)) then 
    return []; 
  fi;
  return SmallMonoidGeneratingSet(GeneratorsOfMonoid(S));
end);

#

InstallMethod(SmallInverseSemigroupGeneratingSet, 
"for generators of an inverse semigroup", 
[IsGeneratorsOfInverseSemigroup],
function(coll)
  if Length(coll)<2 then 
    return coll;
  else
    return GeneratorsOfInverseSemigroup(InverseSemigroup(coll, rec(small:=true)));
  fi;
end);

#

InstallMethod(SmallInverseSemigroupGeneratingSet, 
"for a semigroup with inverse op", [IsSemigroupWithInverseOp],
function(S)
  return SmallSemigroupGeneratingSet(GeneratorsOfInverseSemigroup(S));
end);

#

InstallMethod(SmallInverseMonoidGeneratingSet, 
"for generators of an inverse monoid", 
[IsGeneratorsOfInverseSemigroup and IsMultiplicativeElementWithOneCollection],
function(coll)
  if Length(coll)<2 then 
    return coll;
  else
    return GeneratorsOfInverseMonoid(InverseMonoid(coll, rec(small:=true)));
  fi;
end);

#

InstallMethod(SmallInverseMonoidGeneratingSet, 
"for a monoid with inverse op", 
[IsSemigroupWithInverseOp and IsMonoid],
function(S)
  return SmallSemigroupGeneratingSet(GeneratorsOfInverseMonoid(S));
end);

#

InstallMethod(SmallGeneratingSet, "for a semigroup",
[IsSemigroup],
function(S)
  
  if HasGeneratorsOfSemigroupIdeal(S) then 
    return MinimalIdealGeneratingSet(S);
  elif HasGeneratorsOfGroup(S) then 
    return SmallGeneratingSet(GeneratorsOfGroup(S));
  elif HasGeneratorsOfInverseMonoid(S) then 
    return SmallInverseMonoidGeneratingSet(S);
  elif HasGeneratorsOfInverseSemigroup(S) then 
    return SmallInverseSemigroupGeneratingSet(S);
  elif HasGeneratorsOfMonoid(S) then
    return SmallMonoidGeneratingSet(S);
  fi;

  return SmallSemigroupGeneratingSet(S);
end);

#

InstallMethod(StructureDescription, "for a Brandt semigroup",
[IsBrandtSemigroup],
function(S)
  local x, D;
  
  x:=First(Generators(S), x-> x<>MultiplicativeZero(S));
  
  if x=fail then 
    return "0";
  fi;

  D:=GreensDClassOfElementNC(S, x);
  
  return Concatenation("B(", StructureDescription(GroupHClass(D)), ", ",
  String(NrRClasses(D)), ")");
end);

# same method for ideals 

InstallMethod(StructureDescription, "for a group as semigroup",
[IsGroupAsSemigroup], s-> StructureDescription(Range(IsomorphismPermGroup(s))));


# not relevant for ideals

InstallMethod(Size, "for a monogenic transformation semigroup",
[IsTransformationSemigroup and IsMonogenicSemigroup],
function(s)
  local ind;
  
  ind:=IndexPeriodOfTransformation(GeneratorsOfSemigroup(s)[1]);
  if ind[1]>0 then 
    return Sum(ind)-1;
  fi;
  return Sum(ind);
end);

#EOF
