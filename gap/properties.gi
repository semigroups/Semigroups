###############################################################################
##
#W  properties.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Ecom (commuting idempotents), LI (locally trivial),
# LG (locally group), B1 (dot-depth one), DA (regular D-classes are idempotent)
# R v L (???), IsNilpotentSemigroup, inverses, local submonoid, right ideal,
# left ideal, kernel!?

# IsLeftCancellative,
# IsRightCancellative, IsRightGroup, IsLeftGroup, IsUnitarySemigroup,
# IsRightUnitarySemigp, IsLeftUnitarySemigp, IsCongruenceFree,
# PrimitiveIdempotents, IdempotentOrder,
# IsLeftNormalBand, IsRightNormalBand, IsNormalBand, IsEUnitarySemigroup
# IsRectangularGroup, IsBandOfGroups, IsFreeBand, IsFreeSemilattice,
# IsFreeNormalBand, , IsFundamentalInverseSemigp,
# IsFullSubsemigroup (of an inverse semigroup), IsFactorizableInverseMonoid,
# IsFInverseSemigroup, IsSemigroupWithCentralIdempotents, IsLeftUnipotent,
# IsRightUnipotent, IsSemigroupWithClosedIdempotents, .

# a better method for MinimalIdeal of a simple semigroup.

#InstallMethod(IsAbundantSemigroup, "for a trans. semigroup",
#[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
#function(s)
#  local iter, n, ht, ht_o, reg, i, data, f, ker, val, o, scc;
#
#  Info(InfoWarning, 1, "this will sometimes return a false positive.");
#
#  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then
#    Info(InfoSemigroups, 2, "semigroup is regular");
#    return true;
#  fi;
#
#  iter:=IteratorOfRClassData(s); n:=ActionDegree(s);
#  ht:=HTCreate([1..n], rec(hashlen:=s!.opts!.hashlen!.S));
#  ht_o:=HTCreate([1,1,1,1], rec(hashlen:=s!.opts!.hashlen!.S));
#  reg:=[]; i:=0;
#
#  repeat
#    repeat #JDM this should become an method for IteratorOfRStarClasses
#           # and IsAbundantRClass...
#      data:=NextIterator(iter);
#    until HTValue(ht_o, data{[1,2,4,5]})=fail or IsDoneIterator(iter);
#    if not IsDoneIterator(iter) then
#      HTAdd(ht_o, data{[1,2,4,5]}, true);
#
#      #f:=RClassRepFromData(s, data); ker:=CanonicalTransSameKernel(f);
#      val:=HTValue(ht, ker);
#
#      if val=fail then #new kernel
#        i:=i+1; HTAdd(ht, ker, i);
#        val:=i; reg[val]:=false;
#      fi;
#
#      if reg[val]=false then #old kernel
#        #o:=ImageOrbitFromData(s, data); scc:=ImageOrbitSCCFromData(s, data);
#        reg[val]:=ForAny(scc, j-> IsInjectiveListTrans(o[j], ker));
#      fi;
#    fi;
#  until IsDoneIterator(iter);
#
#  return ForAll(reg, x-> x);
#end);

#InstallMethod(IsAdequateSemigroup,
#"for acting semigroup with generators",
#[IsActingSemigroup and HasGeneratorsOfSemigroup],
#s-> IsAbundantSemigroup(s) and IsBlockGroup(s));

# same method for ideals

InstallMethod(IsBand, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsBand(Parent(S)) and IsBand(Parent(S)) then
    return true;
  else
    return IsCompletelyRegularSemigroup(S) and IsHTrivial(S);
  fi;
end);

# same method for ideals

InstallMethod(IsBand, "for an inverse semigroup", [IsInverseSemigroup],
IsSemilattice);

# same method for ideals

InstallMethod(IsBlockGroup, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local iter, d;

  if HasParent(S) and HasIsBlockGroup(Parent(S))
      and IsBlockGroup(Parent(S)) then
    return true;
  elif HasIsInverseSemigroup(S) and IsInverseSemigroup(S) then
    Info(InfoSemigroups, 2, "inverse semigroup");
    return true;
  elif (HasIsRegularSemigroup(S) and IsRegularSemigroup(S))
      and (HasIsInverseSemigroup(S) and not IsInverseSemigroup(S)) then
    Info(InfoSemigroups, 2, "regular but non-inverse semigroup");
    return false;
  fi;

  iter := IteratorOfDClasses(S);

  for d in iter do
    if IsRegularDClass(d) and
         (ForAny(RClasses(d), x -> NrIdempotents(x) > 1)
          or NrRClasses(d) <> NrLClasses(d)) then
      return false;
    fi;
  od;
  return true;
end);

# same method for ideals

InstallMethod(IsBrandtSemigroup, "for an acting semigroup",
[IsActingSemigroup],
S -> IsZeroSimpleSemigroup(S) and IsInverseSemigroup(S));

# same method for inverse ideals

InstallMethod(IsBrandtSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsZeroSimpleSemigroup);

# same method for non-regular ideals

InstallMethod(IsCliffordSemigroup,
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens, idem, f, g;

  if HasParent(S) and HasIsCliffordSemigroup(Parent(S))
      and IsCliffordSemigroup(Parent(S)) then
    return true;
  elif HasIsInverseSemigroup(S) and not IsInverseSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not inverse");
    return false;
  elif not IsCompletelyRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not completely regular");
    return false;
  elif IsGroup(S) or IsGroupAsSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is a group");
    return true;
  fi;

  if not IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  fi;

  gens := GeneratorsOfSemigroup(S);
  idem := Set(List(gens,
                   x -> IdempotentCreator(S)(LambdaFunc(S)(x),
                                             RhoFunc(S)(x))));

  for f in gens do
    for g in idem do
      if not f * g = g * f then
        Info(InfoSemigroups, 2, "the idempotents are not central:");
        Info(InfoSemigroups, 2, "  ", f, "\n#I  and\n#I    ", g,
             "\n#I  do not commute,");
        return false;
      fi;
    od;
  od;

  return true;
end);

# same method for inverse ideals

InstallMethod(IsCliffordSemigroup, "for an inverse acting semigroup",
[IsInverseSemigroup and IsActingSemigroup],
S -> ForAll(OrbSCC(LambdaOrb(S)), x -> Length(x) = 1));

# same method for regular ideals, or non-regular without a generating set

InstallMethod(IsCliffordSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsCliffordSemigroup(Parent(S))
      and IsCliffordSemigroup(Parent(S)) then
    return true;
  else
    return IsRegularSemigroup(S) and NrHClasses(S) = NrDClasses(S);
  fi;
end);

# different method for ideals

InstallMethod(IsCommutativeSemigroup, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens, n, i, j;

  if HasParent(S) and HasIsCommutativeSemigroup(Parent(S))
      and IsCommutativeSemigroup(Parent(S)) then
    return true;
  fi;

  gens := GeneratorsOfSemigroup(S);
  n := Length(gens);

  for i in [1 .. n] do
    for j in [i + 1 .. n] do
      if not gens[i] * gens[j] = gens[j] * gens[i] then
        Info(InfoSemigroups, 2, "generators ", i, " and ", j,
             " do not commute");
        return false;
      fi;
    od;
  od;

  return true;
end);

# same method for non-regular ideals with generators

InstallMethod(IsCompletelyRegularSemigroup,
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local record, o, pos, f;

  if HasParent(S) and HasIsCompletelyRegularSemigroup(Parent(S))
      and IsCompletelyRegularSemigroup(Parent(S)) then
    return true;
  elif HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "semigroup is not regular");
    return false;
  fi;

  record := ShallowCopy(LambdaOrbOpts(S));
  record.treehashsize := SEMIGROUPS_OptionsRec(S).hashlen.M;

  for f in GeneratorsOfSemigroup(S) do
    o := Orb(S, LambdaFunc(S)(f), LambdaAct(S), record);
    pos := LookForInOrb(o,
                        function(o, x)
                          return LambdaRank(S)(LambdaAct(S)(x, f)) <>
                                 LambdaRank(S)(x);
                        end,
                        1);
    # for transformations we could use IsInjectiveListTrans instead
    # and the performance would be better!

    if pos <> false then
      Info(InfoSemigroups, 2, "at least one H-class is not a subgroup");
      return false;
    fi;
  od;

  return true;
end);

# same method for regular ideals, or non-regular without a generating set

InstallMethod(IsCompletelyRegularSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsCompletelyRegularSemigroup(Parent(S))
      and IsCompletelyRegularSemigroup(Parent(S)) then
    return true;
  elif HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "semigroup is not regular");
    return false;
  fi;
  return NrHClasses(S) = NrIdempotents(S);
end);

# same method for inverse ideals

InstallMethod(IsCompletelyRegularSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsCliffordSemigroup);

# Notes: this test required to avoid conflict with Smallsemi,
# DeclareSynonymAttr causes problems.

#same method for ideals

InstallMethod(IsCompletelySimpleSemigroup, "for a semigroup",
[IsSemigroup], S -> IsSimpleSemigroup(S) and IsFinite(S));

#different method for ideals

InstallMethod(IsFactorisableSemigroup, "for an inverse op acting semigroup",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(S)
  local G, iso, enum, f;

  G := GroupOfUnits(S);

  if G = fail then
    return false;
  elif IsTrivial(G) then
    return IsSemilattice(S);
  fi;

  iso := InverseGeneralMapping(IsomorphismPermGroup(G));
  enum := Enumerator(Source(iso));

  for f in Generators(S) do
    if not f in G then
      if not ForAny(enum, g -> NaturalLeqInverseSemigroup(f, g ^ iso)) then
        return false;
      fi;
    fi;
  od;
  return true;
end);

# same method for ideals

InstallMethod(IsFactorisableSemigroup,
"for an inverse semigroup with generators",
[IsInverseSemigroup and HasGeneratorsOfSemigroup],
function(S)
  if IsGeneratorsOfInverseSemigroup(GeneratorsOfSemigroup(S)) then
    return IsFactorisableSemigroup(AsPartialPermSemigroup(S));
  fi;
  return false;
end);

# same method for ideals

InstallMethod(IsHTrivial, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, x;

  if HasParent(S) and HasIsHTrivial(Parent(S)) and IsHTrivial(Parent(S)) then
    return true;
  fi;

  if IsTransformationSemigroup(S) and HasGeneratorsOfSemigroup(S) then
    for x in GeneratorsOfSemigroup(S) do
      if IndexPeriodOfTransformation(x)[2] <> 1 then
        return false;
      fi;
    od;
  elif IsPartialPermSemigroup(S) and HasGeneratorsOfSemigroup(S) then
    for x in GeneratorsOfSemigroup(S) do
      if IndexPeriodOfPartialPerm(x)[2] <> 1 then
        return false;
      fi;
    od;
  fi;

  iter := IteratorOfDClasses(S);

  for x in iter do
    if not IsTrivial(SchutzenbergerGroup(x)) then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(IsHTrivial, "for a semigroup", [IsSemigroup],
function(S)

  if HasParent(S) and HasIsHTrivial(Parent(S)) and IsHTrivial(Parent(S)) then
    return true;
  fi;
  return NrHClasses(S) = Size(S);
end);

#same method for ideals

InstallMethod(IsHTrivial, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
d -> NrHClasses(d) = Size(d));

#same method for non-inverse ideals

InstallMethod(IsLTrivial, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, d;

  if HasParent(S) and HasIsLTrivial(Parent(S)) and IsLTrivial(Parent(S)) then
    return true;
  fi;

  iter := IteratorOfDClasses(S);

  for d in iter do
    if not IsTrivial(SchutzenbergerGroup(d)) then
      return false;
    fi;
    if Length(RhoOrbSCC(d)) <> 1 then
      return false;
    fi;
  od;

  return true;
end);

# same method for inverse ideals

InstallMethod(IsLTrivial, "for an inverse acting semigroup",
[IsActingSemigroupWithInverseOp],
function(S)
  if HasParent(S) and HasIsLTrivial(Parent(S)) and IsLTrivial(Parent(S)) then
    return true;
  fi;
  return ForAll(OrbSCC(LambdaOrb(S)), x -> Length(x) = 1);
end);

#same method for ideals

InstallMethod(IsLTrivial, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
d -> NrLClasses(d) = Size(d));

#same method for ideals

InstallMethod(IsRTrivial, "for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
d -> NrRClasses(d) = Size(d));

#same method for ideals

InstallMethod(IsRTrivial, "for an inverse semigroup",
[IsInverseSemigroup], IsLTrivial);

# different method for ideals

InstallMethod(IsRTrivial, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  if ForAny(GeneratorsOfSemigroup(S),
            x -> ForAny(CyclesOfTransformation(x), y -> Length(y) > 1)) then
    return false;
  else
    return ForAll(CyclesOfTransformationSemigroup(S), x -> Length(x) = 1);
  fi;
end);

# different method for ideals

InstallMethod(IsRTrivial, "for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(S)
  if ForAny(GeneratorsOfSemigroup(S),
            x -> ForAny(CyclesOfPartialPerm(x), y -> Length(y) > 1)) then
    return false;
  else
    return ForAll(CyclesOfPartialPermSemigroup(S), x -> Length(x) = 1);
  fi;
end);

# same method for non-inverse ideals

InstallMethod(IsRTrivial, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, x;

  if HasParent(S) and HasIsRTrivial(Parent(S)) and IsRTrivial(Parent(S)) then
    return true;
  fi;

  if IsClosedData(SemigroupData(S)) and IsClosed(RhoOrb(S)) then
    for x in GreensDClasses(S) do
      if (not IsTrivial(SchutzenbergerGroup(x)))
          or Length(LambdaOrbSCC(x)) > 1 then
        return false;
      fi;
    od;
    return true;
  fi;

  iter := IteratorOfRClasses(S);

  for x in iter do
    if (not IsTrivial(SchutzenbergerGroup(x)))
        or Length(LambdaOrbSCC(x)) > 1 then
      return false;
    fi;
  od;

  return true;
end);

InstallMethod(IsRTrivial, "for a semigroup", [IsSemigroup],
function(S)
  if HasParent(S) and HasIsRTrivial(Parent(S)) and IsRTrivial(Parent(S)) then
    return true;
  fi;
  return Size(S) = NrRClasses(S);
end);

# same method for non-regular ideals

InstallMethod(IsGroupAsSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gens, lambdafunc, lambda, rhofunc, rho, tester, lambda_f, rho_f, f;

  if HasParent(S) and HasIsGroupAsSemigroup(Parent(S))
      and IsGroupAsSemigroup(Parent(S)) then
    return true;
  fi;

  gens := GeneratorsOfSemigroup(S); #not GeneratorsOfMonoid!

  if IsActingSemigroupWithFixedDegreeMultiplication(S)
      and ForAll(gens, f -> ActionRank(S)(f) = ActionDegree(f)) then
    return true;
  fi;

  lambdafunc := LambdaFunc(S);
  lambda := lambdafunc(gens[1]);
  rhofunc := RhoFunc(S);
  rho := rhofunc(gens[1]);
  tester := IdempotentTester(S);

  for f in gens do
    lambda_f := lambdafunc(f);
    rho_f := rhofunc(f);
    if lambda_f <> lambda or rho_f <> rho or not tester(lambda_f, rho_f) then
      return false;
    fi;
  od;

  return true;
end);

# different method for inverse, regular ideals (so that it has higher rank)

InstallMethod(IsGroupAsSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsGroupAsSemigroup(Parent(S))
      and IsGroupAsSemigroup(Parent(S)) then
    return true;
  fi;
  return NrRClasses(S) = 1 and NrLClasses(S) = 1;
end);

#JDM should gens in the following function be GeneratorsOfSemigroup?
# IteratorOfIdempotents would be good here.

# same method for ideals

InstallMethod(IsIdempotentGenerated, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gens, t, min, new;

  gens := Generators(S);

  if ForAll(gens, IsIdempotent) then
    Info(InfoSemigroups, 2, "all the generators are idempotents");
    return true;
  fi;

  if HasIdempotentGeneratedSubsemigroup(S) then
    t := IdempotentGeneratedSubsemigroup(S);
  else
    min := MinimumList(List(gens, x -> ActionRank(S)(x)));
    new := Filtered(Idempotents(S), x -> ActionRank(S)(x) >= min);
    if new = [] then
      return false;
    fi;
    t := Semigroup(new);
  fi;

  # this is not always the idempotent generated subsemigroup!
  return ForAll(gens, f -> f in t);
end);

# same method for inverse ideals

InstallMethod(IsIdempotentGenerated, "for an inverse semigroup",
[IsInverseSemigroup], IsSemilattice);

# same method for ideals

InstallMethod(IsInverseSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local lambda, rho, iter, x;

  if HasGeneratorsOfSemigroup(S) and
      IsGeneratorsOfInverseSemigroup(GeneratorsOfSemigroup(S)) and
      ForAll(GeneratorsOfSemigroup(S), x -> x ^ -1 in S) then
    return true;
  fi;

  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif IsCompletelyRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return IsCliffordSemigroup(S);
  fi;

  lambda := LambdaOrb(S);
  Enumerate(lambda);
  rho := RhoOrb(S);
  Enumerate(rho, Length(lambda));
  # TODO shouldn't the below be Length(rho) = Length(lambda)?
  # and we should check that rho is closed.
  if not (IsClosed(rho) and Length(rho) >= Length(lambda)) then
    Info(InfoSemigroups, 2,
         "the numbers of lambda and rho values are not equal");
    return false;
  fi;

  if HasGreensDClasses(S) then
    iter := GreensDClasses(S);
    for x in iter do
      if not IsRegularClass(x) or NrIdempotents(x) <> NrRClasses(x) then
        return false;
      fi;
    od;
    return true;
  fi;

  return NrLClasses(S) = NrRClasses(S) and NrIdempotents(S) = NrRClasses(S);
end);

# same method for ideals

InstallMethod(IsLeftSimple, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local iter;

  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then
    return false;
  elif IsLeftZeroSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is a left zero semigroup");
    return true;
  elif HasNrLClasses(S) then
    return NrLClasses(S) = 1;
  fi;

  iter := IteratorOfLClassReps(S);
  NextIterator(iter);
  return IsDoneIterator(iter);
end);

# same method for ideals

InstallMethod(IsLeftSimple, "for an inverse semigroup",
[IsInverseSemigroup],
x -> IsGroup(x) or IsGroupAsSemigroup(x));

# different method for ideals without generators

InstallMethod(IsLeftZeroSemigroup,
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens, lambda, val, x;

  if HasParent(S) and HasIsLeftZeroSemigroup(Parent(S))
      and IsLeftZeroSemigroup(Parent(S)) then
    return true;
  fi;

  gens := GeneratorsOfSemigroup(S);
  lambda := LambdaFunc(S);
  val := lambda(gens[1]);

  for x in gens do
    if not lambda(x) = val then
      return false;
    fi;
  od;

  return ForAll(gens, IsIdempotent);
end);

InstallMethod(IsLeftZeroSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  if HasParent(S) and HasIsLeftZeroSemigroup(Parent(S))
      and IsLeftZeroSemigroup(Parent(S)) then
    return true;
  fi;
  return NrLClasses(S) = 1 and Size(S) = NrRClasses(S);
end);

# same method for ideals

InstallMethod(IsLeftZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# not applicable for ideals

InstallImmediateMethod(IsMonogenicSemigroup,
IsSemigroup and HasGeneratorsOfSemigroup,
0,
function(S)
  if Length(GeneratorsOfSemigroup(S)) = 1 then
    return true;
  fi;
  TryNextMethod();
end);

# same method for ideals

InstallMethod(IsMonogenicSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gens, I, f, i;
  #TODO this if-condition is redundant due to the previous immediate method
  if HasGeneratorsOfSemigroup(S) then

    gens := GeneratorsOfSemigroup(S);

    if not IsDuplicateFreeList(gens) then
      gens := ShallowCopy(DuplicateFreeList(gens));
      Info(InfoSemigroups, 2, "there are repeated generators");
    fi;

    if Length(gens) = 1 then
      Info(InfoSemigroups, 2, "the semigroup only has one generator");
      return true;
    fi;
  fi;

  I := MinimalIdeal(S);

  if not (IsGroup(I) or IsGroupAsSemigroup(I)) then
    Info(InfoSemigroups, 2, "the minimal ideal is not a group.");
    return false;
  elif not IsCyclic(Range(IsomorphismPermGroup(I))) then
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;

  gens := GeneratorsOfSemigroup(S);

  for i in [1 .. Length(gens)] do
    f := gens[i];
    if ForAll(gens, x -> x in Semigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i);
      SetMinimalGeneratingSet(S, [f]);
      return true;
    fi;
  od;
  Info(InfoSemigroups, 2, "at least one generator does not belong to the",
       " semigroup generated by any ");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

# same method for ideals

InstallMethod(IsMonogenicInverseSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  if not IsInverseSemigroup(S) then
    return false;
  fi;
  return IsMonogenicInverseSemigroup(AsPartialPermSemigroup(S));
end);

# same method for ideals

InstallMethod(IsMonogenicInverseSemigroup,
"for an acting semigroup with inverse op", [IsActingSemigroupWithInverseOp],
function(S)
  local gens, I, f, i;

  if HasGeneratorsOfInverseSemigroup(S) then
    gens := GeneratorsOfInverseSemigroup(S);

    if not IsDuplicateFreeList(gens) then
      gens := ShallowCopy(DuplicateFreeList(gens));
      Info(InfoSemigroups, 2, "there are repeated generators");
    fi;

    if Length(gens) = 1 then
      Info(InfoSemigroups, 2, "the semigroup only has one generator");
      return true;
    fi;
  fi;

  I := MinimalIdeal(S);

  if not (IsGroup(I) or IsGroupAsSemigroup(I)) then
    Info(InfoSemigroups, 2, "the minimal ideal is not a group.");
    return false;
  elif not IsCyclic(Range(IsomorphismPermGroup(I))) then
    Info(InfoSemigroups, 2, "the minimal ideal is a non-cyclic group.");
    return false;
  fi;

  gens := GeneratorsOfInverseSemigroup(S);

  for i in [1 .. Length(gens)] do
    f := gens[i];
    if ForAll(gens, x -> x in InverseSemigroup(f)) then
      Info(InfoSemigroups, 2, "the semigroup is generated by generator ", i);
      SetMinimalGeneratingSet(S, [f]);
      return true;
    fi;
  od;

  Info(InfoSemigroups, 2, "at least one generator does not belong to the",
       " inverse semigroup generated by any ");
  Info(InfoSemigroups, 2, "other generator.");
  return false;
end);

#same method for ideals

InstallMethod(IsMonoidAsSemigroup, "for a semigroup",
[IsSemigroup],
x -> not IsMonoid(x) and MultiplicativeNeutralElement(x) <> fail);

# is there a better method? JDM
# same method for ideals

InstallMethod(IsOrthodoxSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local e, m, i, j;

  if not IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  fi;

  e := Idempotents(S);
  m := Length(e);

  for i in [1 .. m] do
    for j in [1 .. m] do

      if not IsIdempotent(e[i] * e[j]) then
        Info(InfoSemigroups, 2, "the product of idempotents ", i, " and ", j,
             " is not an idempotent");
        return false;
      fi;
    od;
  od;

  return true;
end);

# same method for ideals

InstallMethod(IsRectangularBand, "for an acting semigroup",
[IsActingSemigroup],
function(S)

  if HasParent(S) and HasIsRectangularBand(Parent(S))
      and IsRectangularBand(Parent(S)) then
    return true;
  fi;

  if not IsSimpleSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not simple");
    return false;
  elif HasIsBand(S) then
    return IsBand(S);
  fi;

  return IsHTrivial(S);
end);

# same method for ideals

InstallMethod(IsRectangularBand, "for an inverse semigroup",
[IsInverseSemigroup],
function(S)

  if HasParent(S) and HasIsRectangularBand(Parent(S))
      and IsRectangularBand(Parent(S)) then
    return true;
  fi;
  return IsHTrivial(S) and IsSimpleSemigroup(S);
end);

# different method for ideals (ideals know at their point of creation if they
# are regular or not)

InstallMethod(IsRegularSemigroup, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local tester, rhofunc, lookfunc, data, i;

  if IsSimpleSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is simple");
    return true;
  elif HasIsCompletelyRegularSemigroup(S)
      and IsCompletelyRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return true;
  elif HasGreensDClasses(S) then
    return ForAll(GreensDClasses(S), IsRegularDClass);
  fi;

  tester := IdempotentTester(S);
  rhofunc := RhoFunc(S);

  # look for <S> not being regular
  lookfunc := function(data, x)
    local rho, scc, i;
    if data!.repslens[x[2]][data!.orblookup1[x[6]]] > 1 then
      return true;
    fi;

    # data corresponds to the group of units...
    if IsActingSemigroupWithFixedDegreeMultiplication(S)
        and ActionRank(S)(x[4]) = ActionDegree(x[4]) then
      return false;
    fi;

    rho := rhofunc(x[4]);
    scc := OrbSCC(x[3])[x[2]];
    for i in scc do
      if tester(x[3][i], rho) then
        return false;
      fi;
    od;
    return true;
  end;

  data := SemigroupData(S);

  for i in [2 .. Length(data)] do
    if lookfunc(data, data[i]) then
      return false;
    fi;
  od;

  if IsClosedData(data) then
    return true;
  fi;

  data := Enumerate(data, infinity, lookfunc);
  return data!.found = false;
end);

#

InstallMethod(IsRegularSemigroup,
"for an acting star semigroup with generators",
[IsActingSemigroup and IsStarSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local lookfunc, data, i;

  if IsSimpleSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is simple");
    return true;
  elif HasIsCompletelyRegularSemigroup(S)
      and IsCompletelyRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is completely regular");
    return true;
  elif HasGreensDClasses(S) then
    return ForAll(GreensDClasses(S), IsRegularDClass);
  fi;

  # look for <S> not being regular
  lookfunc := function(data, x)
    local l;
    if data!.repslens[x[2]][data!.orblookup1[x[6]]] > 1 then
      return true;
    fi;

    # data corresponds to the group of units...
    if IsActingSemigroupWithFixedDegreeMultiplication(S)
        and ActionRank(S)(x[4]) = ActionDegree(x[4]) then
      return false;
    fi;
    #check that the rho value of <x> is in the same scc as the lambda value of
    #<x>
    l := Position(x[3], RhoFunc(S)(x[4]));
    return l = fail or OrbSCCLookup(x[3])[l] <> x[2];
  end;

  data := SemigroupData(S);

  for i in [2 .. Length(data)] do
    if lookfunc(data, data[i]) then
      return false;
    fi;
  od;

  if IsClosedData(data) then
    return true;
  fi;

  data := Enumerate(data, infinity, lookfunc);
  return data!.found = false;
end);

# same method for ideals

InstallMethod(IsRegularSemigroupElement,
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement],
function(S, f)
  local o, scc, rho, tester, i;

  if not f in S then
    Info(InfoSemigroups, 2, "the element does not belong to the semigroup,");
    return false;
  fi;

  if HasIsRegularSemigroup(S) and IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is regular,");
    return true;
  fi;

  if IsClosed(LambdaOrb(S)) then
    o := LambdaOrb(S);
  else
    o := GradedLambdaOrb(S, f, true);
  fi;

  scc := OrbSCC(o)[OrbSCCLookup(o)[Position(o, LambdaFunc(S)(f))]];
  rho := RhoFunc(S)(f);
  tester := IdempotentTester(S);

  for i in scc do
    if tester(o[i], rho) then
      return true;
    fi;
  od;
  return false;
end);

InstallMethod(IsRegularSemigroupElement,
"for an acting star semigroup and associative element with star",
[IsActingSemigroup and IsStarSemigroup, IsAssociativeElementWithStar],
function(S, x)
  local o, k, l;

  if not x in S then
    Info(InfoSemigroups, 2, "the element does not belong to the semigroup,");
    return false;
  fi;

  if HasIsRegularSemigroup(S) and IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is regular,");
    return true;
  fi;

  if IsClosed(LambdaOrb(S)) then
    o := LambdaOrb(S);
  else
    o := GradedLambdaOrb(S, x, true);
  fi;

  k := Position(o, LambdaFunc(S)(x));

  l := Position(o, RhoFunc(S)(x));
  return l <> fail and OrbSCCLookup(o)[k] = OrbSCCLookup(o)[l];
end);

# same method for ideals

InstallMethod(IsRegularSemigroupElementNC,
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement],
function(S, x)
  local o, l, scc, rho, tester, i;

   if IsClosed(LambdaOrb(S)) then
    o := LambdaOrb(S);
    l := Position(o, LambdaFunc(S)(x));
    if l = fail then
      return false;
    fi;
  else
    # this has to be false, since we're not sure if <x> in <S>
    o := GradedLambdaOrb(S, x, false);
    l := 1;
  fi;

  scc := OrbSCC(o)[OrbSCCLookup(o)[l]];
  rho := RhoFunc(S)(x);
  tester := IdempotentTester(S);

  for i in scc do
    if tester(o[i], rho) then
      return true;
    fi;
  od;
  return false;
end);

InstallMethod(IsRegularSemigroupElementNC,
"for an acting semigroup with star and associative element with star",
[IsActingSemigroup and IsStarSemigroup, IsAssociativeElementWithStar],
function(S, x)
  local o, k, l;

   if IsClosed(LambdaOrb(S)) then
    o := LambdaOrb(S);
    k := Position(o, LambdaFunc(S)(x));
    if k = fail then
      return false;
    fi;
  else
    # this has to be false, since we're not sure if <x> in <S>
    o := GradedLambdaOrb(S, x, false);
    k := 1;
  fi;
  l := Position(o, RhoFunc(S)(x));
  return l <> fail and OrbSCCLookup(o)[k] = OrbSCCLookup(o)[l];
end);

# same method for ideals

InstallMethod(IsRightSimple, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local iter;

  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then
    return false;
  elif IsRightZeroSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is a left zero semigroup");
    return true;
  elif HasNrRClasses(S) then
    return NrRClasses(S) = 1;
  fi;

  iter := IteratorOfRClassData(S);
  NextIterator(iter);
  return IsDoneIterator(iter);
end);

# same method for ideals

InstallMethod(IsRightSimple, "for an inverse semigroup",
[IsInverseSemigroup],
x -> IsGroup(x) or IsGroupAsSemigroup(x));

# different method for ideals

InstallMethod(IsRightZeroSemigroup,
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens, rho, val, x;

  if HasParent(S) and HasIsRightZeroSemigroup(Parent(S))
      and IsRightZeroSemigroup(Parent(S)) then
    return true;
  fi;
  gens := GeneratorsOfSemigroup(S);
  rho := RhoFunc(S);
  val := rho(gens[1]);
  for x in gens do
    if rho(x) <> val or not IsIdempotent(x) then
      return false;
    fi;
  od;

  return true;
end);

InstallMethod(IsRightZeroSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsRightZeroSemigroup(Parent(S))
      and IsRightZeroSemigroup(Parent(S)) then
    return true;
  fi;
  return NrRClasses(S) = 1 and Size(S) = NrLClasses(S);
end);

# same method for ideals

InstallMethod(IsRightZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# same method for ideals, JDM why is this not a synonym?

InstallMethod(IsSemiband, "for a semigroup", [IsSemigroup],
IsIdempotentGenerated);

# same method for ideals

InstallMethod(IsSemilattice, "for a semigroup", [IsSemigroup],
function(S)
  if HasParent(S) and HasIsSemilattice(Parent(S))
      and IsSemilattice(Parent(S)) then
    return true;
  fi;
  return IsCommutativeSemigroup(S) and IsBand(S);
end);

# not applicable to ideals

InstallMethod(IsSemilattice,
"for an inverse semigroup with generators",
[IsInverseSemigroup and HasGeneratorsOfSemigroup],
function(S)
  if HasParent(S) and HasIsSemilattice(Parent(S))
      and IsSemilattice(Parent(S)) then
    return true;
  fi;
  return ForAll(GeneratorsOfSemigroup(S), IsIdempotent);
end);

# same method for ideals

InstallMethod(IsSemilattice, "for an inverse semigroup",
[IsInverseSemigroup],
function(S)
  if HasParent(S) and HasIsSemilattice(Parent(S))
      and IsSemilattice(Parent(S)) then
    return true;
  else
    return ForAll(GreensDClasses(S), IsTrivial);
  fi;
end);

# same method for ideals

InstallMethod(IsSimpleSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local gens, lambdafunc, lambdarank, rank, opts, o, pos, iter, name, f;

  if HasIsRegularSemigroup(S) and not IsRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not regular");
    return false;
  elif HasIsCompletelyRegularSemigroup(S)
      and not IsCompletelyRegularSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not completely regular");
    return false;
  elif HasNrDClasses(S) then
    return NrDClasses(S) = 1;
  elif HasGeneratorsOfSemigroup(S) then
    gens := GeneratorsOfSemigroup(S); #not GeneratorsOfMonoid!
    lambdafunc := LambdaFunc(S);
    lambdarank := LambdaRank(S);
    rank := lambdarank(lambdafunc(gens[1]));

    if not ForAll([2 .. Length(gens)],
                  i -> lambdarank(lambdafunc(gens[i])) = rank) then
      return false;
    fi;

    opts := rec(treehashsize := SEMIGROUPS_OptionsRec(S).hashlen.M);

    for name in RecNames(LambdaOrbOpts(S)) do
      opts.(name) := LambdaOrbOpts(S).(name);
    od;

    for f in gens do
      o := Orb(S, LambdaFunc(S)(f), LambdaAct(S), opts);
      pos := LookForInOrb(o, function(o, x)
                               return LambdaRank(S)(x) < rank;
                             end, 1);
        #or LambdaRank(S)(LambdaAct(S)(x, f))<>LambdaRank(S)(x); end, 1);
      if pos <> false then
        return false;
      fi;
    od;

    return true;
  else #regular ideal case
    iter := IteratorOfDClasses(S);
    NextIterator(iter);
    return IsDoneIterator(iter);
  fi;

end);

# same method for ideals

InstallMethod(IsSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsGroupAsSemigroup);

# different method for ideals

InstallMethod(IsTrivial, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local gens;
  if HasSize(S) and Size(S) = 1 then
    return true;
  fi;
  gens := GeneratorsOfSemigroup(S);
  return ForAll(gens, x -> gens[1] = x) and IsIdempotent(gens[1]);
end);

# same method for ideals

InstallMethod(IsUnitRegularSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local g, perm_g, o, scc, graded, tester, gens, rhofunc, dom, rho, m, j;

  if not IsRegularSemigroup(S) then
    return false;
  fi;

  g := GroupOfUnits(S);

  if g = fail then
    return false;
  elif IsTrivial(g) then #JDM is this any better than the below?
    return IsBand(S);
  elif IsSemigroupIdeal(S) then
    return IsUnitRegularSemigroup(SupersemigroupOfIdeal(S));
  fi;

  perm_g := Range(IsomorphismPermGroup(g));
  o := LambdaOrb(S);
  scc := OrbSCC(o);
  graded := GradedLambdaOrbs(g);
  tester := IdempotentTester(S);
  gens := o!.gens;
  rhofunc := RhoFunc(S);

  for m in [2 .. Length(scc)] do
    dom := Union(Orbits(perm_g, o[scc[m][1]], OnPoints));
    if not IsSubgroup(Action(perm_g, dom),
                      Action(LambdaOrbSchutzGp(o, m), o[scc[m][1]])) then
      return false;
    elif Length(scc[m]) > 1 then
      rho := rhofunc(EvaluateWord(gens,
                                  TraceSchreierTreeForward(o, scc[m][1])));
      for j in scc[m] do
        if not o[j] in graded then
          if not ForAny(GradedLambdaOrb(g, o[j], true),
                        x -> tester(x, rho)) then
            return false;
          fi;
        fi;
      od;
    fi;
  od;
  return true;
end);

# same method for ideals

InstallMethod(IsZeroGroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)

  if HasParent(S) and HasIsZeroGroup(Parent(S)) and IsZeroGroup(Parent(S)) then
    return true;
  fi;

  if MultiplicativeZero(S) = fail then
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

  if NrHClasses(S) = 2 then
    return ForAll(GreensHClasses(S), IsGroupHClass);
  fi;

  Info(InfoSemigroups, 2, "the semigroup has more than two H-classes");
  return false;
end);

# same method for ideals

InstallMethod(IsZeroRectangularBand, "for an acting semigroup",
[IsActingSemigroup],
function(S)

  if HasParent(S) and HasIsZeroRectangularBand(Parent(S))
      and IsZeroRectangularBand(Parent(S)) then
    return true;
  elif not IsZeroSimpleSemigroup(S) then
    Info(InfoSemigroups, 2, "the semigroup is not 0-simple");
    return false;
  fi;

  return IsHTrivial(S);
end);

# different method for ideals

InstallMethod(IsZeroSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  local z, gens, m, i, j;

  if HasParent(S) and HasIsZeroSemigroup(Parent(S))
      and IsZeroSemigroup(Parent(S)) then
    return true;
  fi;

  z := MultiplicativeZero(S);

  if z = fail then
    Info(InfoSemigroups, 2, "the semigroup does not have a zero");
    return false;
  fi;

  gens := GeneratorsOfSemigroup(S);
  m := Length(gens);
  for i in [1 .. m] do
    for j in [1 .. m] do
      if not gens[i] * gens[j] = z then
        Info(InfoSemigroups, 2, "the product of generators ", i, " and ", j,
             " is not the multiplicative zero \n", z);
        return false;
      fi;
    od;
  od;

  return true;
end);

# same method for ideals

InstallMethod(IsZeroSemigroup, "for an inverse semigroup",
[IsInverseSemigroup], IsTrivial);

# same method for ideals

InstallMethod(IsZeroSimpleSemigroup, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, D;

  if MultiplicativeZero(S) = fail then
    return false;
  fi;
  if IsClosedData(SemigroupData(S)) then
    return IsRegularSemigroup(S) and NrDClasses(S) = 2;
  fi;
  iter := IteratorOfDClasses(S);
  D := NextIterator(iter);
  if IsDoneIterator(iter) or not IsRegularDClass(D) then
    return false;
  fi;
  D := NextIterator(iter);
  return IsDoneIterator(iter) and IsRegularDClass(D);
end);

# same method for ideals

InstallMethod(IsZeroSimpleSemigroup, "for an inverse semigroup",
[IsInverseSemigroup],
function(S)
  return MultiplicativeZero(S) <> fail and NrDClasses(S) = 2;
end);

#same method for ideals

InstallMethod(IsCongruenceFreeSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local t, p, rowsDiff;

  rowsDiff := function(p)
    local i, j;
    for i in [1 .. Size(p) - 1] do
      for j in [i + 1 .. Size(p)] do
        if p[i] = p[j] then
          return false;
        fi;
      od;
    od;
    return true;
  end;

  if Size(S) <= 2 then
    return true;
  fi;

  if MultiplicativeZero(S) <> fail then
    # CASE 1: s has zero
    if IsZeroSimpleSemigroup(S) then
      # Find an isomorphic RMS
      t := Range(IsomorphismReesMatrixSemigroup(S));
      if IsTrivial(UnderlyingSemigroup(t)) then
        # Check that no two rows or columns are identical
        p := Matrix(t);
        if rowsDiff(p) and rowsDiff(TransposedMat(p)) then
          return true;
        fi;
      fi;
    fi;
    return false;
  else
    # CASE 2: s has no zero
    return IsGroup(S) and IsSimpleGroup(S);
  fi;
end);

# same method for ideals

InstallMethod(IsEUnitaryInverseSemigroup,
"for an inverse semigroup which has NaturalLeqInverseSemigroup function",
[IsInverseSemigroup],
function(S)
  if not IsPartialPermSemigroup(S) and not IsBlockBijectionSemigroup(S)
      and not IsPartialPermBipartitionSemigroup(S) then
    TryNextMethod(); # FIXME is there one? if not, then just return an error
                     # here or better still take an isomorphism to a partial
                     # perm semigroup and answer the question there.
  fi;
  return IsMajorantlyClosed(S, IdempotentGeneratedSubsemigroup(S));
end);

InstallMethod(IsSemigroupWithAdjoinedZero,
"for a semigroup",
[IsSemigroup],
x -> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(x) <> fail);
