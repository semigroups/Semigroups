#############################################################################
##
#W  semigroups.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for finite semigroups which do not depend on
# whether they are acting or not, i.e. they should work for all semigroups.

InstallMethod(IsGeneratorsOfInverseSemigroup,
"for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  if IsSemigroupWithInverseOp(S) then
    return true;
  fi;
  return IsGeneratorsOfInverseSemigroup(GeneratorsOfSemigroup(S));
end);

# star

InstallMethod(Star, "for an associative element with star",
[IsAssociativeElementWithStar],
function(elm)
  elm := StarOp(elm);
  MakeImmutable(elm);
  return elm;
end);

# basic things

InstallMethod(Generators, "for a semigroup",
[IsSemigroup],
function(S)

  if HasGeneratorsOfMagmaIdeal(S) then
    return GeneratorsOfMagmaIdeal(S);
  elif HasGeneratorsOfGroup(S) then
    return GeneratorsOfGroup(S);
  elif HasGeneratorsOfInverseMonoid(S) then
    return GeneratorsOfInverseMonoid(S);
  elif HasGeneratorsOfInverseSemigroup(S) then
    return GeneratorsOfInverseSemigroup(S);
  elif HasGeneratorsOfMonoid(S) then
    return GeneratorsOfMonoid(S);
  fi;

  return GeneratorsOfSemigroup(S);
end);

#

InstallMethod(ViewString, "for a group of bipartitions",
[IsBipartitionSemigroup and IsGroupAsSemigroup],
function(s)
  local str, nrgens;
  if IsGroup(s) then
    TryNextMethod();
  fi;
  str := "\><";
  if HasIsTrivial(s) and IsTrivial(s) then
    Append(str, "\>trivial\< ");
  fi;

  Append(str, "\>bipartition\< \>group\< ");
  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s)
      and Size(s) < 2 ^ 64 then
    Append(str, "\>of size\> ");
    Append(str, String(Size(s)));
    Append(str, ",\<\< ");
  fi;

  nrgens := Length(Generators(s));

  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfBipartitionSemigroup(s)));
  Append(str, "\< pts with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(ViewString, "for a group of partial perms",
[IsPartialPermSemigroup and IsGroupAsSemigroup],
function(s)
  local str, nrgens;

  str := "\><";
  if HasIsTrivial(s) and IsTrivial(s) then
    Append(str, "\>trivial\< ");
  fi;

  Append(str, "\>partial perm\< \>group\< ");
  if HasIsTrivial(s) and not IsTrivial(s)
      and HasSize(s) and Size(s) < 2 ^ 64 then
    Append(str, "\>of size\> ");
    Append(str, String(Size(s)));
    Append(str, ",\<\< ");
  fi;

  nrgens := Length(Generators(s));

  Append(str, "\>on \>");
  Append(str, ViewString(RankOfPartialPermSemigroup(s)));
  Append(str, "\< pts with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

# creating semigroups, monoids, inverse semigroups, etc

InstallMethod(MagmaByGenerators, "for an associative element collection",
[IsAssociativeElementCollection], SemigroupByGenerators);

#

InstallMethod(SemigroupByGenerators, "for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
  return SemigroupByGenerators(coll, SEMIGROUPS_DefaultOptionsRec);
end);

#

InstallMethod(SemigroupByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, opts)
  local n, i, S, filts, pos, x;

  opts := SEMIGROUPS_ProcessOptionsRec(opts);
  gens := AsList(gens);

  # try to find a smaller generating set
  if opts.small and Length(gens) > 1 then
    gens := Shuffle(SSortedList(gens)); #remove duplicates, permute
    if IsGeneratorsOfActingSemigroup(gens) then
      n := ActionDegree(gens);
      Sort(gens, function(x, y)
                   return ActionRank(x, n) > ActionRank(y, n);
                 end);
      #remove the identity
      if IsOne(gens[1]) and IsBound(gens[2])
          and ActionRank(gens[2], n) = n then
        Remove(gens, 1);
      fi;
    else
      Sort(gens, IsGreensDLeq(Semigroup(gens)));
      if IsMultiplicativeElementWithOneCollection(gens) and IsOne(gens[1]) and
          IsBound(gens[2]) and gens[1] in Semigroup(gens[2]) then
        Remove(gens, 1);
      fi;
    fi;

    opts := ShallowCopy(opts);
    opts.small := false;
    opts.regular := false;
    S := Semigroup(gens[1], opts);

    if InfoLevel(InfoSemigroups) > 1 then
      n := Length(gens);
      for i in [2 .. n] do
        if not gens[i] in S then
          S := ClosureSemigroupNC(S, [gens[i]], opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(S)),
              " generators so far\r");
      od;
      Print("\n");
    else
      for x in gens do
        S := ClosureSemigroup(S, x, opts);
      od;
    fi;
    return S;
  fi;

  filts := IsSemigroup and IsAttributeStoringRep;

  if not opts.generic and IsGeneratorsOfActingSemigroup(gens) then
    filts := filts and IsActingSemigroup;
  fi;

  S := Objectify(NewType(FamilyObj(gens), filts), rec(opts := opts));

  if opts.regular then
    SetIsRegularSemigroup(S, true);
  fi;

  SetGeneratorsOfMagma(S, gens);

  if IsMultiplicativeElementWithOneCollection(gens)
      and CanEasilyCompareElements(gens) then
    pos := Position(gens, One(gens));
    if pos <> fail then
      SetFilterObj(S, IsMonoid);
      gens := ShallowCopy(gens);
      Remove(gens, pos);
      SetGeneratorsOfMonoid(S, gens);
    fi;
  fi;

  return S;
end);

#

InstallMethod(MonoidByGenerators, "for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  return MonoidByGenerators(gens, SEMIGROUPS_DefaultOptionsRec);
end);

#

InstallMethod(MonoidByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, opts)
  local n, S, filts, pos, i, x;

  opts := SEMIGROUPS_ProcessOptionsRec(opts);
  gens := ShallowCopy(gens);

  if opts.small and Length(gens) > 1 then #small gen. set
    gens := Shuffle(SSortedList(gens));
    if IsGeneratorsOfActingSemigroup(gens) then
      n := ActionDegree(gens);
      Sort(gens, function(x, y)
                   return ActionRank(x, n) > ActionRank(y, n);
                 end);

      if IsOne(gens[1]) and IsBound(gens[2])
          and ActionRank(gens[2], n) = n then
        #remove id
        Remove(gens, 1);
      fi;
    else
      Sort(gens, IsGreensDLeq(Semigroup(gens)));
      if IsOne(gens[1]) and IsBound(gens[2])
          and gens[1] in Semigroup(gens[2]) then
        Remove(gens, 1);
      fi;
    fi;

    opts := ShallowCopy(opts);
    opts.small := false;
    opts.regular := false;
    S := Monoid(gens[1], opts);

    if InfoLevel(InfoSemigroups) > 1 then
      n := Length(gens);
      for i in [2 .. n] do
        if not gens[i] in S then
          S := ClosureSemigroupNC(S, [gens[i]], opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(S)),
              " generators so far");
      od;
      Print("\n");
    else
      for x in gens do
        S := ClosureSemigroup(S, x, opts);
      od;
    fi;
    return S;
  fi;

  filts := IsMonoid and IsAttributeStoringRep;

  if not opts.generic and IsGeneratorsOfActingSemigroup(gens) then
    filts := filts and IsActingSemigroup;
  fi;

  S := Objectify(NewType(FamilyObj(gens), filts), rec(opts := opts));

  if opts.regular then
    SetIsRegularSemigroup(S, true);
  fi;

  # remove one from gens if it's there.
  if CanEasilyCompareElements(gens) then
    pos := Position(gens, One(gens));
    if pos <> fail then
      SetGeneratorsOfMagma(S, AsList(gens));
      gens := ShallowCopy(gens);
      Remove(gens, pos);
    else
      SetGeneratorsOfMagma(S, Concatenation([One(gens)], gens));
    fi;
  fi;
  SetGeneratorsOfMagmaWithOne(S, gens);
  return S;
end);

#

InstallMethod(InverseMonoidByGenerators,
"for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  return InverseMonoidByGenerators(gens, SEMIGROUPS_DefaultOptionsRec);
end);

#

InstallMethod(InverseSemigroupByGenerators,
"for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  return InverseSemigroupByGenerators(gens, SEMIGROUPS_DefaultOptionsRec);
end);

#

InstallMethod(InverseMonoidByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection and IsMultiplicativeElementWithOneCollection,
 IsRecord],
function(gens, opts)
  local n, S, filts, one, pos, x;

  if not IsGeneratorsOfInverseSemigroup(gens) then
    Error("Semigroups: InverseMonoidByGenerators: usage,\n",
          "the first argument must satisfy `IsGeneratorsOfInverseSemigroup',");
    return;
  fi;

  opts := SEMIGROUPS_ProcessOptionsRec(opts);

  if opts.small and Length(gens) > 1 then
    gens := Shuffle(Set(gens));
    if IsGeneratorsOfActingSemigroup(gens) then
      n := ActionDegree(gens);
      Sort(gens, function(x, y)
                   return ActionRank(x, n) > ActionRank(y, n);
                 end);
    fi;
    opts := ShallowCopy(opts);
    opts.small := false;
    S := InverseMonoid(gens[1], opts);

    for x in gens do
      S := ClosureInverseSemigroup(S, x, opts);
    od;
    return S;
  fi;

  filts := IsMagmaWithOne and IsInverseSemigroup and IsAttributeStoringRep;

  if not opts.generic and IsGeneratorsOfActingSemigroup(gens) then
    filts := filts and IsActingSemigroup;
  fi;

  S := Objectify(NewType(FamilyObj(gens), filts), rec(opts := opts));
  one := One(gens);
  SetOne(S, one);
  pos := Position(gens, one);
  # FIXME shouldn't we check that we can easily compare the gens?

  if pos <> fail then
    SetGeneratorsOfInverseSemigroup(S, gens);
    gens := ShallowCopy(gens);
    Remove(gens, pos);
    SetGeneratorsOfInverseMonoid(S, gens);
  else
    SetGeneratorsOfInverseMonoid(S, gens);
    gens := ShallowCopy(gens);
    Add(gens, one);
    SetGeneratorsOfInverseSemigroup(S, gens);
  fi;

  return S;
end);

#

InstallMethod(InverseSemigroupByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, opts)
  local n, S, filts, pos, x;

  if not IsGeneratorsOfInverseSemigroup(gens) then
    Error("Semigroups: InverseSemigroupByGenerators: usage,\n",
          "the first argument must satisfy `IsGeneratorsOfInverseSemigroup',");
    return;
  fi;

  opts := SEMIGROUPS_ProcessOptionsRec(opts);

  if opts.small and Length(gens) > 1 then
    gens := Shuffle(Set(gens));
    if IsGeneratorsOfActingSemigroup(gens) then
      n := ActionDegree(gens);
      Sort(gens, function(x, y)
                   return ActionRank(x, n) > ActionRank(y, n);
                 end);
    fi;

    opts := ShallowCopy(opts);
    opts.small := false;

    S := InverseSemigroup(gens[1], opts);
    for x in gens do
      if not x in S then
        S := ClosureInverseSemigroupNC(S, [x], opts);
      fi;
    od;
    return S;
  fi;

  filts := IsMagma and IsInverseSemigroup and IsAttributeStoringRep;

  if not opts.generic and IsGeneratorsOfActingSemigroup(gens) then
    filts := filts and IsActingSemigroup;
  fi;

  S := Objectify(NewType(FamilyObj(gens), filts), rec(opts := opts));
  SetGeneratorsOfInverseSemigroup(S, AsList(gens));

  if IsMultiplicativeElementWithOneCollection(gens) then
    pos := Position(gens, One(gens));
    if pos <> fail then
      SetFilterObj(S, IsMonoid);
      gens := ShallowCopy(gens);
      Remove(gens, pos);
      SetGeneratorsOfInverseMonoid(S, gens);
    fi;
  fi;

  return S;
end);

# closure

InstallMethod(ClosureInverseSemigroup,
"for a semigroup with inverse op and associative element coll.",
[IsSemigroupWithInverseOp, IsAssociativeElementCollection],
function(S, coll) #FIXME is the ShallowCopy really necessary?
  return ClosureInverseSemigroup(S,
                                 coll,
                                 ShallowCopy(SEMIGROUPS_OptionsRec(S)));
end);

#

InstallMethod(ClosureInverseSemigroup,
"for a semigroup with inverse op and an associative element",
[IsSemigroupWithInverseOp, IsAssociativeElement],
function(S, x) #FIXME is the ShallowCopy really necessary?
  return ClosureInverseSemigroup(S,
                                 [x],
                                 ShallowCopy(SEMIGROUPS_OptionsRec(S)));
end);

#

InstallMethod(ClosureInverseSemigroup,
"for semigroup with inverse op, associative element, record",
[IsSemigroupWithInverseOp, IsAssociativeElement, IsRecord],
function(S, x, opts)
  return ClosureInverseSemigroup(S, [x], opts);
end);

#

InstallMethod(ClosureInverseSemigroup,
"for a semigroup with inverse op, associative elt coll, and record",
[IsSemigroupWithInverseOp, IsAssociativeElementCollection, IsRecord],
function(S, coll, opts)

  if IsEmpty(coll) then
    return S;
  fi;

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(Representative(coll)) then
    Error("Semigroups: ClosureInverseSemigroup: usage,\n",
          "the semigroup and collection of elements are not of the same ",
          "type,");
    return;
  fi;

  if not IsGeneratorsOfInverseSemigroup(coll) then
    Error("Semigroups: ClosureInverseSemigroup: usage,\n",
          "the first argument must satisfy `IsGeneratorsOfInverseSemigroup',");
    return;
  fi;

  if IsSemigroup(coll) then
    coll := GeneratorsOfSemigroup(coll);
  fi;

  coll := Set(coll);

  return ClosureInverseSemigroupNC(S,
                                   Filtered(coll, x -> not x in S),
                                   SEMIGROUPS_ProcessOptionsRec(opts));
end);

#

InstallGlobalFunction(ClosureInverseSemigroupNC,
function(S, coll, opts)
  local gens, T, o, n, x;

  if coll = [] then
    Info(InfoSemigroups, 2, "the elements in the collection belong to the ",
         "semigroup,");
    return S;
  elif not IsActingSemigroup(S) or IsSemigroupIdeal(S) then
    return InverseSemigroup(S, coll, opts);
  fi;

  if Length(coll) = 1 then
    gens := GeneratorsOfInverseSemigroup(S);
    T := InverseSemigroupByGenerators(Concatenation(gens, coll), opts);

    if not IsIdempotent(coll[1]) then
      Add(coll, coll[1] ^ -1);
    fi;

    o := StructuralCopy(LambdaOrb(S));
    AddGeneratorsToOrbit(o, coll);

    #remove everything related to strongly connected components
    Unbind(o!.scc);
    Unbind(o!.trees);
    Unbind(o!.scc_lookup);
    Unbind(o!.mults);
    Unbind(o!.schutz);
    Unbind(o!.reverse);
    Unbind(o!.rev);
    Unbind(o!.truth);
    Unbind(o!.schutzstab);
    Unbind(o!.exhaust);
    Unbind(o!.factors);

    o!.parent := T;
    o!.scc_reps := [FakeOne(Generators(T))];

    SetLambdaOrb(T, o);
    return T;
  fi;

  Shuffle(coll);
  n := ActionDegree(coll);
  Sort(coll, function(x, y)
               return ActionRank(x, n) > ActionRank(y, n);
             end);

  opts.small := false;

  for x in coll do
    if not x in S then
      S := ClosureInverseSemigroupNC(S, [x], opts);
    fi;
  od;

  return S;
end);

#

InstallMethod(ClosureSemigroup,
"for a semigroup and associative element collection",
[IsSemigroup, IsAssociativeElementCollection],
function(S, coll) #FIXME: ShallowCopy?
  return ClosureSemigroup(S, coll, ShallowCopy(SEMIGROUPS_OptionsRec(S)));
end);

#

InstallMethod(ClosureSemigroup, "for a semigroup and associative element",
[IsSemigroup, IsAssociativeElement],
function(S, x) #FIXME: ShallowCopy
  return ClosureSemigroup(S, [x], ShallowCopy(SEMIGROUPS_OptionsRec(S)));
end);

#

InstallMethod(ClosureSemigroup,
"for a semigroup, associative element, and record",
[IsSemigroup, IsAssociativeElement, IsRecord],
function(S, x, opts)
  return ClosureSemigroup(S, [x], opts);
end);

#

InstallMethod(ClosureSemigroup,
"for a semigroup, associative element collection, and record",
[IsSemigroup, IsAssociativeElementCollection, IsRecord],
function(S, coll, opts)

  if IsEmpty(coll) then
    return S;
  fi;

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(Representative(coll)) then
    Error("Semigroups: ClosureSemigroup: usage,\n",
          "the semigroup and collection of elements are not of the same ",
          "type,");
    return;
  fi;

  if IsActingSemigroup(S)
      and IsActingSemigroupWithFixedDegreeMultiplication(S)
      and ActionDegree(S) <> ActionDegree(Representative(coll)) then
    Error("Semigroups: ClosureSemigroup: usage,\n",
          "the degree of the semigroup and collection must be equal,");
    return;
  fi;

  if IsSemigroup(coll) then
    coll := GeneratorsOfSemigroup(coll); #JDM: was just Generators, ok?
  fi;

  opts.small := false;

  return ClosureSemigroupNC(S,
                            Filtered(coll, x -> not x in S), # FIXME don't do this
                            SEMIGROUPS_ProcessOptionsRec(opts));
end);

#recreate the lambda/rho orb using the higher degree!
BindGlobal("SEMIGROUPS_ChangeDegree", # for a transformation semigroup
function(o, old_deg, t)
  local deg, extra, ht, max, i, orb;
  deg := DegreeOfTransformationSemigroup(t);
  orb := o!.orbit;
  if IsLambdaOrb(o) then
    # rehash the orbit values
    extra := [old_deg + 1 .. deg];
    ht := HTCreate(o[1], rec(treehashsize := o!.treehashsize));
    #JDM: could make the treehashsize bigger if needed here!
    HTAdd(ht, o[1], 1);
    for i in [2 .. Length(o)] do
      orb[i] := ShallowCopy(o[i]);
      Append(o[i], extra);
      HTAdd(ht, o[i], i);
    od;
    Unbind(o!.ht);
    o!.ht := ht;

    # change the action of <o> to that of <t>
    o!.op := LambdaAct(t);
  elif IsRhoOrb(o) then
    ht := HTCreate(o[1], rec(treehashsize := o!.treehashsize));
    #JDM: could make the treehashsize bigger if needed here!
    HTAdd(ht, o[1], 1);
    for i in [2 .. Length(o)] do
      orb[i] := ShallowCopy(o[i]);
      if not IsEmpty(o[i]) then
        max := MaximumList(o[i]); #nr kernel classes
      else
        max := 0;
      fi;
      Append(o[i], [max + 1 .. max + deg - old_deg]);
      HTAdd(ht, o[i], i);
    od;
    Unbind(o!.ht);
    o!.ht := ht;

    # change the action of <o> to that of <t>
    o!.op := RhoAct(t);
  fi;
  return o;
end);

# this is the fallback method, coll should consist of elements not in 
# the semigroup

InstallMethod(ClosureSemigroupNC, 
"for a semigroup, associative element collection, and record",
[IsSemigroup, IsAssociativeElementCollection, IsRecord],
function(S, coll, opts)
  local data, T;

  if SEMIGROUPS_IsCCSemigroup(S) then 
    data := rec();
    data.gens := ShallowCopy(coll);
    data.nr := 0;
    data.pos := 0;
    # the degree is the length of the std::vector required to hold the object
    data.degree := SEMIGROUPS_DegreeOfSemigroup(S, coll);
    data.report := SEMIGROUPS_OptionsRec(S).report;
    data := Objectify(NewType(FamilyObj(S), IsGenericSemigroupData and IsMutable
                                            and IsAttributeStoringRep), data);
    CLOSURE_SEMIGROUP(GenericSemigroupData(S), data);
    T := Semigroup(data!.gens, opts);
    SetGenericSemigroupData(T, data);
    data!.genstoapply := [1 .. Length(GeneratorsOfSemigroup(T))];
    return T;
  else 
    Info(InfoWarning, 1, "using default method for ClosureSemigroupNC");
    return Semigroup(S, coll, opts);
  fi;
end);

InstallMethod(ClosureSemigroupNC, 
"for a semigroup, empty collection, and record",
[IsSemigroup, IsListOrCollection and IsEmpty, IsRecord],
function(S, coll, opts)
  return S;
end);

#subsemigroups

# <limit> is the max size of the subsemigroup.

InstallMethod(SubsemigroupByProperty,
"for a semigroup, function, and positive integer",
[IsSemigroup, IsFunction, IsPosInt],
function(S, func, limit)
  local iter, T, f;

  iter := Iterator(S);

  repeat
    f := NextIterator(iter);
  until func(f) or IsDoneIterator(iter);

  if not func(f) then
    return fail; # should really return the empty semigroup
  fi;

  T := Semigroup(f);

  while Size(T) < limit and not IsDoneIterator(iter) do
    f := NextIterator(iter);
    if func(f) then
      T := ClosureSemigroup(T, f);
    fi;
  od;
  SetParent(T, S);
  return T;
end);

# <limit> is the max size of the subsemigroup.

InstallMethod(InverseSubsemigroupByProperty,
"for a semigroup with inverse op, function, positive integer",
[IsSemigroupWithInverseOp, IsFunction, IsPosInt],
function(S, func, limit)
  local iter, T, f;

  iter := Iterator(S);

  repeat
    f := NextIterator(iter);
  until func(f) or IsDoneIterator(iter);

  if not func(f) then
    return fail; # should really return the empty semigroup
  fi;

  T := InverseSemigroup(f);

  while Size(T) < limit and not IsDoneIterator(iter) do
    f := NextIterator(iter);
    if func(f) then
      T := ClosureInverseSemigroup(T, f);
    fi;
  od;
  SetParent(T, S);
  return T;
end);

#

InstallMethod(SubsemigroupByProperty, "for a semigroup and function",
[IsSemigroup, IsFunction],
function(S, func)
  return SubsemigroupByProperty(S, func, Size(S));
end);

#

InstallMethod(InverseSubsemigroupByProperty,
"for semigroup with inverse op and function",
[IsSemigroupWithInverseOp, IsFunction],
function(S, func)
  return InverseSubsemigroupByProperty(S, func, Size(S));
end);

#miscellaneous

InstallGlobalFunction(RegularSemigroup,
function(arg)
  if not IsRecord(arg[Length(arg)]) then
    Add(arg, rec(regular := true));
  else
    arg[Length(arg)].regular := true;
  fi;
  return CallFuncList(Semigroup, arg);
end);

#random

InstallMethod(Random,
"for a semigroup with AsList",
[IsSemigroup and HasAsList],
20, # to beat other random methods
function(S)
  return AsList(S)[Random([1 .. Size(S)])];
end);

#

InstallMethod(RandomMatrixSemigroup,
"for a ring, positive integer and positive integer",
[IsRing, IsPosInt, IsPosInt],
function(R, m, n)
  return Semigroup(List([1 .. m], x -> RandomMat(n, n, R)));
end);

#

InstallMethod(RandomBinaryRelationSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  local s;

  s := Semigroup(List([1 .. m], x -> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true);
  return s;
end);

#

InstallMethod(RandomBlockGroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return Semigroup(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomPartialPermMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return Monoid(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return InverseMonoid(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return InverseSemigroup(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomTransformationSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return Semigroup(Set(List([1 .. m], x -> RandomTransformation(n))));
end);

#

InstallMethod(RandomTransformationMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return Monoid(Set(List([1 .. m], x -> RandomTransformation(n))));
end);

#

InstallMethod(RandomBipartitionSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return Semigroup(Set(List([1 .. m], x -> RandomBipartition(n))));
end);

#

InstallMethod(RandomBipartitionMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m, n)
  return Monoid(Set(List([1 .. m], x -> RandomBipartition(n))));
end);
