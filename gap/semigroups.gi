#############################################################################
##
#W  semigroups.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
function(s)

  if HasGeneratorsOfMagmaIdeal(s) then
    return GeneratorsOfMagmaIdeal(s);
  elif HasGeneratorsOfGroup(s) then
    return GeneratorsOfGroup(s);
  elif HasGeneratorsOfInverseMonoid(s) then
    return GeneratorsOfInverseMonoid(s);
  elif HasGeneratorsOfInverseSemigroup(s) then
    return GeneratorsOfInverseSemigroup(s);
  elif HasGeneratorsOfMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
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

ViewStringForGroupOfTransformations@ := function(s)
local str, nrgens;
  str := "\><";
  if HasIsTrivial(s) and IsTrivial(s) then
    Append(str, "\>trivial\< ");
  fi;

  Append(str, "\>transformation\< \>group\<");
  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s)
   and Size(s) < 2 ^ 64 then
    Append(str, " \>of size \>");
    Append(str, String(Size(s)));
    Append(str, ",\<\<");
  fi;

  nrgens := Length(Generators(s));
  if DegreeOfTransformationSemigroup(s) > 0 then
    Append(str, " \>on \>");
    Append(str, ViewString(DegreeOfTransformationSemigroup(s)));
    Append(str, "\< pts");
  fi;
  if nrgens > 0 then
    Append(str, " with\> ");
    Append(str, ViewString(nrgens));
    Append(str, "\< generator");
    if nrgens > 1 or nrgens = 0 then
      Append(str, "s\<");
    else
      Append(str, "\<");
    fi;
  fi;
  Append(str, ">\<");

  return str;
end;

InstallMethod(ViewString, "for a group of transformations",
[IsTransformationSemigroup and IsGroupAsSemigroup],
ViewStringForGroupOfTransformations@);

InstallMethod(ViewString, "for a group of transformations",
[IsTransformationSemigroup and IsGroup],
ViewStringForGroupOfTransformations@);

Unbind(ViewStringForGroupOfTransformations@);

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
[IsAssociativeElementCollection],
function(coll)
  if IsGeneratorsOfActingSemigroup(coll) then
    return SemigroupByGenerators(coll);
  else
    TryNextMethod();
  fi;
end);

#

InstallMethod(SemigroupByGenerators, "for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
   if IsGeneratorsOfActingSemigroup(gens) then
     return SemigroupByGenerators(gens, SemigroupsOptionsRec);
   else
     TryNextMethod();
    fi;
end);

#

InstallMethod(SemigroupByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, opts)
  local deg, n, i, closure_opts, s, filts, pos, f;

  if not IsGeneratorsOfActingSemigroup(gens) then
    TryNextMethod();
  fi;

  opts := SemigroupOptions(opts);
  gens := AsList(gens);

  # try to find a smaller generating set
  if opts.small and Length(gens) > 1 then
    gens := SSortedList(gens); #remove duplicates
    gens := Permuted(gens, Random(SymmetricGroup(Length(gens))));
    n := ActionDegree(gens);
    Sort(gens, function(x, y)
                 return ActionRank(x, n) > ActionRank(y, n);
               end);

    #remove the identity
    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2], n) = n then
      Remove(gens, 1);
    fi;

    i := 0;
    closure_opts := rec(small := false, hashlen := opts.hashlen);
    s := Semigroup(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups) > 1 then
      n := Length(gens);
      for i in [2 .. n] do
        if not gens[i] in s then
          s := ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far\r");
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then
          s := ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;

  filts := IsSemigroup and IsAttributeStoringRep;

  if opts.acting then
    filts := filts and IsActingSemigroup;
  fi;

  s := Objectify( NewType( FamilyObj( gens ), filts ), rec(opts := opts));

  if opts.regular then
    SetIsRegularSemigroup(s, true);
  fi;

  SetGeneratorsOfMagma(s, gens);

  if IsMultiplicativeElementWithOneCollection(gens)
   and CanEasilyCompareElements(gens) then
    pos := Position(gens, One(gens));
    if pos <> fail then
      SetFilterObj(s, IsMonoid);
      gens := ShallowCopy(gens);
      Remove(gens, pos);
      SetGeneratorsOfMonoid(s, gens);
    fi;
  fi;

  return s;
end);

#

InstallMethod(MonoidByGenerators, "for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  if IsGeneratorsOfActingSemigroup(gens) then
    return MonoidByGenerators(gens, SemigroupsOptionsRec);
  else
    TryNextMethod();
  fi;
end);

#

InstallMethod(MonoidByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, record)
  local deg, n, i, closure_opts, s, filts, pos, f;

  if not IsGeneratorsOfActingSemigroup(gens) then
    TryNextMethod();
  fi;

  record := SemigroupOptions(record);
  gens := ShallowCopy(gens);

  if record.small and Length(gens) > 1 then #small gen. set
    gens := SSortedList(gens); #remove duplicates
    gens := Permuted(gens, Random(SymmetricGroup(Length(gens))));
    n := ActionDegree(gens);
    Sort(gens, function(x, y)
                 return ActionRank(x, n) > ActionRank(y, n);
               end);

    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2], n) = n then
      #remove id
      Remove(gens, 1);
    fi;

    i := 0;
    closure_opts := rec(small := false, hashlen := record.hashlen);
    s := Monoid(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups) > 1 then
      n := Length(gens);
      for i in [1 .. n] do
        if not gens[i] in s then
          s := ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then
          s := ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;

  filts := IsMonoid and IsAttributeStoringRep;

  if record.acting then
    filts := filts and IsActingSemigroup;
  fi;

  s := Objectify( NewType( FamilyObj( gens ), filts ), rec(opts := record));

  if record.regular then
    SetIsRegularSemigroup(s, true);
  fi;

  # remove one from gens if it's there.
  if CanEasilyCompareElements(gens) then
    pos := Position(gens, One(gens));
    if pos <> fail then
      SetGeneratorsOfMagma(s, AsList(gens));
      gens := ShallowCopy(gens);
      Remove(gens, pos);
    else
      SetGeneratorsOfMagma(s, Concatenation([One(gens)], gens));
    fi;
  fi;
  SetGeneratorsOfMagmaWithOne( s, gens );
  return s;
end);

#

InstallMethod(InverseMonoidByGenerators,
"for an associative element collection",
[IsAssociativeElementCollection],
function(gens)

  if IsGeneratorsOfActingSemigroup(gens) then
    return InverseMonoidByGenerators(gens, SemigroupsOptionsRec);
  else
    TryNextMethod();
  fi;
end);

#

InstallMethod(InverseSemigroupByGenerators,
"for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  if IsGeneratorsOfActingSemigroup(gens) then
    return InverseSemigroupByGenerators(gens, SemigroupsOptionsRec);
  else
    TryNextMethod();
  fi;
end);

#

InstallMethod(InverseMonoidByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, record)
  local n, closure_opts, s, filts, one, pos, f;

  if not IsGeneratorsOfActingSemigroup(gens) then
    TryNextMethod();
  fi;

  record := SemigroupOptions(record);

  if record.small and Length(gens) > 1 then
    gens := SSortedList(ShallowCopy(gens));
    gens := Permuted(gens, Random(SymmetricGroup(Length(gens))));
    n := ActionDegree(gens);
    Sort(gens, function(x, y)
                 return ActionRank(x,n) > ActionRank(y,n);
               end);

    closure_opts := rec(small := false, hashlen := record.hashlen);
    s := InverseMonoid(gens[1], closure_opts);

    for f in gens do
      if not f in s then
        s := ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;

  filts := IsMagmaWithOne and IsInverseSemigroup and IsAttributeStoringRep;

  if record.acting then
    filts := filts and IsActingSemigroupWithInverseOp;
  fi;

  s := Objectify( NewType (FamilyObj( gens ), filts), rec(opts := record));

  one := One(gens);
  SetOne(s, one);
  pos := Position(gens, one);

  if pos <> fail then
    SetGeneratorsOfInverseSemigroup(s, gens);
    gens := ShallowCopy(gens);
    Remove(gens, pos);
    SetGeneratorsOfInverseMonoid(s, gens);
  else
    SetGeneratorsOfInverseMonoid(s, gens);
    gens := ShallowCopy(gens);
    Add(gens, one);
    SetGeneratorsOfInverseSemigroup(s, gens);
  fi;

  return s;
end);

#

InstallMethod(InverseSemigroupByGenerators,
"for an associative element collection and record",
[IsAssociativeElementCollection, IsRecord],
function(gens, record)
  local n, closure_opts, s, filts, pos, f;

  if not IsGeneratorsOfActingSemigroup(gens) then
    TryNextMethod();
  fi;

  record := SemigroupOptions(record);

  if record.small and Length(gens) > 1 then
    gens := SSortedList(ShallowCopy(gens));
    gens := Permuted(gens, Random(SymmetricGroup(Length(gens))));
    n := ActionDegree(gens);
    Sort(gens, function(x, y)
                 return ActionRank(x,n) > ActionRank(y,n);
               end);

    closure_opts := rec(small := false, hashlen := record.hashlen);
    s := InverseSemigroup(gens[1], closure_opts);
    for f in gens do
      if not f in s then
        s := ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;

  filts := IsMagma and IsInverseSemigroup and IsAttributeStoringRep;

  if record.acting then
    filts := filts and IsActingSemigroupWithInverseOp;
  fi;

  s := Objectify( NewType (FamilyObj( gens ), filts), rec(opts := record));
  SetGeneratorsOfInverseSemigroup(s, AsList(gens));

  if IsMultiplicativeElementWithOneCollection(gens) then
    pos := Position(gens, One(gens));
    if pos <> fail then
      SetFilterObj(s, IsMonoid);
      gens := ShallowCopy(gens);
      Remove(gens, pos);
      SetGeneratorsOfInverseMonoid(s, gens);
    fi;
  fi;
  return s;
end);

# closure

InstallMethod(ClosureInverseSemigroup,
"for acting semigroup with inverse op and associative element coll.",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection],
function(s, coll)
  return ClosureInverseSemigroup(s, coll, s!.opts);
end);

#

InstallMethod(ClosureInverseSemigroup,
"for acting semigroup with inverse op and an associative element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  return ClosureInverseSemigroup(s, [f], s!.opts);
end);

#

InstallMethod(ClosureInverseSemigroup,
"for acting semigroup with inverse op, associative element, record",
[IsActingSemigroupWithInverseOp, IsAssociativeElement, IsRecord],
function(s, f, record)
  return ClosureInverseSemigroup(s, [f], record);
end);

#

InstallMethod(ClosureInverseSemigroup,
"for an acting semigroup with inverse op, associative elt coll, and record",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection, IsRecord],
function(s, coll, record)
  local n;

  if not IsGeneratorsOfActingSemigroup(coll) then
    Error("Semigroups: ClosureInverseSemigroup: usage,\n",
          "the second argument <coll> should be a collection ",
          "satisfying\nIsGeneratorsOfActingSemigroup,");
    return;
  fi;

  if not ElementsFamily(FamilyObj(s)) = FamilyObj(Representative(coll)) then
    Error("Semigroups: ClosureInverseSemigroup: usage,\n",
          "the semigroup and collection of elements are not of the same type,");
    return;
  fi;

  if not IsGeneratorsOfInverseSemigroup(coll) then
    Error("Semigroups: ClosureInverseSemigroup: usage,\n",
          "<coll> is a collection of generators of an inverse semigroup,");
    return;
  fi;

  if IsSemigroup(coll) then
    coll := GeneratorsOfSemigroup(coll);
  fi;

  return ClosureInverseSemigroupNC(s, Filtered(coll, x -> not x in s),
   SemigroupOptions(record));
end);

#

InstallGlobalFunction(ClosureInverseSemigroupNC,
function(s, coll, record)
  local t, coll_copy, o, f;

  if coll = [] then
    Info(InfoSemigroups, 2, "the elements in the collection belong to the ",
    "semigroup,");
    return s;
  elif IsSemigroupIdeal(s) then
    return InverseSemigroup(s, coll, record);
  fi;

  coll_copy := Set(ShallowCopy(coll));
  for f in coll do
    if not f ^ - 1 in coll then
      Add(coll_copy, f ^ - 1);
    fi;
  od;

  o := StructuralCopy(LambdaOrb(s));
  AddGeneratorsToOrbit(o, coll_copy);

  #TODO should be a case split here for semigroups and monoids?
  t := InverseSemigroupByGenerators(
   Concatenation(GeneratorsOfInverseSemigroup(s), coll), record);

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

  o!.parent := t;
  o!.scc_reps := [FakeOne(Generators(t))];

  SetLambdaOrb(t, o);
  return t;
end);

#

InstallMethod(ClosureSemigroup,
"for an acting semigroup and associative element collection",
[IsActingSemigroup, IsAssociativeElementCollection],
function(s, coll)
  return ClosureSemigroup(s, coll, s!.opts);
end);

#

InstallMethod(ClosureSemigroup,
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return ClosureSemigroup(s, [f], s!.opts);
end);

#

InstallMethod(ClosureSemigroup,
"for an acting semigroup, associative element, and record",
[IsActingSemigroup, IsAssociativeElement, IsRecord],
function(s, f, record)
  return ClosureSemigroup(s, [f], SemigroupOptions(record));
end);

#

InstallMethod(ClosureSemigroup,
"for an acting semigroup, associative element collection, and record",
[IsActingSemigroup, IsAssociativeElementCollection, IsRecord],
function(s, coll, record)

  if IsEmpty(coll) then
    return s;
  fi;

  if not IsGeneratorsOfActingSemigroup(coll) then
    Error("Semigroups: ClosureSemigroup: usage,\n",
    "the second argument <coll> should be a collection ",
    "satisfying\nIsGeneratorsOfActingSemigroup,");
    return;
  fi;

  if not ElementsFamily(FamilyObj(s)) = FamilyObj(Representative(coll)) then
    Error("Semigroups: ClosureSemigroup: usage,\n",
          "the semigroup and collection of elements are not of the same type,");
    return;
  fi;

  record.small := false;

  if IsActingSemigroup(coll) then
    coll := Generators(coll);
  fi;

  if IsActingSemigroupWithFixedDegreeMultiplication(s) and
    ActionDegree(s) <> ActionDegree(Representative(coll)) then
    Error("Semigroups: ClosureSemigroup: usage,\n",
          "usage: the degree of the semigroup and collection must be equal,");
    return;
  fi;

  return ClosureSemigroupNC(s, Filtered(coll, x -> not x in s),
   SemigroupOptions(record));
end);

#recreate the lambda/rho orb using the higher degree!
InstallGlobalFunction(ChangeDegreeOfTransformationSemigroupOrb,
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

# coll should consist of elements not in s

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll, opts)
  local t, old_o, o, rho_o, old_deg, oht, scc, old_scc, lookup, old_lookup,
  rho_ht, new_data, old_data, max_rank, ht, new_orb, old_orb, new_nr, old_nr,
  graph, old_graph, reps, lambdarhoht, rholookup, repslookup, orblookup1,
  orblookup2, repslens, lenreps, new_schreierpos, old_schreierpos,
  new_schreiergen, old_schreiergen, new_schreiermult, old_schreiermult, gens,
  nr_new_gens, nr_old_gens, lambda, lambdaact, lambdaperm, rho, old_to_new,
  htadd, htvalue, i, x, pos, m, rank, rhox, l, ind, pt, schutz, data_val, old,
  n, j;

  if coll = [] then
    Info(InfoSemigroups, 2, "every element in the collection belong to the ",
    " semigroup,");
    return s;
  fi;

  # init the semigroup or monoid
  if IsMonoid(s) and One(coll) = One(s) then
    # it can be that these One's differ, and hence we shouldn't call Monoid here
    t := Monoid(s, coll, opts);
  else
    t := Semigroup(s, coll, opts);
  fi;

  # if nothing is known about s, then return t
  if not HasLambdaOrb(s) or IsSemigroupIdeal(s) then
    return t;
  fi;

  # set up lambda orb for t
  old_o := LambdaOrb(s);
  o := StructuralCopy(old_o);
  rho_o := StructuralCopy(RhoOrb(s));

  if IsTransformationSemigroup(s) then
    old_deg := DegreeOfTransformationSemigroup(s);
    if old_deg < DegreeOfTransformationSemigroup(t) then
      ChangeDegreeOfTransformationSemigroupOrb(o, old_deg, t);
      ChangeDegreeOfTransformationSemigroupOrb(rho_o, old_deg, t);
    fi;
  fi;

  AddGeneratorsToOrbit(o, coll);

  # unbind everything related to strongly connected components, since
  # even if the orbit length doesn't change the strongly connected components
  # might
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

  o!.parent := t;
  o!.scc_reps := [FakeOne(GeneratorsOfSemigroup(t))];

  SetLambdaOrb(t, o);

  if not HasSemigroupData(s) or SemigroupData(s)!.pos = 0 then
    return t;
  fi;

  oht := o!.ht;
  scc := OrbSCC(o);
  old_scc := OrbSCC(old_o);
  lookup := o!.scc_lookup;

  old_lookup := old_o!.scc_lookup;

  # we don't do AddGeneratorsToOrbit of rho_o here because this is handled by
  # Enumerate(SemigroupData.. later
  rho_ht := rho_o!.ht;

  # unbind everything related to strongly connected components, since
  # even if the orbit length doesn't change the strongly connected components
  # might
  Unbind(rho_o!.scc);
  Unbind(rho_o!.trees);
  Unbind(rho_o!.scc_lookup);
  Unbind(rho_o!.mults);
  Unbind(rho_o!.schutz);
  Unbind(rho_o!.reverse);
  Unbind(rho_o!.rev);
  Unbind(rho_o!.truth);
  Unbind(rho_o!.schutzstab);

  rho_o!.parent := t;
  rho_o!.scc_reps := [FakeOne(GeneratorsOfSemigroup(t))];
  Append(rho_o!.gens, coll);
  ResetFilterObj(rho_o, IsClosed);
  SetRhoOrb(t, rho_o);

  # get new and old R-rep orbit data
  new_data := SemigroupData(t);
  old_data := SemigroupData(s);
  max_rank := MaximumList(List(coll, x -> ActionRank(t)(x)));

  ht := new_data!.ht;
  # so far found R-reps

  new_orb := new_data!.orbit;
  old_orb := old_data!.orbit;
  # the so far found R-reps data

  new_nr := Length(new_orb);
  old_nr := Length(old_orb);
  # points in orb in position at most i have descendants

  graph := new_data!.graph;
  old_graph := old_data!.graph;
  graph[1] := ShallowCopy(old_graph[1]);
  # orbit graph of orbit of R-classes under left mult

  reps := new_data!.reps;
  # reps grouped by equal lambda and rho value
  # HTValue(lambdarhoht, Concatenation(lambda(x), rho(x))

  lambdarhoht := new_data!.lambdarhoht;
  rholookup := new_data!.rholookup;

  repslookup := new_data!.repslookup;
  # Position(orb, reps[i][j])=repslookup[i][j] = HTValue(ht, reps[i][j])

  orblookup1 := new_data!.orblookup1;
  # orblookup1[i] position in reps containing orb[i][4] (the R-rep)

  orblookup2 := new_data!.orblookup2;
  # orblookup2[i] position in reps[orblookup1[i]]
  # containing orb[i][4] (the R-rep)

  repslens := new_data!.repslens;
  # Length(reps[i])=repslens[i]

  lenreps := new_data!.lenreps;
  # lenreps=Length(reps)

  # schreier
  new_schreierpos := new_data!.schreierpos;
  old_schreierpos := old_data!.schreierpos;
  new_schreiergen := new_data!.schreiergen;
  old_schreiergen := old_data!.schreiergen;
  new_schreiermult := new_data!.schreiermult;
  old_schreiermult := old_data!.schreiermult;

  # generators
  gens := new_data!.gens;
  nr_new_gens := Length(gens);
  nr_old_gens := Length(old_data!.gens);

  # lambda/rho
  lambda := LambdaFunc(t);
  lambdaact := LambdaAct(t);
  lambdaperm := LambdaPerm(t);
  rho := RhoFunc(t);

  # look up for old_to_new[i]:=Position(new_orb, old_orb[i]);
  # i.e. position of old R-rep in new_orb
  # JDM: this is mainly used to update the orbit graph of the R-rep orbit, but
  # I think this could also be done during the main loop below.

  old_to_new := EmptyPlist(old_nr);
  old_to_new[1] := 1;

  # initialise <reps>, <repslookup>, <repslens>, <lenreps>...
  for i in [2 .. Length(scc)] do
    reps[i] := [];
    repslookup[i] := [];
    repslens[i] := [];
    lenreps[i] := 0;
  od;

  if IsBoundGlobal("ORBC") then
    htadd := HTAdd_TreeHash_C;
    htvalue := HTValue_TreeHash_C;
  else
    htadd := HTAdd;
    htvalue := HTValue;
  fi;

  i := 1;

  # install old R-class reps in new_orb
  while new_nr <= old_nr and i < old_nr do
    i := i + 1;

    x := old_orb[i][4];

    pos := old_schreiermult[i]; #lambda-index for x
    m := lookup[pos];
    rank := ActionRank(t)(x);

    if rank > max_rank or scc[m][1] = old_scc[old_lookup[pos]][1] then
    # in either case x is an old R-rep and so has rectified lambda value.
    elif pos = old_scc[old_lookup[pos]][1] then
      x := x * LambdaOrbMult(o, m, pos)[2];
    else
      # x has rectified lambda value but pos refers to the unrectified value
      x := x * LambdaOrbMult(old_o, old_lookup[pos], pos)[1]
       * LambdaOrbMult(o, m, pos)[2];
    fi;

    rhox := rho(x);
    l := htvalue(rho_ht, rhox); #l<>fail since we have copied the old rho values

    if not IsBound(lambdarhoht[l]) then
    # old rho-value, but new lambda-rho-combination

      new_nr := new_nr + 1;
      lenreps[m] := lenreps[m] + 1;
      ind := lenreps[m];
      lambdarhoht[l] := [];
      lambdarhoht[l][m] := ind;

      reps[m][ind] := [x];
      repslookup[m][ind] := [new_nr];
      repslens[m][ind] := 1;

      orblookup1[new_nr] := ind;
      orblookup2[new_nr] := 1;

      pt := [t, m, o, x, false, new_nr];
    elif not IsBound(lambdarhoht[l][m]) then
    # old rho-value, but new lambda-rho-combination

      new_nr := new_nr + 1;
      lenreps[m] := lenreps[m] + 1;
      ind := lenreps[m];
      lambdarhoht[l][m] := ind;

      reps[m][ind] := [x];
      repslookup[m][ind] := [new_nr];
      repslens[m][ind] := 1;

      orblookup1[new_nr] := ind;
      orblookup2[new_nr] := 1;

      pt := [t, m, o, x, false, new_nr];
    else
    # old rho value, and maybe we already have a rep of y's R-class...
      ind := lambdarhoht[l][m];
      pt := [t, m, o, x, false, new_nr + 1];
      if not rank > max_rank then
      # this is maybe a new R-reps and so tests are required...

        #check membership in Schutzenberger group via stabiliser chain
        schutz := LambdaOrbStabChain(o, m);

        if schutz = true then
        # the Schutzenberger group is the symmetric group
          old_to_new[i] := repslookup[m][ind][1];
          continue;
        else
          if schutz = false then
           # the Schutzenberger group is trivial
            data_val := htvalue(ht, x);
            if data_val <> fail then
              old_to_new[i] := data_val;
              continue;
            fi;
          else
          # the Schutzenberger group is neither trivial nor symmetric group
           old := false;
            for n in [1 .. repslens[m][ind]] do
              if SiftedPermutation(schutz, lambdaperm(reps[m][ind][n], x)) = ()
                then
                old := true;
                old_to_new[i] := repslookup[m][ind][n];
                break;
              fi;
            od;
            if old then
              continue;
            fi;
          fi;
        fi;
      fi;
      # if rank>max_rank, then <y> is an old R-rep and hence a new one too
      new_nr := new_nr + 1;
      repslens[m][ind] := repslens[m][ind] + 1;
      reps[m][ind][repslens[m][ind]] := x;
      repslookup[m][ind][repslens[m][ind]] := new_nr;
      orblookup1[new_nr] := ind;
      orblookup2[new_nr] := repslens[m][ind];
    fi;
    rholookup[new_nr] := l;
    new_orb[new_nr] := pt;
    graph[new_nr] := ShallowCopy(old_graph[i]);
    new_schreierpos[new_nr] := old_to_new[old_schreierpos[i]];
    # orb[nr] is obtained from orb[i]
    new_schreiergen[new_nr] := old_schreiergen[i];
    # by multiplying by gens[j]
    new_schreiermult[new_nr] := pos;  # and ends up in position <pos> of
                                    # its lambda orb
    htadd(ht, x, new_nr);
    old_to_new[i] := new_nr;
  od;

  # process the orbit graph
  for i in [1 .. new_nr] do
    for j in [1 .. Length(graph[i])] do
      graph[i][j] := old_to_new[graph[i][j]];
    od;
  od;

  # apply new generators to old R-reps
  new_data!.genstoapply := [nr_old_gens + 1 .. nr_new_gens];
  new_data!.pos := 0;
  new_data!.stopper := old_to_new[old_data!.pos];
  new_data!.init := true;
  Enumerate(new_data, infinity, ReturnFalse);

  new_data!.pos := old_to_new[old_data!.pos];
  new_data!.stopper := false;
  new_data!.genstoapply := [1 .. nr_new_gens];

  return t;
end);

#subsemigroups

# <limit> is the max size of the subsemigroup.

InstallMethod(SubsemigroupByProperty,
"for an acting semigroup, function, and positive integer",
[IsActingSemigroup, IsFunction, IsPosInt],
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
"for acting semigroup with inverse op, function, positive integer",
[IsActingSemigroupWithInverseOp, IsFunction, IsPosInt],
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

InstallMethod(SubsemigroupByProperty,
"for an acting semigroup and function",
[IsActingSemigroup, IsFunction],
function(S, func)
  return SubsemigroupByProperty(S, func, Size(S));
end);

#

InstallMethod(InverseSubsemigroupByProperty,
"for acting semigroup with inverse op and function",
[IsActingSemigroupWithInverseOp, IsFunction],
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

InstallMethod(RandomMatrixSemigroup,
"for a ring, positive integer and positive integer",
[IsRing, IsPosInt, IsPosInt],
function(R, m, n)
  return Semigroup(List([1 .. m], x -> RandomMat(n, n, R)));
end);

#

InstallMethod(RandomBinaryRelationMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  local s;

  s := Monoid(List([1 .. m], x -> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true);
  return s;
end);

#

InstallMethod(RandomBinaryRelationSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  local s;

  s := Semigroup(List([1 .. m], x -> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true);
  return s;
end);

#

InstallMethod(RandomBlockGroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return Semigroup(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomPartialPermMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return Monoid(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return InverseMonoid(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return InverseSemigroup(Set(List([1 .. m], x -> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomTransformationSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return Semigroup(Set(List([1 .. m], x -> RandomTransformation(n))));
end);

#

InstallMethod(RandomTransformationMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return Monoid(Set(List([1 .. m], x -> RandomTransformation(n))));
end);

#

InstallMethod(RandomBipartitionSemigroup,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return Semigroup(Set(List([1 .. m], x -> RandomBipartition(n))));
end);

#

InstallMethod(RandomBipartitionMonoid,
"for positive integer and positive integer",
[IsPosInt, IsPosInt],
function(m,n)
  return Monoid(Set(List([1 .. m], x -> RandomBipartition(n))));
end);

#EOF
