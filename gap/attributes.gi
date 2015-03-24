#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for finding various attributes of finite
# semigroups.

# Note about the difference between One and MultiplicativeNeutralElement
# (the same goes for Zero and MultplicativeZero):
#
# One(S) returns One(Representative(S)) if it belongs to S, so that
# One(S)=Transformation([1..DegreeOfTransformationSemigroup(S)]) if S is a
# transformation semigroup and it returns fail otherwise, or it returns
# PartialPerm([1..DegreeOfPartialPermSemigroup]) if this belongs to S.
#
# MultiplicativeNeutralElement on the other hand returns the element of S that
# acts as the identity, note that this can be equal to One(S) but it can also
# not be equal to One(S).
#
# A semigroup satisfies IsMonoidAsSemigroup(S) if
# MultiplicativeNeutralElement(S) <> fail, so it could be that One(S) returns
# fail but IsMonoidAsSemigroup is still true.

# same method for ideals

InstallMethod(IsMultiplicativeZero,
"for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(S, x)
  return MultiplicativeZero(S) <> fail and x = MultiplicativeZero(S);
end);

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

InstallMethod(IsGreensDLeq, "for an acting semigroup",
[IsActingSemigroup],
function(S) 
  local partial, data, comp_index;

  partial := PartialOrderOfDClasses(S);
  data := SemigroupData(S);

  comp_index := function(x, y)
    if y in partial[x] then
      return true;
    elif Length(partial[x]) = 1 and partial[partial[x][1]] = partial[x] then
      return false;
    fi;
    return ForAny(partial[x], z -> z <> x and comp_index(z, y));
  end;

  return function(x, y)
    return comp_index(OrbSCCLookup(data)[Position(data, x)] - 1,
      OrbSCCLookup(data)[Position(data, y)] - 1);
  end;
end);

# different method for ideals/regular/inverse, although this method also works

InstallMethod(MaximalDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local gens, partial, data, pos, i, out, classes, x;

  gens := GeneratorsOfSemigroup(s);
  partial := PartialOrderOfDClasses(s);
  data := SemigroupData(s);
  pos := [];
  for x in gens do
    i := OrbSCCLookup(data)[Position(data, x)] - 1;
    #index of the D-class containing x
    AddSet(pos, i);
  od;

  out := [];
  classes := GreensDClasses(s);
  for i in pos do
    if not ForAny([1 .. Length(partial)], j -> j <> i and i in partial[j]) then
      Add(out, classes[i]);
    fi;
  od;

  return out;
end);

# same method for inverse, different method for inverse ideals

InstallMethod(MaximalDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  local gens, partial, pos, o, scc, out, classes, x, i;

  gens := GeneratorsOfSemigroup(S);
  partial := PartialOrderOfDClasses(S);
  pos := [];
  o := LambdaOrb(S);
  scc := OrbSCCLookup(o);

  for x in gens do
    #index of the D-class containing x
    AddSet(pos, scc[Position(o, LambdaFunc(S)(x))] - 1);
  od;

  out := [];
  classes := GreensDClasses(S);
  for i in pos do
    if not ForAny([1 .. Length(partial)], j -> j <> i and i in partial[j]) then
      Add(out, classes[i]);
    fi;
  od;

  return out;
>>>>>>> other
end);

# same method for ideals

<<<<<<< local
InstallMethod(IsomorphismFpSemigroup, "for a finite semigroup",
[IsFinite and IsSemigroup],
=======
InstallMethod(StructureDescriptionSchutzenbergerGroups,
"for an acting semigroup", [IsActingSemigroup],
>>>>>>> other
function(S)
<<<<<<< local
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
=======
  local o, scc, out, m;

  o := LambdaOrb(S);
  Enumerate(o, infinity);
  scc := OrbSCC(o);
  out := [];

  for m in [2 .. Length(scc)] do
    AddSet(out, StructureDescription(LambdaOrbSchutzGp(o, m)));
  od;

  return out;
>>>>>>> other
end);

# same method for ideals

<<<<<<< local
InstallMethod(IsomorphismFpSemigroup, "for a finite monoid",
[IsMonoid and IsFinite],
function(S)
  local rules, lookup, convert, F, A, rels, one, Q, B, i;

  if GeneratorsOfSemigroup(S)=GeneratorsOfMonoid(S) then
    return IsomorphismFpMonoid(S);
  fi;
=======
InstallMethod(StructureDescriptionMaximalSubgroups,
"for an acting semigroup", [IsActingSemigroup],
function(s)
  local out, d;
>>>>>>> other

<<<<<<< local
  lookup:=List(GeneratorsOfMonoid(S), x-> Position(GeneratorsOfSemigroup(S), x));
  one:=Position(GeneratorsOfSemigroup(S), One(S));
  # if One(S) appears more than once in the generators of S, then this causes
  # problems here... JDM
  convert:=function(w)
    if not IsEmpty(w) then
      return List(w, i-> lookup[i]);
    else
      return [one];
=======
  out := [];
  for d in DClasses(s) do
    if IsRegularClass(d) then
      AddSet(out, StructureDescription(GroupHClass(d)));
>>>>>>> other
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

<<<<<<< local
InstallMethod(RightCayleyGraphSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Enumerate(ExhaustiveData(S))!.right;
=======
InstallMethod(GroupOfUnits, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s) = fail then
    return fail;
  fi;

  r := GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g := SchutzenbergerGroup(r);
  deg := DegreeOfTransformationSemigroup(s);

  u := Monoid(List(GeneratorsOfGroup(g), x -> AsTransformation(x, deg)));

  SetIsomorphismPermGroup(u, MappingByFunction(u, g, PermutationOfImage,
   x -> AsTransformation(x, deg)));

  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
>>>>>>> other
end);

# same method for ideals

<<<<<<< local
InstallMethod(LeftCayleyGraphSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
=======
InstallMethod(GroupOfUnits, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s) = fail then
    return fail;
  fi;

  r := GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g := SchutzenbergerGroup(r);
  deg := Maximum(DegreeOfPartialPermSemigroup(s),
   CodegreeOfPartialPermSemigroup(s));

  u := Monoid(List(GeneratorsOfGroup(g), x -> AsPartialPerm(x, deg)));

  SetIsomorphismPermGroup(u, MappingByFunction(u, g, AsPermutation,
   x -> AsPartialPerm(x, deg)));

  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a bipartition semigroup",
[IsBipartitionSemigroup],
>>>>>>> other
function(S)
<<<<<<< local
  return Enumerate(ExhaustiveData(S))!.left;
=======
  local R, G, deg, U;

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  fi;

  R := GreensRClassOfElementNC(S, MultiplicativeNeutralElement(S));
  G := SchutzenbergerGroup(R);
  deg := DegreeOfBipartitionSemigroup(S);

  U := Monoid(List(GeneratorsOfGroup(G), x -> AsBipartition(x, deg)));

  SetIsomorphismPermGroup(U, MappingByFunction(U, G, AsPermutation,
   x -> AsBipartition(x, deg)));

  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, G);

  return U;
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(s)
  local r, g, i, j, u;

  if MultiplicativeNeutralElement(s) = fail then
    return fail;
  fi;

  r := GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g := SchutzenbergerGroup(r);
  i := MultiplicativeNeutralElement(s)![1];
  j := MultiplicativeNeutralElement(s)![3];

  u := Semigroup(List(GeneratorsOfGroup(g), x -> RMSElement(s, i, x, j)));

  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
>>>>>>> other
end);

# same method for ideals

<<<<<<< local
InstallMethod(IdempotentGeneratedSubsemigroup, "for finite semigroup",
[IsActingSemigroup], S-> Semigroup(Idempotents(S), rec(small:=true)));
=======
InstallMethod(IdempotentGeneratedSubsemigroup, "for an acting semigroup",
[IsActingSemigroup], s -> Semigroup(Idempotents(s), rec(small := true)));
>>>>>>> other

# same method for ideals

<<<<<<< local
InstallMethod(IdempotentGeneratedSubsemigroup,
"for a finite inverse op semigroup",
[IsSemigroupWithInverseOp and IsFinite],
S-> InverseSemigroup(Idempotents(S), rec(small:=true)));
=======
InstallMethod(IdempotentGeneratedSubsemigroup,
"for an inverse op acting semigroup", [IsActingSemigroupWithInverseOp],
s -> InverseSemigroup(Idempotents(s), rec(small := true)));
>>>>>>> other

# same method for ideals

<<<<<<< local
InstallMethod(IsomorphismReesMatrixSemigroup, "for a D-class", [IsGreensDClass],
InjectionPrincipalFactor);
=======
InstallMethod(InjectionPrincipalFactor, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, inv_l, inv_r, lambdaperm, leftact, rightact,
  f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("Semigroups: InjectionPrincipalFactor: usage,\n",
    "the argument <d> must be a regular D-class,");
    return;
  fi;

  g := GroupHClass(d);
  rep := Representative(g);
  g := Range(IsomorphismPermGroup(g));

  rreps := HClassReps(LClass(d, rep));
  lreps := HClassReps(RClass(d, rep));
  mat := [];

  inv_l := EmptyPlist(Length(lreps));
  inv_r := EmptyPlist(Length(rreps));

  lambdaperm := LambdaPerm(Parent(d));
  if IsTransformationSemigroupGreensClass(d)
    or IsPartialPermSemigroupGreensClass(d)
    or IsBipartitionSemigroupGreensClass(d) then
    leftact := PROD;
  elif IsReesZeroMatrixSubsemigroup(Parent(d)) then
    leftact := function(x, y)
      if y![1] = 0 then
        return y;
      fi;
      return Objectify(TypeObj(y), [y![1], y![4][rep![3]][rep![1]] ^ -1
      * x * rep![2] ^ -1 * y![2], y![3], y![4]]);
    end;
  fi;

  rightact := StabilizerAction(Parent(d));

  for i in [1 .. Length(lreps)] do
    mat[i] := [];
    for j in [1 .. Length(rreps)] do
      f := lreps[i] * rreps[j];
      if f in d then
        mat[i][j] := lambdaperm(rep, f);
        if not IsBound(inv_r[j]) then
          # could use lreps[i]*rreps[j]^-1*lreps[i] instead if there was a
          # method for ^-1...
          inv_r[j] := leftact(mat[i][j] ^ -1, lreps[i]);
        fi;
        if not IsBound(inv_l[i]) then
          inv_l[i] := rightact(rreps[j], mat[i][j] ^ -1);
        fi;
      else
        mat[i][j] := 0;
      fi;
    od;
  od;

  if NrIdempotents(d) = NrHClasses(d) then
    rms := ReesMatrixSemigroup(g, mat);
  else
    rms := ReesZeroMatrixSemigroup(g, mat);
  fi;

  iso := function(f)
    local o, m, i, j;
    o := LambdaOrb(d);
    m := LambdaOrbSCCIndex(d);
    i := Position(o, LambdaFunc(Parent(d))(f));

    if i = fail or OrbSCCLookup(o)[i] <> m then
      return fail;
    fi;
    i := Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then
      o := RhoOrb(d);
      m := RhoOrbSCCIndex(d);
    fi;
    j := Position(o, RhoFunc(Parent(d))(f));
    if j = fail or OrbSCCLookup(o)[j] <> m then
      return fail;
    fi;
    j := Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);
    return Objectify(TypeReesMatrixSemigroupElements(rms),
     [j, lambdaperm(rep, rep * inv_r[j] * f * inv_l[i]), i, mat]);
  end;

  inv := function(x)
    if x![1] = 0 then
      return fail;
    fi;
    return rightact(rreps[x![1]], x![2]) * lreps[x![3]];
  end;

  hom := MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);

# same method for ideals

InstallMethod(IsomorphismReesMatrixSemigroup,
"for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], InjectionPrincipalFactor);
>>>>>>> other

# same method for ideal

InstallMethod(IrredundantGeneratingSubset,
"for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
  local gens, nrgens, deg, out, redund, i, f;
<<<<<<< local

   if (IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll)) or
=======

  if not (IsActingSemigroup(coll) or IsGeneratorsOfActingSemigroup(coll)) then
    Error("Semigroups: IrredundantGeneratingSubset: usage,\n",
          "<coll> must be a generators of an acting semigroup,");
  fi;

  if (IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll)) or
>>>>>>> other
   (HasIsSemigroupIdeal(coll) and IsSemigroupIdeal(coll)) then
    coll := ShallowCopy(GeneratorsOfSemigroup(coll));
  fi;

  if Size(coll) = 1 then
    return coll;
  fi;

<<<<<<< local
  gens:=Set(ShallowCopy(coll));
  nrgens:=Length(gens);
=======
  gens := Set(ShallowCopy(coll));
  nrgens := Length(gens);
  deg := ActionDegree(coll);
  coll := Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y)
               return ActionRank(x, deg) > ActionRank(y, deg);
             end);
>>>>>>> other

<<<<<<< local
  if IsActingSemigroup(coll) or IsGeneratorsOfActingSemigroup(coll) then
    deg:=ActionDegree(coll);
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
    Sort(coll, function(x, y) return ActionRank(x, deg)>ActionRank(y, deg); end);
  fi;

  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;
=======
  out := EmptyPlist(Length(coll));
  redund := EmptyPlist(Length(coll));
  i := 0;
>>>>>>> other

  repeat
    i := i + 1;
    f := coll[i];
    if InfoLevel(InfoSemigroups) >= 3 then
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund),
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then
      if f in Semigroup(Difference(gens, [f])) then
        AddSet(redund, f);
        gens := Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund) + Length(out) = nrgens;

  if InfoLevel(InfoSemigroups) > 1 then
    Print("\n");
  fi;
  return out;
end);

#

<<<<<<< local
InstallMethod(IsomorphismReesMatrixSemigroup,
"for a finite simple or 0-simplesemigroup", [IsFinite and IsSemigroup],
=======
InstallMethod(IsomorphismReesMatrixSemigroup,
"for a simple or 0-simple acting semigroup", [IsActingSemigroup],
>>>>>>> other
function(S)
  local D, iso, inv;
  if not (IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S)) then
    TryNextMethod();
  fi;
  D := GreensDClasses(S)[1];
  if IsZeroSimpleSemigroup(S)
   and IsMultiplicativeZero(S, Representative(D)) then
    D := GreensDClasses(S)[2];
  fi;
  iso := IsomorphismReesMatrixSemigroup(D);
  inv := InverseGeneralMapping(iso);
  return MagmaIsomorphismByFunctionsNC(S, Range(iso),
   x -> x ^ iso, x -> x ^ inv);
end);

# same method for ideals

<<<<<<< local
InstallMethod(InversesOfSemigroupElement,
"for a finite semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement],
=======
InstallMethod(InversesOfSemigroupElement,
"for an acting semigroup and associative element",
[IsActingSemigroup, IsAssociativeElement],
>>>>>>> other
function(S, x)

  if x in S then
    return InversesOfSemigroupElementNC(S, x);
  fi;

  return fail;
end);

<<<<<<< local
=======
# different method for ideals

InstallMethod(InversesOfSemigroupElementNC,
"for an acting semigroup and associative element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts, grades,
   rho_f, lambdarank, creator, inv, out, k, g, i, name, rho;

  regular := IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  lambda := LambdaFunc(s)(f);
  rank := LambdaRank(s)(LambdaFunc(s)(f));
  rhorank := RhoRank(s);
  tester := IdempotentTester(s);
  j := 0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  # JDM is this really true?
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    o := RhoOrb(s);
    rhos := EmptyPlist(Length(o));
    for i in [2 .. Length(o)] do
      if rhorank(o[i]) = rank and tester(lambda, o[i]) then
        j := j + 1;
        rhos[j] := o[i];
      fi;
    od;
  else

    opts := rec(treehashsize := s!.opts.hashlen.M,
                gradingfunc := function(o, x) return rhorank(x); end,
                onlygrades := function(x, y) return x >= rank; end,
                onlygradesdata := fail);

    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name) := LambdaOrbOpts(s).(name);
    od;

    o := Orb(s, RhoOrbSeed(s), RhoAct(s), opts);
    Enumerate(o, infinity);

    grades := Grades(o);
    rhos := EmptyPlist(Length(o));
    for i in [2 .. Length(o)] do
      if grades[i] = rank and tester(lambda, o[i]) then
        j := j + 1;
        rhos[j] := o[i];
      fi;
    od;
  fi;
  ShrinkAllocationPlist(rhos);

  rho_f := RhoFunc(s)(f);
  lambdarank := LambdaRank(s);
  creator := IdempotentCreator(s);
  inv := LambdaInverse(s);

  out := [];
  k := 0;

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then
    o := LambdaOrb(s);
    for i in [2 .. Length(o)] do
      if lambdarank(o[i]) = rank and tester(o[i], rho_f) then
        for rho in rhos do
          g := creator(lambda, rho) * inv(o[i], f);
          if regular or g in s then
            k := k + 1;
            out[k] := g;
          fi;
        od;
      fi;
    od;
  else
     opts := rec(treehashsize := s!.opts.hashlen.M,
                 gradingfunc := function(o, x) return lambdarank(x); end,
                 onlygrades := function(x, y) return x >= rank; end,
                 onlygradesdata := fail);

    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name) := LambdaOrbOpts(s).(name);
    od;

    o := Orb(s, LambdaOrbSeed(s), LambdaAct(s), opts);
    Enumerate(o);
    grades := Grades(o);

    for i in [2 .. Length(o)] do
      if grades[i] = rank and tester(o[i], rho_f) then
        for rho in rhos do
          g := creator(lambda, rho) * inv(o[i], f);
          if regular or g in s then
            k := k + 1;
            out[k] := g;
          fi;
        od;
      fi;
    od;
  fi;

  return out;
end);

>>>>>>> other
# same method for ideals

<<<<<<< local
InstallMethod(MinimalIdeal, "for a finite semigroup", [IsFinite and IsSemigroup],
=======
InstallMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local gens, rank, lambda, max, r, rep, f;

  gens := Generators(s);
  rank := LambdaRank(s);
  lambda := LambdaFunc(s);
  max := 0;
  rep := gens[1];

  for f in gens do
    r := rank(lambda(f));
    if r > max then
      max := r;
      rep := f;
    fi;
  od;

  if max = ActionDegree(s) and IsMultiplicativeElementWithOneCollection(s) then
    return One(s);
  fi;

  r := GreensRClassOfElementNC(s, rep);

  if not NrIdempotents(r) = 1 then
    Info(InfoSemigroups, 2, "the number of idempotents in the R-class of the",
    " first maximum rank");
    Info(InfoSemigroups, 2, " generator is not 1");
    return fail;
  fi;

  f := Idempotents(r)[1];

  if ForAll(gens, x -> x * f = x and f * x = x) then
    return f;
  fi;

  Info(InfoSemigroups, 2, "the unique idempotent in the R-class of the first",
  " maximum rank");
  Info(InfoSemigroups, 2, " generator is not the identity");
  return fail;
end);

# it just so happens that the MultiplicativeNeutralElement of a semigroup of
# partial permutations has to coincide with the One. This is not the case for
# transformation semigroups

# same method for ideals

InstallMethod(MultiplicativeNeutralElement, "for a partial perm semigroup",
[IsPartialPermSemigroup], One);

# same method for ideals...

InstallMethod(MultiplicativeZero, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local min, o, rank, i, pos, f, min_found, rank_i;

  if IsSemigroupIdeal(s)
    and HasMultiplicativeZero(SupersemigroupOfIdeal(s)) then
    return MultiplicativeZero(SupersemigroupOfIdeal(s));
  fi;

  min := MinActionRank(s);
  o := LambdaOrb(s);
  rank := LambdaRank(s);

  #is there an element in s with minimum possible rank
  if IsTransformationSemigroup(s) then
    i := 0;
    repeat
      i := i + 1;
      pos := EnumeratePosition(o, [i], false);
    until pos <> fail or i = ActionDegree(s);
  elif IsPartialPermSemigroup(s) then
    pos := EnumeratePosition(o, [], false);
  else
    pos := LookForInOrb(o, function(o, x)
                             return rank(x) = min;
                           end, 2);
  fi;

  if pos <> fail and pos <> false then
    f := EvaluateWord(o, TraceSchreierTreeForward(o, pos));
  else
    # lambda orb is closed, find an element with minimum rank
    min_found := rank(o[2]);
    pos := 2;
    i := 1;

    while min_found > min and i < Length(o) do
      i := i + 1;
      rank_i := rank(o[i]);
      if rank_i < min_found then
        min_found := rank_i;
        pos := i;
      fi;
    od;
    f := EvaluateWord(o, TraceSchreierTreeForward(o, pos));
  fi;
  if IsIdempotent(f) and Size(GreensRClassOfElementNC(s, f)) = 1 then
    return f;
  fi;

  return fail;
end);

# same method for ideals

InstallMethod(MinimalIdeal, "for an acting semigroup", [IsActingSemigroup],
>>>>>>> other
function(S)
  local I;
  I := SemigroupIdeal(S, Representative(MinimalDClass(S)));
  SetIsSimpleSemigroup(I, true);
  return I;
end);

<<<<<<< local
=======
# same method for inverse/ideals

InstallMethod(MinimalDClass, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local rank, o, pos, min, len, m, x, i;

  if IsSemigroupIdeal(S) and HasMinimalDClass(SupersemigroupOfIdeal(S)) then
    return GreensDClassOfElementNC(S,
     Representative(MinimalDClass(SupersemigroupOfIdeal(S))));
  fi;

  rank := LambdaRank(S);
  o := LambdaOrb(S);

  pos := LookForInOrb(o, function(o, x)
                           return rank(x) = MinActionRank(S);
                         end, 2);

  if pos = false then
    min := rank(o[2]);
    pos := 2;
    len := Length(o);
    for i in [3 .. len] do
      m := rank(o[i]);
      if m < min then
        pos := i;
        min := m;
      fi;
    od;
  fi;

  x := EvaluateWord(o, TraceSchreierTreeForward(o, pos));
  return GreensDClassOfElementNC(S, x);
end);

>>>>>>> other
#

<<<<<<< local
InstallMethod(PrincipalFactor, "for a Green's D-class",
[IsGreensDClass], D-> Range(InjectionPrincipalFactor(D)));
=======
InstallMethod(PrincipalFactor, "for a D-class",
[IsGreensDClass and IsActingSemigroupGreensClass],
d -> Range(InjectionPrincipalFactor(d)));
>>>>>>> other

#

InstallMethod(PrincipalFactor, "for a D-class",
[IsGreensDClass], AssociatedReesMatrixSemigroupOfDClass);

# different method for ideals, not yet implemented

InstallMethod(SmallSemigroupGeneratingSet,
"for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
<<<<<<< local
  if Length(coll)<2 then
=======
  if not IsGeneratorsOfActingSemigroup(coll) then
    TryNextMethod();
  elif Length(coll) < 2 then
>>>>>>> other
    return coll;
  else
    return GeneratorsOfSemigroup(Semigroup(coll, rec(small := true)));
  fi;
end);

# different method for ideals, not yet implemented

<<<<<<< local
InstallMethod(SmallSemigroupGeneratingSet,
"for a finite semigroup", [IsSemigroup and IsFinite],
=======
InstallMethod(SmallSemigroupGeneratingSet,
"for an acting semigroup", [IsActingSemigroup],
>>>>>>> other
function(S)
  return SmallSemigroupGeneratingSet(GeneratorsOfSemigroup(S));
end);

#

InstallMethod(SmallMonoidGeneratingSet,
"for an associative element with one collection",
[IsAssociativeElementCollection and IsMultiplicativeElementWithOneCollection],
function(coll)
<<<<<<< local
  if Length(coll)<2 then
=======
  if not IsGeneratorsOfActingSemigroup(coll) then
    TryNextMethod();
  elif Length(coll) < 2 then
>>>>>>> other
    return coll;
  else
    return GeneratorsOfMonoid(Monoid(coll, rec(small := true)));
  fi;
end);

# same method for ideals

<<<<<<< local
InstallMethod(SmallMonoidGeneratingSet, "for a finite monoid",
[IsFinite and IsMonoid],
=======
InstallMethod(SmallMonoidGeneratingSet,
"for an acting monoid", [IsActingSemigroup and IsMonoid],
>>>>>>> other
function(S)
<<<<<<< local
  if IsEmpty(GeneratorsOfMonoid(S)) then
    return [];
=======
  if IsEmpty(GeneratorsOfMonoid(S)) then
    return [];
>>>>>>> other
  fi;
  return SmallMonoidGeneratingSet(GeneratorsOfMonoid(S));
end);

#

InstallMethod(SmallInverseSemigroupGeneratingSet,
"for generators of an inverse semigroup",
[IsGeneratorsOfInverseSemigroup],
function(coll)
<<<<<<< local
  if Length(coll)<2 then
=======
  if not IsGeneratorsOfActingSemigroup(coll) then
    TryNextMethod();
  elif Length(coll) < 2 then
>>>>>>> other
    return coll;
  else
    return GeneratorsOfInverseSemigroup(
      InverseSemigroup(coll, rec(small := true)));
  fi;
end);

#

<<<<<<< local
InstallMethod(SmallInverseSemigroupGeneratingSet,
"for a semigroup with inverse op", [IsSemigroupWithInverseOp],
=======
InstallMethod(SmallInverseSemigroupGeneratingSet,
"for an acting inverse semigroup with generators",
[IsActingSemigroup and IsInverseSemigroup],
>>>>>>> other
function(S)
  return SmallSemigroupGeneratingSet(GeneratorsOfInverseSemigroup(S));
end);

#

InstallMethod(SmallInverseMonoidGeneratingSet,
"for generators of an inverse monoid",
[IsGeneratorsOfInverseSemigroup and IsMultiplicativeElementWithOneCollection],
function(coll)
<<<<<<< local
  if Length(coll)<2 then
=======
  if not IsGeneratorsOfActingSemigroup(coll) then
    TryNextMethod();
  elif Length(coll) < 2 then
>>>>>>> other
    return coll;
  else
    return GeneratorsOfInverseMonoid(InverseMonoid(coll, rec(small := true)));
  fi;
end);

#

<<<<<<< local
InstallMethod(SmallInverseMonoidGeneratingSet,
"for a monoid with inverse op",
[IsSemigroupWithInverseOp and IsMonoid],
=======
InstallMethod(SmallInverseMonoidGeneratingSet,
"for an acting inverse semigroup with generators",
[IsActingSemigroup and IsInverseMonoid],
>>>>>>> other
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

<<<<<<< local
InstallMethod(StructureDescription, "for a Brandt semigroup",
[IsBrandtSemigroup],
function(S)
  local x, D;

  x:=First(Generators(S), x-> x<>MultiplicativeZero(S));

  if x=fail then
    return "0";
  fi;
=======
InstallMethod(StructureDescription, "for an acting Brandt semigroup",
[IsActingSemigroup and IsBrandtSemigroup],
function(s)
  local x, d;
>>>>>>> other

<<<<<<< local
  D:=GreensDClassOfElementNC(S, x);

  return Concatenation("B(", StructureDescription(GroupHClass(D)), ", ",
  String(NrRClasses(D)), ")");
=======
  x := First(Generators(s), x -> x <> MultiplicativeZero(s));
  d := GreensDClassOfElementNC(s, x);

  return Concatenation("B(", StructureDescription(GroupHClass(d)), ", ",
  String(NrRClasses(d)), ")");
>>>>>>> other
end);

# same method for ideals

<<<<<<< local
InstallMethod(StructureDescription, "for a group as semigroup",
[IsGroupAsSemigroup],
function(S)
  if IsGroup(S) then # since groups (even perm groups) satisfy IsGroupAsSemigroup
    TryNextMethod();
=======
InstallMethod(StructureDescription,
"for an acting group as semigroup",
[IsActingSemigroup and IsGroupAsSemigroup],
s -> StructureDescription(Range(IsomorphismPermGroup(s))));

#

InstallMethod(IsomorphismTransformationMonoid,
"for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local id, dom, gens, inv;

  if IsMonoid(s) then
    return MappingByFunction(s, s, IdFunc, IdFunc);
  fi;

  if MultiplicativeNeutralElement(s) = fail then
    Error("Semigroups: IsomorphismTransformationMonoid: usage,\n",
          "the argument <s> must have a multiplicative neutral element,");
    return;
>>>>>>> other
  fi;
<<<<<<< local

  return StructureDescription(Range(IsomorphismPermGroup(S)));
=======

  id := MultiplicativeNeutralElement(s);
  dom := ImageSetOfTransformation(id);

  gens := List(Generators(s), x -> TransformationOp(x, dom));

  inv := function(f)
    local out, i;

    out := [1 .. DegreeOfTransformationSemigroup(s)];
    for i in [1 .. Length(dom)] do
      out[dom[i]] := dom[i ^ f];
    od;
    return id * Transformation(out);
  end;

  return MappingByFunction(s, Monoid(gens), f -> TransformationOp(f, dom),
   inv);
end);

# different method for ideals

InstallMethod(IsomorphismTransformationSemigroup,
"for a matrix semigroup with generators",
[IsMatrixSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, F, T;
  n := Length(GeneratorsOfSemigroup(S)[1][1]);
  F := FieldOfMatrixList(GeneratorsOfSemigroup(S));
  T := Semigroup(List(GeneratorsOfSemigroup(S), x ->
   TransformationOp(x, Elements(F ^ n), OnRight)));
  return MappingByFunction(S, T,
   x -> TransformationOp(x, Elements(F ^ Size(F)), OnRight));
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)

  if not IsGroupAsSemigroup(s)  then
   Error("Semigroups: IsomorphismPermGroup: usage,\n",
         "the argument <s> must be a transformation semigroup ",
         "satisfying IsGroupAsSemigroup,");
   return;
  fi;

  return MagmaIsomorphismByFunctionsNC(s,
   Group(List(GeneratorsOfSemigroup(s), PermutationOfImage)),
    PermutationOfImage,
    x -> AsTransformation(x, DegreeOfTransformationSemigroup(s)));
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(s)

  if not IsGroupAsSemigroup(s)  then
   Error("Semigroups: IsomorphismPermGroup: usage,\n",
         "the argument <s> must be a partial perm semigroup ",
         "satisfying IsGroupAsSemigroup,");
   return;
  fi;

  return MagmaIsomorphismByFunctionsNC(s,
   Group(List(GeneratorsOfSemigroup(s), AsPermutation)),
    AsPermutation, x -> AsPartialPerm(x, DomainOfPartialPermCollection(s)));
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup,
"for a subsemigroup of a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup],
function(S)
  local rep;

  if not IsGroupAsSemigroup(S)  then
   Error("Semigroups: IsomorphismPermGroup: usage,\n",
         "the argument <S> must be a subsemigroup of a Rees 0-matrix ",
         "semigroup satisfying IsGroupAsSemigroup,");
   return;
  fi;

  rep := S.1;
  if rep![1] = 0 then # special case for the group consisting of 0
    return MagmaIsomorphismByFunctionsNC(S, Group(()), x -> (), x -> rep);
  fi;

  return MagmaIsomorphismByFunctionsNC(S,
    Group(List(GeneratorsOfSemigroup(S), x -> x![2])),
      x -> x![2], x -> RMSElement(S, rep![1], x, rep![3]));
end);

# fall back method, same method for ideals

InstallMethod(IsomorphismPermGroup, "for a semigroup", [IsSemigroup],
function(S)
  local en, act, gens;

  if not IsGroupAsSemigroup(S)  then
    Error("Semigroups: IsomorphismPermGroup: usage,\n",
          "the argument must be a semigroup satisfying ",
          "IsGroupAsSemigroup,");
    return;
  fi;

  en := EnumeratorSorted(S);

  act := function(i, x)
    return Position(en, en[i] * x);
  end;

  gens := List(GeneratorsOfSemigroup(S),
   x -> Permutation(x, [1 .. Length(en)], act));

  return MagmaIsomorphismByFunctionsNC(S, Group(gens),
   x -> Permutation(x, [1 .. Length(en)], act),
   x -> en[Position(en, MultiplicativeNeutralElement(S)) ^ x]);
end);

# not relevant for ideals

InstallMethod(IsomorphismTransformationSemigroup,
"for semigroup of binary relations with generators",
[IsSemigroup and IsGeneralMappingCollection and HasGeneratorsOfSemigroup],
function(s)
  local n, pts, o, t, pos, i;
  if not IsBinaryRelationOnPointsRep(Representative(s)) then
    TryNextMethod();
  fi;
  n := DegreeOfBinaryRelation(GeneratorsOfSemigroup(s)[1]);
  pts := EmptyPlist(2 ^ n);

  for i in [1 .. n] do
    o := Orb(s, [i], OnPoints); #JDM multiseed orb
    Enumerate(o);
    pts := Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  pos := List([1 .. n], x -> Position(pts, [x]));
  t := Semigroup(List(GeneratorsOfSemigroup(s),
   x -> TransformationOpNC(x, pts, OnPoints)));

  return MappingByFunction(s, t, x -> TransformationOpNC(x, pts, OnPoints),
  x -> BinaryRelationOnPoints(List([1 .. n], i -> pts[pos[i] ^ x])));
end);

# not relevant for ideals

InstallMethod(Size, "for a monogenic transformation semigroup",
[IsTransformationSemigroup and IsMonogenicSemigroup],
function(s)
  local ind;

  ind := IndexPeriodOfTransformation(GeneratorsOfSemigroup(s)[1]);
  if ind[1] > 0 then
    return Sum(ind) - 1;
  fi;
  return Sum(ind);
>>>>>>> other
end);

#EOF
