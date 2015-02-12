#############################################################################
##
#W  attributes-inverse.gi
#Y  Copyright (C) 2013-15                               James D. Mitchell,
##                                                      Wilf Wilson,
##                                                      Rhiannon Dougall,
##                                                      Robert Hancock
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# same method for ideals

InstallMethod(CharacterTableOfInverseSemigroup,
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  local reps, p, H, C, r, tbl, id, l, A, o, lookup, scc, conjclass, conjlens,
  j, conjreps, dom, subsets, x, m, u, k, h, i, n, y;

  reps := ShallowCopy(DClassReps(S));
  p := Sortex(reps, function(x,y)
    return RankOfPartialPerm(x) > RankOfPartialPerm(y);
  end);

  H := List(reps, e -> SchutzenbergerGroup(HClass(S, e)));
  C := []; # the group character matrices
  r := 0;

  #the block diagonal matrix of group character matrices
  for h in H do
    tbl := CharacterTable(h);
    id := IdentificationOfConjugacyClasses(tbl);
    tbl := Irr(tbl);
    l := Length(id);
    for i in [1 + r .. l + r] do
      C[i] := [];
      for j in [1 .. r] do
        C[i][j] := 0;
        C[j][i] := 0;
      od;

      for j in [1 + r .. l + r] do
        C[i][j] := tbl[i - r][Position(id, j - r)];
      od;
    od;
    r := r + l;
  od;

  A := List([1 .. r], x -> [1 .. r] * 0);
  o := LambdaOrb(S);
  lookup := OrbSCCLookup(o);
  scc := OrbSCC(o);

  conjclass := [ConjugacyClasses(H[1])];
  conjlens := [0];

  for i in [2 .. Length(H)] do
    Add(conjclass, ConjugacyClasses(H[i]));
    conjlens[i] := conjlens[i - 1] + Length(conjclass[i - 1]);
  od;

  j := 0;
  conjreps := [];
  for i in [1 .. Length(H)] do
    dom := DomainOfPartialPerm(reps[i]);
    subsets := Filtered(o, x -> IsSubset(dom, x));
    for n in [1 .. Length(conjclass[i])] do
      j := j + 1;
      conjreps[n + conjlens[i]] :=
        AsPartialPerm(Representative(conjclass[i][n]), dom);
      for y in subsets do
        x := RestrictedPartialPerm(conjreps[n + conjlens[i]], y);
        if y = ImageSetOfPartialPerm(x) then
          l := Position(o, y);
          m := lookup[l];
          u := LambdaOrbMult(o, m, l);
          x := AsPermutation(u[1] * x * u[2]);
          m := (m - 1) ^ p;
          k := PositionProperty(conjclass[m], class -> x in class)
               + conjlens[m];
          A[k][j] := A[k][j] + 1;
        fi;
      od;
    od;
  od;
  return [C * A, conjreps];
end);

# same method for ideals

InstallMethod(IsGreensDLeq, "for an inverse op acting semigroup",
[IsActingSemigroupWithInverseOp],
function(S)
  local partial, o, comp_index;

  partial := PartialOrderOfDClasses(S);
  o := LambdaOrb(S);

  comp_index := function(x, y)
    if y in partial[x] then
      return true;
    elif Length(partial[x]) = 1 and partial[partial[x][1]] = partial[x] then
      return false;
    fi;
    return ForAny(partial[x], z -> z <> x and comp_index(z,y));
  end;

  return function(x,y)
    return comp_index(OrbSCCLookup(o)[Position(o, LambdaFunc(S)(x))] - 1,
      OrbSCCLookup(o)[Position(o, LambdaFunc(S)(y))] - 1);
  end;
end);

# same method for ideals

InstallMethod(PrimitiveIdempotents, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc, rank, min, l, min2, m;

  o := LambdaOrb(s);
  scc := OrbSCC(o);
  rank := LambdaRank(s);
  min := ActionDegree(s);

  if MultiplicativeZero(s) = fail then
    for m in [2 .. Length(scc)] do
      l := rank(o[scc[m][1]]);
      if l < min then
        min := l;
      fi;
    od;
    return Idempotents(s, min);
  else
    for m in [2 .. Length(scc)] do
      l := rank(o[scc[m][1]]);
      if l < min then
        min2 := min;
        min := l;
      fi;
    od;
    return Idempotents(s, min2);
  fi;
end);

# same method for ideals

InstallMethod(IsJoinIrreducible,
"for an acting semigroup with inverse op and an associative element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local y, elts, i, k, singleline, sup, j;

  if not x in S then
    Error("Semigroups: IsJoinIrreducible: usage,\n",
          "the second argument <x> must be an element of the first,");
    return;
  fi;

  if IsMultiplicativeZero(S, x) then
    return false;
  fi;

  y := LeftOne(x);
  elts := ShallowCopy(Idempotents(S));

  if IsPartialPermBipartitionSemigroup(S) then
    Sort(elts, PartialPermLeqBipartition);
  else
    elts := Set(elts);
  fi;

  i := Position(elts, y);
  k := 0;
  singleline := true;

  # Find an element smaller than y, k
  for j in [i - 1,i - 2 .. 1] do
    if NaturalLeqInverseSemigroup(elts[j], elts[i]) then
      k := j;
      break;
    fi;
  od;

  # If there is no smaller element k: true
  if k = 0 then
    return true;
  fi;

  # Look for other elements smaller than y which are not smaller than k
  for j in [1 .. (k - 1)] do
    if NaturalLeqInverseSemigroup(elts[j], elts[i]) and not
      NaturalLeqInverseSemigroup(elts[j], elts[k]) then
      singleline := false;
      break;
    fi;
  od;

  if singleline then
    return true;
  elif Size(HClass(S, y)) = 1 then
    return false;
  fi;

  sup := SupremumIdempotentsNC(Minorants(S, y), x);

  return y <> sup
   and ForAny(HClass(S, y), x -> NaturalLeqInverseSemigroup(sup, x) and x <> y);
end);

# same method for ideals

InstallMethod(IsMajorantlyClosed,
"for an acting semigroup with inverse op and an acting semigroup",
[IsActingSemigroupWithInverseOp, IsActingSemigroup],
function(S, T)
  if not IsSubsemigroup(S, T) then
    Error("Semigroups: IsMajorantlyClosed: usage,\n",
          "the second argument must be a subsemigroup of the first,");
    return;
  else
    return IsMajorantlyClosedNC(S, Elements(T));
  fi;
end);

# same method for ideals

InstallMethod(IsMajorantlyClosed,
"for an acting semigroup with inverse op and an associative element collection",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection],
function(S, T)
  if not IsSubset(S, T) then
    Error("Semigroups: IsMajorantlyClosed: usage,\n",
          "the second argument should be a subset of the first,");
    return;
  else
    return IsMajorantlyClosedNC(S, T);
  fi;
end);

# same method for ideals

InstallMethod(IsMajorantlyClosedNC,
"for an acting semigroup with inverse op and associative element collection",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection],
function(S, T)
  local i, iter, t, u;

  if Size(S) = Size(T) then
    return true;
  fi;

  i := 0;
  for t in T do
    iter := Iterator(S);
    for u in iter do
      i := i + 1;
      if NaturalLeqInverseSemigroup(t, u) and not u in T then
        return false;
      fi;
    od;
  od;
  return true;
end);

# same method for ideals

InstallMethod(JoinIrreducibleDClasses,
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  local D, elts, out, seen_zero, rep, i, k, minorants, singleline, d, j, p;

  D := GreensDClasses(S);
  elts := Set(Idempotents(S));
  out := EmptyPlist(Length(D));
  seen_zero := false;

  for d in D do
    rep := Representative(d);

    if not seen_zero and IsMultiplicativeZero(S, rep) then
      seen_zero := true;
      continue;
    fi;

    i := Position(elts, rep);
    k := First([i - 1,i - 2 .. 1],
          j -> NaturalLeqInverseSemigroup(elts[j], rep));

    if k = fail then # d is the minimal non-trivial D-class
      Add(out, d);
      continue;
    fi;

    minorants := [k];
    singleline := true;

    for j in [1 .. k - 1] do
      if NaturalLeqInverseSemigroup(elts[j], rep) then
        if singleline and not NaturalLeqInverseSemigroup(elts[j], elts[k]) then
          # rep is the lub of {elts[j], elts[k]}, not quite
          singleline := false;
          if IsTrivial(SchutzenbergerGroup(d)) then
            break;
          fi;
        else
          Add(minorants, j);
        fi;
      fi;
    od;

    if singleline then
      Add(out, d);
      continue;
    elif IsTrivial(SchutzenbergerGroup(d)) then
      continue;
    fi;

    minorants := Union(List(minorants, j -> DomainOfPartialPerm(elts[j])));

    if DomainOfPartialPerm(rep) = minorants then
      # rep=lub(minorants) but rep not in minorants
      continue;
    fi;

    for p in SchutzenbergerGroup(d) do
      if p <> () and ForAll(MovedPoints(p), x -> not x in minorants) then
        # rep*p<>rep and rep, rep*p>lub(minorants) and rep||rep*p and
        # hence neither rep*p nor rep is of any set.
        Add(out, d);
        break;
      fi;
    od;
  od;

  return out;
end);

# same method for ideals

InstallMethod(JoinIrreducibleDClasses,
"for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(S)
  return Filtered(GreensDClasses(S),
    x -> IsJoinIrreducible(S, Representative(x)));
end);

# same method for ideals

InstallMethod(MajorantClosure,
"for an acting semigroup with inverse op and an acting semigroup",
[IsActingSemigroupWithInverseOp, IsActingSemigroup],
function(S, T)
  if not IsSubsemigroup(S, T) then
    Error("Semigroups: MajorantClosure: usage,\n",
          "the second argument must be a subset of the first,");
    return;
  else
    return MajorantClosureNC(S, Elements(T));
  fi;
end);

# same method for ideals

InstallMethod(MajorantClosure,
"for an acting semigroup with inverse op and associative element collections",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection],
function(S, T)
  if not IsSubset(S, T) then
    Error("Semigroups: MajorantClosure: usage,\n",
          "the second argument must be a subset of the first,");
    return;
  else
    return MajorantClosureNC(S,T);
  fi;
end);

# same method for ideals

InstallMethod(MajorantClosureNC,
"for an acting semigroup with inverse op and associative element collections",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection],
function(S, T)
  local elts, n, out, ht, k, val, t, i;

  elts := Elements(S);
  n := Length(elts);
  out := EmptyPlist(n);
  ht := HTCreate(T[1]);
  k := 0;

  for t in T do
    HTAdd(ht, t, true);
    Add(out, t);
    k := k + 1;
  od;

  for t in out do
    for i in [1 .. n] do
      if NaturalLeqInverseSemigroup(t, elts[i]) then
        val := HTValue(ht, elts[i]);
        if val = fail then
          k := k + 1;
          Add(out, elts[i]);
          HTAdd(ht, elts[i], true);
          if k = Size(S) then
            return out;
          fi;
        fi;
      fi;
    od;
  od;
  return out;
end);

# same method for ideals

#C method? JDM

InstallMethod(Minorants,
"for an acting semigroup with inverse op and associative element collections",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, f)
  local out, elts, i, j, k, NaturalLeq;

  if not f in S then
    Error("Semigroups: Minorant: usage,\n",
          "the second argument is not an element of the first,");
    return;
  fi;

  if HasNaturalPartialOrder(S) then
    elts := Elements(S);
    i := Position(elts, f);
    return elts{NaturalPartialOrder(S)[i]};
  fi;

  if IsIdempotent(f) then #(always true if S is a D-Class rep of an inverse sgp)
    out := EmptyPlist(NrIdempotents(S));
    elts := ShallowCopy(Idempotents(S));
  else
    out := EmptyPlist(Size(S));
    elts := ShallowCopy(Elements(S));
  fi;

  if IsPartialPermBipartitionSemigroup(S) then
    Sort(elts, PartialPermLeqBipartition);
  else
    elts := SSortedList(elts);
  fi;

  i := Position(elts, f);
  j := 0;

  for k in [1 .. i - 1] do
    if NaturalLeqInverseSemigroup(elts[k], f) and f <> elts[k] then
      j := j + 1;
      out[j] := elts[k];
    fi;
  od;
  ShrinkAllocationPlist(out);
  return out;
end);

# same method for ideals
# TODO: rename this RightCosets

InstallMethod(RightCosetsOfInverseSemigroup,
"for acting semigroups with inverse op",
[IsActingSemigroupWithInverseOp, IsActingSemigroupWithInverseOp],
function(S, T)
  local elts, idem, usedreps, out, dupe, coset, s, rep, t;


  if not IsSubsemigroup(S, T) then
    Error("Semigroups: RightCosetsOfInverseSemigroup: usage,\n",
          "the second argument should be a subsemigroup of the first,");
    return;
  fi;

  elts := Elements(T);

  if not IsMajorantlyClosedNC(S, elts) then
    Error("Semigroups: RightCosetsOfInverseSemigroup: usage,\n",
          "the second argument must be majorantly closed,");
    return;
  fi;

  idem := Representative(MinimalIdeal(T));
  usedreps := [];
  out := [];

  for s in RClass(S, idem) do

    # Check if Ts is a duplicate coset
    dupe := false;
    for rep in [1 .. Length(usedreps)] do
      if s * usedreps[rep] ^ - 1 in elts then
        dupe := true;
        break;
      fi;
    od;

    if dupe then
      continue;
    fi;

    Add(usedreps, s);

    coset := [];
    for t in elts do
      Add(coset, t * s);
    od;
    coset := Set(coset);

    # Generate the majorant closure of Ts to create the coset

    coset := MajorantClosureNC(S, coset);
    Add(out, coset);

  od;

  return out;
end);

# same method for ideals

InstallMethod(SameMinorantsSubgroup,
"for a group H-class of an acting semigroup with op",
[IsGroupHClass and IsInverseOpClass and IsActingSemigroupGreensClass],
function(h)
  local S, e, F, out, i, NaturalLeq;

  S := Parent(h);

  e := Representative(h);
  F := Minorants(S, e);
  h := Elements(h);
  out := [e];

  for i in [2 .. Length(h)] do
    if ForAll(F, f -> NaturalLeqInverseSemigroup(f, h[i])) then
      Add(out, h[i]);
    fi;
  od;
  return out;
end);

#

InstallMethod(SmallerDegreePartialPermRepresentation,
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  local oldgens, newgens, D, e, He, sigma, sigmainv, schutz, sup, trivialse,
  psi, psiinv, rho, rhoinv, orbits, cosets, stabpp, stab, h, nrcosets, j, reps,
  lookup, gen, offset, rep, box, subbox, T, d, i, k, m;

  oldgens := Generators(S);
  newgens := List(oldgens, x -> []);
  D := JoinIrreducibleDClasses(S);

  for d in D do

    e := Representative(d);
    ## He is a group H-Class in our join-irreducible D-Class ##
    ## Sigma: isomorphism to a perm group (unfortunately necessary)
    ## Psi: homom from Schutzenberger Group corresponding to He, to a perm group
    ## Rho: isomorphism to a smaller degree perm group
    He := GroupHClass(d);

    sigma := IsomorphismPermGroup(He);
    sigmainv := InverseGeneralMapping(sigma);

    schutz := SchutzenbergerGroup(d);
    sup := SupremumIdempotentsNC(Minorants(S, e), e);
    trivialse := not ForAny(He, x -> NaturalLeqPartialPerm(sup, x) and x <> e);

    psi := ActionHomomorphism(
     schutz, Difference(DomainOfPartialPerm(e), DomainOfPartialPerm(sup)));
    psiinv := InverseGeneralMapping(psi);

    rho := SmallerDegreePermutationRepresentation(Image(psi));
    rhoinv := InverseGeneralMapping(rho);

    ##  Se is the subgroup of He whose elements have the same minorants as e
    if trivialse then
      orbits := [[ActionDegree(He) + 1]];
      cosets := [e];
      #stab:=schutz;
      stabpp := He;
    else
      orbits := Orbits(Image(rho));
    fi;

    for i in orbits do

      if not trivialse then
        stab := ImagesSet(psiinv, ImagesSet(rhoinv,
         Stabilizer(Image(rho), i[1])));
        cosets := RightTransversal(schutz, stab);
        stabpp := ImagesSet(sigmainv, stab);
      fi;

      ##### Generate representatives for all the H-Classes in the R-Class of He
      h := HClassReps( RClassNC(d, e) );
      nrcosets := Size(h) * Length(cosets);

      # Generate representatives for ALL the cosets the generator will act
      #on  #### Divide every H-Class in the R-Class into 'cosets' like stab in
      #He
      j := 0;
      reps := [];
      lookup := EmptyPlist(Length(LambdaOrb(d)));
      for k in [1 .. Size(h)] do
        lookup[Position(LambdaOrb(d), ImageSetOfPartialPerm(h[k]))] := k;
        for m in [1 .. Length(cosets)] do
          j := j + 1;
          reps[j] := cosets[m] * h[k];
        od;
      od;
      ShrinkAllocationPlist(lookup);

      ##### Loop over old generators of S to calculate its action on the cosets
      for j in [1 .. Length(oldgens)] do

        gen := oldgens[j];
        offset := Length(newgens[j]);

        # Loop over cosets to calculate the image of each under the generator
        for k in [1 .. nrcosets] do
          rep := reps[k] * gen;
          # Will the new generator will be defined at this point?
          if not rep * rep ^ ( - 1) in stabpp then
            Add(newgens[j], 0);
          else
            box := lookup[Position(LambdaOrb(d), ImageSetOfPartialPerm(rep))];
            if trivialse then
              subbox := 1;
            else
              ## Below, could be ^sigma instead of AsPermutation
              subbox := PositionCanonical(cosets,
               AsPermutation((rep * h[box] ^ ( - 1))));
            fi;
            Add(newgens[j], (box - 1) * Length(cosets) + subbox + offset);
          fi;
        od;
      od;
    od;
  od;


  T := InverseSemigroup(List(newgens, x -> PartialPermNC(x)));

  # Return identity mapping if nothing has been accomplished; else the result.
  if NrMovedPoints(T) > NrMovedPoints(S)
    or (NrMovedPoints(T) = NrMovedPoints(S)
        and ActionDegree(T) >= ActionDegree(S)) then
    return IdentityMapping(S);
  else
    return MagmaIsomorphismByFunctionsNC(S, T,
      x -> EvaluateWord(GeneratorsOfSemigroup(T), Factorization(S, x)),
      x -> EvaluateWord(GeneratorsOfSemigroup(S), Factorization(T, x)));
  fi;

end);

#TODO review and generalise this...

InstallGlobalFunction(SupremumIdempotentsNC,
function(coll, type)
  local dom, i, part, rep, reps, out, todo, inter;

  if IsPartialPerm(type) then

    if IsList(coll) and IsEmpty(coll) then
      return PartialPerm([]);
    elif not IsPartialPermCollection(coll) then
      Error("Semigroups: SupremumIdempotentsNC: usage,\n",
            "the argument must be a collection of partial perms,");
      return;
    fi;
    dom := DomainOfPartialPermCollection(coll);
    return PartialPerm(dom, dom);

  elif IsBipartition(type) and IsBlockBijection(type) then

    if IsList(coll) and IsEmpty(coll) then
      return Bipartition(Concatenation([1 .. DegreeOfBipartition(type)],
        - [1 .. DegreeOfBipartition(type)]));
    elif not IsBipartitionCollection(coll) then
      Error("Semigroups: SupremumIdempotentsNC: usage,\n",
            "the argument must be a collection of block bijections,");
      return;
    fi;

    reps := List(coll,ExtRepOfBipartition);
    todo := [1 .. DegreeOfBipartition(type)];
    out := [];
    for i in todo do
      inter := [];
      for rep in reps do
        for part in rep do
          if i in part then
            Add(inter, part);
            break;
          fi;
        od;
      od;
      inter := Intersection(inter);
      AddSet(out, inter);
      todo := Difference(todo,inter);
    od;
    return Bipartition(out);

  elif IsBipartition(type) and IsPartialPermBipartition(type) then
    #FIXME shouldn't there be a check here like above?
    return AsBipartition(SupremumIdempotentsNC(
      List(coll, AsPartialPerm), PartialPerm([])));
  else
    Error("Semigroups: SupremumIdempotentsNC: usage,\n",
          "the argument must be a collection of partial perms, block ",
          "bijections,\n", "or partial perm bipartitions,");
    return;
  fi;
end);

# same method for ideals

InstallMethod(VagnerPrestonRepresentation,
"for an acting semigroup with inverse operation",
[IsActingSemigroupWithInverseOp],
function(S)
  local gens, elts, out, iso, T, inv, i;

  gens := GeneratorsOfSemigroup(S);
  elts := Elements(S);
  out := EmptyPlist(Length(gens));

  iso := function(x)
    local dom;
    dom := Set(elts * (x ^ - 1));
    return PartialPermNC(List(dom, y -> Position(elts, y)),
     List(List(dom, y -> y * x), y -> Position(elts, y)));
  end;

  for i in [1 .. Length(gens)] do
    out[i] := iso(gens[i]);
  od;

  T := InverseSemigroup(out);
  inv := x -> EvaluateWord(GeneratorsOfSemigroup(S), Factorization(T, x));

  return MagmaIsomorphismByFunctionsNC(S, T, iso, inv);
end);

#

