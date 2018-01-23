#############################################################################
##
##  semieunit.gi
##  Copyright (C) 2017                                    Christopher Russell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

#############################################################################
# Methods for creating McAlister triple semigroups
#############################################################################

InstallMethod(McAlisterTripleSemigroup,
"for a group, digraph, digraph, and action",
[IsGroup, IsDigraph, IsDigraph, IsFunction],
function(G, X, Y, act)
  local anti_act, hom, out_nbrs, orbs, min, fam, filt, M, x, y;

  if not IsFinite(G) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                  "the first argument must be a finite group,");
  fi;

  anti_act := function(pt, g)
    return act(pt, g ^ -1);
  end;

  hom := ActionHomomorphism(G, DigraphVertices(X), anti_act);

  if not IsSubgroup(AutomorphismGroup(X), Image(hom)) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                  "the first argument (a group) must act by order ",
                  "automorphisms on the second argument (a partial order ",
                  "digraph),");
  elif not IsPartialOrderDigraph(X) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                  "the second argument must be a partial order digraph,");
  fi;

  # Check that Y is a semilattice and an induced subdigraph of X
  if not Y = InducedSubdigraph(X, DigraphVertexLabels(Y)) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                  "the third argument <Y> must be an induced subdigraph of\n",
                  "the second argument <X> with vertex labels corresponding\n",
                  "to the vertices of <X> on which <Y> was induced,");
  elif not IsJoinSemilatticeDigraph(Y) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                  "the third argument must be a join-semilattice digraph,");
  fi;

  # Check condition M2 (check that Y is an order ideal of X.)
  # TODO: implement IsOrderIdeal for a subset of a partial order digraph.
  out_nbrs := OutNeighbors(X);
  for x in DigraphVertices(X) do
    if not x in DigraphVertexLabels(Y) then
      for y in DigraphSources(DigraphRemoveLoops(Y)) do
        if x in out_nbrs[DigraphVertexLabel(Y, y)] then
          ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                        "the out-neighbours of each vertex of <X> which is ",
                        "in <Y> must contain only vertices which are in <Y> ",
                        "- see the documentation for more detail,");
        fi;
      od;
    fi;
  od;

  orbs := Orbits(Image(hom));

  # Check condition M3 (check that G.Y = X.)
  if not ForAll(orbs, o -> ForAny(DigraphVertexLabels(Y), v -> v in o)) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                  "every vertex of <X> must be in the orbit of some vertex ",
                  "of <X> which is in <Y> - see the documentation ",
                  "for more detail,");
  fi;

  for x in DigraphVertices(X) do
    if not x in Union(orbs) and not (x in DigraphVertexLabels(Y)) then
      ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: usage,\n",
                    "every vertex of <X> must be in the orbit of some ",
                    "vertex of <X> which is in <Y> - see the documentation",
                    " for more detail,");
    fi;
  od;

  # Check condition M4 (essentially, check that G.Y = X is connected.)
  min := DigraphVertexLabel(Y, DigraphSinks(DigraphRemoveLoops(Y))[1]);
  if ForAny(GeneratorsOfGroup(Image(hom)), g -> min ^ g <> min) then
      ErrorNoReturn("Semigroups: McAlisterTripleSemigroup: \n",
                    "<act> must fix the vertex of <X> which is the minimal ",
                    "vertex of <Y> - see the documentation for more detail,");
  fi;

  fam := NewFamily("McAlisterTripleSemigroupFamily",
                   IsMcAlisterTripleSemigroupElement);

  # Check if this McAlister triple semigroup is a monoid
  if IsMeetSemilatticeDigraph(Y) then
    filt := IsMcAlisterTripleSemigroup and IsMonoid;
  else
    filt := IsMcAlisterTripleSemigroup;
  fi;

  # Create the semigroup itself
  M := Objectify(NewType(CollectionsFamily(fam), filt and IsAttributeStoringRep
                         and IsEUnitaryInverseSemigroup and IsWholeFamily),
                 rec());

  M!.elementType := NewType(fam, IsMcAlisterTripleSemigroupElementRep);

  SetMcAlisterTripleSemigroupGroup(M, G);
  SetMcAlisterTripleSemigroupAction(M, anti_act);
  SetMcAlisterTripleSemigroupPartialOrder(M, X);
  SetMcAlisterTripleSemigroupSemilattice(M, Y);

  GeneratorsOfSemigroup(M);
  return M;
end);

InstallMethod(McAlisterTripleSemigroup,
"for a perm group, digraph, and digraph",
[IsPermGroup, IsDigraph, IsDigraph],
function(G, X, Y)
  return McAlisterTripleSemigroup(G, X, Y, OnPoints);
end);

InstallMethod(McAlisterTripleSemigroup,
"for a perm group, digraph, homogeneous list, and action",
[IsGroup, IsDigraph, IsHomogeneousList, IsFunction],
function(G, X, sub_ver, act)
  return McAlisterTripleSemigroup(G, X, InducedSubdigraph(X, sub_ver), act);
end);

InstallMethod(McAlisterTripleSemigroup,
"for a perm group, digraph, and homogeneous list",
[IsPermGroup, IsDigraph, IsHomogeneousList],
function(G, X, sub_ver)
  return McAlisterTripleSemigroup(G, X, InducedSubdigraph(X, sub_ver),
                                  OnPoints);
end);

#############################################################################
# Methods for McAlister triple semigroups
#############################################################################

InstallMethod(OneImmutable, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  local Y;
  if not IsMonoid(S) then
    return fail;
  fi;
  Y := McAlisterTripleSemigroupSemilattice(S);
  return MTSE(S, DigraphSources(DigraphRemoveLoops(Y))[1], ());
end);

# (A, g) in S if and only if Ag^-1 is a vertex of the semilattice of S
InstallMethod(AsList, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  local out, g, A, V;
  out := [];
  V := DigraphVertexLabels(McAlisterTripleSemigroupSemilattice(S));
  for g in McAlisterTripleSemigroupGroup(S) do
    for A in V do
      if (McAlisterTripleSemigroupAction(S)(A, Inverse(g)) in V) then
        Add(out, MTSE(S, A, g));
      fi;
    od;
  od;
  SetMcAlisterTripleSemigroupElmList(S, out);
  return out;
end);

InstallMethod(String, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  local G, X, Y;
  G := McAlisterTripleSemigroupGroup(S);
  X := McAlisterTripleSemigroupPartialOrder(S);
  Y := McAlisterTripleSemigroupSemilattice(S);
  return Concatenation("McAlisterTripleSemigroup(", String(G), ", ",
                       String(X), ", ", String(DigraphVertexLabels(Y)), ")");
end);

InstallMethod(PrintObj, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  Print(String(S));
  return;
end);

# TODO Linebreak hints

InstallMethod(ViewString, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  local G;
  G := McAlisterTripleSemigroupGroup(S);
  return Concatenation("<McAlister triple semigroup over ", ViewString(G), ">");
end);

InstallMethod(IsomorphismSemigroups, "for two McAlister triple semigroups",
[IsMcAlisterTripleSemigroup, IsMcAlisterTripleSemigroup],
function(S, T)
  local YS, YT, XT, iso_g, iso_x, im_YS, rep, A;

  iso_g := IsomorphismGroups(McAlisterTripleSemigroupGroup(S),
                             McAlisterTripleSemigroupGroup(T));

  if iso_g = fail then
    return fail;
  fi;

  YS := McAlisterTripleSemigroupSemilattice(S);
  YT := McAlisterTripleSemigroupSemilattice(T);
  XT := McAlisterTripleSemigroupPartialOrder(T);

  if not IsIsomorphicDigraph(YS, YT) then
    return fail;
  fi;

  iso_x := IsomorphismDigraphs(McAlisterTripleSemigroupPartialOrder(S), XT);

  if iso_x = fail then
    return fail;
  fi;

  im_YS := List(DigraphVertexLabels(YS), a -> a ^ iso_x);
  # if the restriction of iso_x to DigraphVertexLabels(YS) is not
  # DigraphVertexLabels(YT) then we need to compose iso_x with an
  # automorphism of McAlisterTripleSemilattice(T). Composing this with
  # iso_x will restrict to an isomorphism from (the labels of) YS to YT.
  if not im_YS = DigraphVertexLabels(YT) then
    A := AutomorphismGroup(XT);
    rep := RepresentativeAction(A, im_YS, DigraphVertexLabels(YT), OnSets);
    if rep = fail then
      return fail;
    fi;
  else
    rep := ();
  fi;

  if ForAll(McAlisterTripleSemigroupGroup(S),
      g -> ForAll(DigraphVertices(McAlisterTripleSemigroupPartialOrder(S)),
      x -> (McAlisterTripleSemigroupAction(S)(x, g)) ^ (rep * iso_x) =
      McAlisterTripleSemigroupAction(T)((x ^ iso_x), (g ^ iso_g)) ^ rep)) then
    return MappingByFunction(S, T, s -> MTSE(T, s[1] ^ iso_x, s[2] ^ iso_g));
  fi;

  return fail;
end);

#############################################################################
# Methods for McAlister triple elements
#############################################################################
InstallMethod(McAlisterTripleSemigroupElement,
"for a McAlister triple semigroup, pos int, and perm",
[IsMcAlisterTripleSemigroup, IsPosInt, IsMultiplicativeElementWithInverse],
function(S, A, g)
  if not A in DigraphVertexLabels(McAlisterTripleSemigroupSemilattice(S)) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroupElement: usage,\n",
                  "second argument should be a vertex label of the ",
                  "join-semilattice of the McAlister triple,");
  elif not g in McAlisterTripleSemigroupGroup(S) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroupElement: usage,\n",
                  "third argument must an element of the group of the ",
                  "McAlister triple,");
  elif not (McAlisterTripleSemigroupAction(S)(A, g ^ -1) in
      DigraphVertexLabels(McAlisterTripleSemigroupSemilattice(S))) then
    ErrorNoReturn("Semigroups: McAlisterTripleSemigroupElement: usage,\n",
                  "the arguments do not specify an element of the McAlister ",
                  "triple semigroup,");
  fi;
  return Objectify(S!.elementType, [A, g, S]);
end);

InstallMethod(ELM_LIST,
"for a McAlister triple semigroup element rep and a pos int",
[IsMcAlisterTripleSemigroupElementRep, IsPosInt],
function(x, i)
  if i <= 2 then
    return x![i];
  fi;
  ErrorNoReturn("Semigroups: ELM_LIST (for a McAlisterTripleSemigroupElement)",
                ": usage,\n", "the index must be at most 2,");
end);

InstallMethod(McAlisterTripleSemigroupElementParent,
"for a McAlister triple semigroup element rep",
[IsMcAlisterTripleSemigroupElementRep],
function(x)
  return x![3];
end);

InstallMethod(String, "for a McAlister triple semigroup element rep",
[IsMcAlisterTripleSemigroupElementRep],
function(x)
  return Concatenation("MTSE(", String(x![3]), ", ", String(x![1]), ", ",
                       String(x![2]), ")");
end);

# TODO Linebreak hints

InstallMethod(ViewString, "for a McAlister triple semigroup element rep",
[IsMcAlisterTripleSemigroupElementRep],
function(x)
  return Concatenation("(", ViewString(x[1]), ", ", ViewString(x[2]), ")");
end);

InstallMethod(\=, "for two McAlister triple semigroup element reps",
IsIdenticalObj,
[IsMcAlisterTripleSemigroupElementRep, IsMcAlisterTripleSemigroupElementRep],
function(x, y)
  if x![1] = y![1] and x![2] = y![2] and x![3] = y![3] then
    return true;
  fi;
  return false;
end);

InstallMethod(\*, "for two McAlister triple semigroup element reps",
[IsMcAlisterTripleSemigroupElementRep, IsMcAlisterTripleSemigroupElementRep],
function(x, y)
  local S;
  S := McAlisterTripleSemigroupElementParent(x);
  if not S = McAlisterTripleSemigroupElementParent(y) then
    ErrorNoReturn("Semigroups: \* (for an McAlisterTripleSemigroupElement): ",
                  "usage,\n", "the elements must be from the same McAlister ",
                  "triple semigroup,");
  fi;
  return MTSE(S, DigraphVertexLabel(McAlisterTripleSemigroupPartialOrder(S),
               PartialOrderDigraphJoinOfVertices(
                 McAlisterTripleSemigroupPartialOrder(S), x[1],
                 McAlisterTripleSemigroupAction(S)(y[1], x[2]))),
             x[2] * y[2]);
end);

InstallMethod(\<, "for two McAlister triple semigroup element reps",
[IsMcAlisterTripleSemigroupElementRep, IsMcAlisterTripleSemigroupElementRep],
function(x, y)
  return x[1] < y[1] or (x[1] = y[1] and x[2] < y[2]);
end);

InstallMethod(InverseOp, "for a McAlister triple semigroup element rep",
[IsMcAlisterTripleSemigroupElementRep],
function(x)
  return MTSE(x![3], McAlisterTripleSemigroupAction(x![3])(x[1], Inverse(x[2])),
            Inverse(x[2]));
end);

InstallMethod(\^, "for a McAlister triple semigroup element and a negative int",
              [IsMcAlisterTripleSemigroupElement, IsNegInt],
function(x, i)
  return InverseOp(x ^ - i);
end);

#############################################################################
# Implementing IsomorphismSemigroup for IsMcAlisterTripleSemigroup
#############################################################################
SEMIGROUPS.EUISPrincipalRightIdeal := function(S, e)
  local elements;
  elements := ShallowCopy(Elements(S));
  Apply(elements, x -> e * x);
  return DuplicateFreeList(elements);
end;

SEMIGROUPS.McAlisterTripleSemigroupSemilatticeIsomorphism := function(S, cong)
  local iso, s, YY;

  iso := function(s)
    local ideal;
    ideal := SEMIGROUPS.EUISPrincipalRightIdeal(S, s);
    return Set(ideal, x -> [InverseOp(x) * x,
                            CongruenceClassOfElement(cong, x)]);
  end;

  YY := Set(Idempotents(S), e -> iso(e));
  return MappingByFunction(S, Domain(YY), iso);
end;

SEMIGROUPS.McAlisterTripleSemigroupConstructPartialOrder := function(S, Y, G)
  local g, A, sc, out;

  out := ShallowCopy(Y);
  for g in G do  # TODO: if Ag = A then should not check powers of g
    for A in Y do
      sc := ShallowCopy(A);
      sc := Set(sc, x -> [x[1], g * x[2]]);
      if not sc in out then
        Append(out, [sc]);
        out := Set(out);
      fi;
    od;
  od;

  return out;
end;

SEMIGROUPS.McAlisterTripleSemigroupConstructAction := function(xx, map)
  local map2, act, pt, g;

  map2 := InverseGeneralMapping(map);
  act := function(pt, g)
    local out;
    out := Set(ShallowCopy(xx[pt]), x -> [x[1], (g ^ map2) * x[2]]);
    return Position(xx, out);
  end;

  return act;
end;

SEMIGROUPS.McAlisterTripleSemigroupConstructStandardPartialOrder := function(x)
  local  sizes, vertices, adjacency, a, i, j, intr;

  sizes := ShallowCopy(x);
  Apply(sizes, a -> Size(a));
  vertices := [1 .. Size(x)];
  adjacency := [];

  for i in vertices do
    Append(adjacency, [[i]]);
  od;

  for i in vertices do
    for j in vertices do
      if i < j then
        # ignore symmetric pairs
        intr := Intersection(x[i], x[j]);
        if Size(intr) = sizes[i] then
          Append(adjacency[j], [i]);
        elif Size(intr) = sizes[j] then
          Append(adjacency[i], [j]);
        fi;
      fi;
    od;
  od;

  Perform(adjacency, Sort);

  return Digraph(adjacency);
end;

InstallMethod(IsomorphismMcAlisterTripleSemigroup,
"for a semigroup",
[IsSemigroup],
function(S)
  local cong, grp, map_y, map_yy, map_g, yy, xx,
        labels, x, y, iso, anti_act, act, M;

  if not IsEUnitaryInverseSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismSemigroup: usage,\n",
                  "the semigroup is not E-unitary,");
  fi;

  cong := MinimumGroupCongruence(S);
  grp := S / cong;
  # This is G.
  map_yy := SEMIGROUPS.McAlisterTripleSemigroupSemilatticeIsomorphism(S, cong);
  yy := Set(Image(map_yy));
  # Construct Y as in Howie.
  xx := SEMIGROUPS.McAlisterTripleSemigroupConstructPartialOrder(S, yy, grp);
  # Construct X as in Howie.

  # Next we create the digraphs x and y for the McAlister triple semigroup.
  x := SEMIGROUPS.McAlisterTripleSemigroupConstructStandardPartialOrder(xx);
  # The elements of yy may not be the first elements of xx.
  map_y := function(a)
    return Position(xx, a);
  end;
  map_y := MappingByFunction(Domain(yy), Domain(Set([1 .. Size(xx)])), map_y);

  labels := ShallowCopy(yy);
  Apply(labels, a -> Position(xx, a));
  y := InducedSubdigraph(x, labels);
  SetDigraphVertexLabels(y, labels);

  # The semigroup quotient group is not a group object, so find an isomorphism.
  map_g := IsomorphismPermGroup(grp);
  map_g := CompositionMapping(
             SmallerDegreePermutationRepresentation(Image(map_g)), map_g);

  # Create the action of Image(map_g) on x - this is the action of G on X.
  anti_act := SEMIGROUPS.McAlisterTripleSemigroupConstructAction(xx, map_g);
  act := function(pt, g)
    return(anti_act(pt, g ^ -1));
  end;

  M := McAlisterTripleSemigroup(Range(map_g), x, y, act);
  iso := function(s)
    local t;
    t := s;
    return MTSE(M, (t ^ map_yy) ^ map_y,
               CongruenceClassOfElement(cong, s) ^ map_g);
  end;
  return MappingByFunction(S, M, iso);
end);

InstallMethod(IsomorphismSemigroup,
"for IsMcAlisterTripleSemigroup and a semigroup",
[IsMcAlisterTripleSemigroup, IsSemigroup],
function(filt, S)
  return IsomorphismMcAlisterTripleSemigroup(S);
end);

InstallMethod(IsWholeFamily, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroupElementCollection],
function(C)
  return Size(Elements(C)[1]![3]) = Size(C);
end);

InstallMethod(ChooseHashFunction, "for McAlister triple semigroup elements",
[IsMcAlisterTripleSemigroupElement, IsInt],
function(x, hashlen)
  local data;
  data := [ChooseHashFunction(x[1], hashlen),
           ChooseHashFunction(x[2], hashlen),
           hashlen];
  return rec(func := SEMIGROUPS.HashFunctionForMcAlisterTripleSemigroupElements,
             data := data);
end);

SEMIGROUPS.HashFunctionForMcAlisterTripleSemigroupElements := function(x, data)
  return  (17 * data[1].func(x[1], data[1].data)
           + data[2].func(x[2], data[2].data)) mod data[3] + 1;
end;

###############################################################################
# F-inverse Semigroups
###############################################################################
# The connected components of the natural partial order will be the
# congruence classes of the minmum group congruence. Thus we can simply
# check that precisely one of the sources of the digraph of the natural
# partial order is in each connected component.
InstallMethod(IsFInverseMonoid, "for a semigroup",
[IsSemigroup],
function(S)
  local comp, po;
  if not IsInverseMonoid(S) then
    return false;
  fi;
  po := Digraph(NaturalPartialOrder(S));
  for comp in DigraphConnectedComponents(po).comps do
    if not Size(Intersection(comp, DigraphSources(po))) = 1 then
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(IsFInverseMonoid, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  return IsMonoid(S) and IsFInverseSemigroup(S);
end);

# A McAlister triple semigroup is F-inverse precisely when X, the partial
# order, is a join-semilattice.
InstallMethod(IsFInverseSemigroup, "for a McAlister triple semigroup",
[IsMcAlisterTripleSemigroup],
function(S)
  return IsJoinSemilatticeDigraph(McAlisterTripleSemigroupPartialOrder(S));
end);

# For an inverse semigroup S we denote \sigma_{e,f}  = \sigma \cap eSf x eSf.
# An E-unitary inverse semigroup is said to be an F-inverse semigroup if
# for each pair of idempotents (e,f): each \sigma_{e,f} class has a maximal
# element. It is simpler to find an isomorphism and use the above method.
InstallMethod(IsFInverseSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if not IsEUnitaryInverseSemigroup(S) then
    return false;
  fi;
  return IsFInverseSemigroup(AsSemigroup(IsMcAlisterTripleSemigroup, S));
end);

###############################################################################
# Find E-unitary inverse covers
###############################################################################
InstallMethod(EUnitaryInverseCover,
"for an inverse partial perm semigroup",
[IsInverseSemigroup and IsPartialPermCollection],
function(S)
  local gens, deg, units, G, P, embed, id, cover_gens, s, g, cover, i;
  gens := GeneratorsOfSemigroup(S);
  deg := DegreeOfPartialPermSemigroup(S);
  units := [];
  for s in gens do
    Add(units, SEMIGROUPS.PartialPermExtendToPerm(s, deg));
  od;
  G := InverseSemigroup(units);

  P := DirectProduct(S, G);
  embed := SemigroupDirectProductInfo(P).embedding;
  if not IsMonoid(S) then
    id := PartialPerm([1 .. deg]);
  fi;
  cover_gens := [];
  for i in [1 .. Size(gens)] do
    s := embed(gens[i], 1);
    g := embed(units[i], 2);
    if not IsMonoid(S) then
      g := JoinOfPartialPerms(g, id);
    fi;
    Add(cover_gens, s * g);
  od;

  cover := SemigroupDirectProductInfo(P).projection;
  return MappingByFunction(InverseSemigroup(cover_gens), S, x -> cover(x, 1));
end);

InstallMethod(EUnitaryInverseCover,
"for an inverse semigroup",
[IsSemigroup],
function(S)
  local cov, iso, T;
  if not IsInverseSemigroup(S) then
    ErrorNoReturn("Semigroups: EUnitaryInverseCover: usage,\n",
                  "the argument must be an inverse semigroup,");
  fi;
  iso := IsomorphismPartialPermSemigroup(S);
  T := Range(iso);
  cov := EUnitaryInverseCover(T);
  return CompositionMapping(InverseGeneralMapping(iso), cov);
end);

# This method extends a partial perm 'x' to a permutation of degree 'deg'.
SEMIGROUPS.PartialPermExtendToPerm := function(x, deg)
  local c, i, dom, image;
  image := [];
  # Turn all components into cycles.
  for c in ComponentsOfPartialPerm(x) do
    image[c[1]] := OnPoints(c[1], x);
    if Size(c) > 1 then
      for i in [1 .. Size(c) - 1] do
        image[c[i]] := OnPoints(c[i], x);
      od;
      image[c[i + 1]] := c[1];
    fi;
  od;
  dom := [1 .. deg];
  # Map everything else to itself.
  for i in dom do
    if not IsBound(image[i]) then
      image[i] := i;
    fi;
  od;
  return(PartialPerm(dom, image));
end;

###############################################################################
# TODO:
# 1) Write hash function that works when group is not a perm group.
# 2) Consider hash function for improvements.
# 3) Write OrderIdeal and FindOrderIrreducibleElements for digraphs package
#    (order irreducible elements are the ones which generate the semilattice
#    and order ideals relate to checking condition M2 from Howie).
# 4) Improve GeneratorsOfSemigroup method.
# 5) Line break hints for printing MTSEs and McAlisterTripleSemigroups.
# 6) Implement EUnitaryInverseCover which covers with a McAlisterTriple
# 7) Improve EUnitaryInverseCover by finding smaller covers
# 8) Improve IsomorphismSemigroup method.
# 9) Improve 'if not IsSubgroup(AutomorphismGroup(X), Image(hom)) then' line of
# McAlisterTripleSemigroup
###############################################################################

