############################################################################
##
#W  reesmat-iso.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# FIXME
# 1) \< and \= are incompatible, it could be that map1 < map2 and map1 = map2
#

# TODO
#
# 1) SEMIGROUPS_HashFunctionMatrixOfRMS (requires RZMS/RMS matrices to have
#    their own type)

#############################################################################
## This file contains functions for isomorphisms and automorphisms of Rees
## matrix and 0-matrix semigroup. The algorithm used in this file is described
## in:
##
##   J. Araújo, P. von Bünau, J. D. Mitchell, and M. Neunhoeffer,
##   ‘Computing automorphisms of semigroups’,
##   J. Symbolic Comput. 45 (2010) 373-392;
##   http://dx.doi.org/10.1016/j.jsc.2009.10.001.
##
## This file is organized as follows:
##
##   1. Internal functions for the avoidance of duplicate code.
##
##   2. Automorphism group functions (the main functions in this file)
##
##   3. Methods for automorphism group of RMS and RZMS (ViewObj,
##      IdentityMapping etc)
##
##   4. Isomorphisms
##
##   5. RMS/RZMSIsoByTriple (equality, <, ImagesRepresentative)
##
#############################################################################

#############################################################################
# 1. Internal functions
#############################################################################

# Same as <Info> but without a line break at the end if the boolean <linebreak>
# is false.

BindGlobal("SEMIGROUPS_Info",
function(level, linebreak, arg)

  if InfoLevel(InfoSemigroups) >= level then
    Apply(arg, String);
    if not linebreak then
      Print("#I  ");
    fi;
    Print(Concatenation(arg));
    if linebreak then
      Print("\n");
    fi;
  fi;
end);

# Find the stabiliser of the matrix of a Rees (0-)matrix semigroup under the
# action of the automorphism group of the R(Z)MSGraph via rearranging rows and
# columns

BindGlobal("SEMIGROUPS_StabOfRMSMatrix",
function(G, R)
  local OnMatrix, H, m, n;

  m := Length(Matrix(R)[1]);
  n := Length(Matrix(R));

  OnMatrix := function(mat, x)
    local rows;
    mat := StructuralCopy(mat);
    rows := Permutation(x, [1 .. n], function(i, p)
                                       return (i + m) ^ p - m;
                                     end);

    return List(Permuted(mat, rows), y -> Permuted(y, x));
  end;
  SEMIGROUPS_Info(2, false, "finding the stabilizer of matrix . . . ");
  H := Stab(G, Matrix(R), OnMatrix).stab;
  SEMIGROUPS_Info(2, true, Size(H));
  return H;
end);

# Find the pointwise stabiliser of the entries in the matrix of a Rees
# (0-)matrix semigroup under the action of the automorphism group of the
# underlying group.

BindGlobal("SEMIGROUPS_StabOfRMSEntries",
function(G, R)
  local H, entries, i;
  SEMIGROUPS_Info(2, false, "finding the stabilizer of matrix entries . . . ");
  H := G;
  entries := MatrixEntries(R);
  i := PositionProperty(entries, x -> x <> 0 and x <> ());

  while not IsTrivial(H) and i <= Length(entries) do
    H := Stabilizer(H, entries[i], OnPoints);
    i := i + 1;
  od;

  SEMIGROUPS_Info(2, true, Size(H));
  return H;
end);

# An elementary pruner for the backtrack search in the direct product V (to
# find the subgroup U).

BindGlobal("SEMIGROUPS_RMSIsoPruner",
function(U, V)
  local blist, right;

  if IsTrivial(U) then
    return false;
  fi;
  blist := BlistList([1 .. Index(V, U)], [1]);
  right := RightTransversal(V, U);
  return
    function(chain, j, x, y, word)
      local pos;
      pos := PositionCanonical(right, x);
      if blist[pos] then  # we visited this coset before. . .
        return false;     # don't continue searching in this subtree. . .
      else
        blist[pos] := true;
        return true;
      fi;
    end;
end);

#

BindGlobal("SEMIGROUPS_RMSInducedFunction",
function(R, l, g, x)
  local mat, m, n, out, j, i;

  mat := Matrix(R);
  m := Length(mat[1]);
  n := Length(mat);
  out := EmptyPlist(m + n);
  out[1] := x;
  #FIXME for loop for this and the next
  out{[m + 1 .. n + m]} := List([m + 1 .. n + m],
                                v -> mat[v ^ l - m][1 ^ l] * x
                                         * (mat[v - m][1] ^ g) ^ -1);

  out{[2 .. m]} := List([2 .. m],
                        v -> (mat[(m + 1) ^ l - m][v ^ l]) ^ -1 * out[m + 1]
                             * (mat[1][v] ^ g));
  for j in [m + 2 .. n + m] do
    for i in [2 .. m] do
      if mat[j ^ l - m][i ^ l] <> out[j] * mat[j - m][i] ^ g * out[i] ^ -1 then
        return fail;
      fi;
    od;
  od;

  return out;
end);

#

if IsGrapeLoaded then

  BindGlobal("SEMIGROUPS_RZMSInducedFunction",
  function(R, l, g, x, component)
    local mat, m, n, graph, rep, out, edges, bicomps, sub, perm, defined, orb,
    j, Last, involved, verts, v, new, k;

    mat := Matrix(R);
    m := Length(mat[1]);
    n := Length(mat);
    graph := RZMSGraph(R);
    rep := Minimum(component);
    out := EmptyPlist(m + n);
    out[rep] := x;

    if Length(component) = Length(Vertices(graph)) then
      edges := DirectedEdges(graph);
      bicomps := Bicomponents(graph);
    else
      sub := InducedSubgraph(graph, component);
      perm := MappingPermListList(VertexNames(sub), Vertices(sub));
      edges := OnTuplesTuples(DirectedEdges(sub), perm ^ -1);
      bicomps := OnTuplesTuples(Bicomponents(sub), perm ^ -1);
    fi;

    defined := [];
    orb := [rep];
    j := 0;

    repeat
      j := j + 1;
      Last := orb[j];

      involved := Filtered(edges, x -> x[1] = Last and not x in defined);

      if not involved = [] then
        verts := List(involved, x -> x[2]);
        Append(orb, Filtered(verts, x -> not x in orb));

        for k in [1 .. Length(verts)] do
          v := verts[k];

          if Last in bicomps[1] then
            new := mat[v ^ l - m][Last ^ l] * out[Last]
                  * (mat[v - m][Last] ^ g) ^ -1;
          else
            new := (mat[Last ^ l - m][v ^ l]) ^ -1 * out[Last]
                   * (mat[Last - m][v] ^ g);
          fi;

          if not IsBound(out[v]) then
            out[v] := new;
          elif not out[v] = new then
            return fail;
          fi;
          defined := Union(defined, [involved[k], Reversed(involved[k])]);
        od;
      fi;
    until defined = edges;

    return out;
  end);

  #

  BindGlobal("SEMIGROUPS_RZMStoRZMSInducedFunction",
  function(rms1, rms2, l, g, groupelts)
    local mat1, mat2, m, n, rmsgraph, components, reps, imagelist, edges,
    bicomps, sub, perm, defined, orb, j, Last, involved, verts, v, new, i, k;

    mat1 := Matrix(rms1);
    mat2 := Matrix(rms2);
    m := Length(mat1[1]);
    n := Length(mat1);
    rmsgraph := RZMSGraph(rms1);
    components := ConnectedComponents(rmsgraph);

    if not Length(groupelts) = Length(components) then
      ErrorMayQuit("Semigroups: SEMIGROUPS_RZMStoRZMSInducedFunction: ",
                   "usage,\nthe 5th argument must be a list of length ",
                   Length(components), ",");
    fi;

    reps := List(components, Minimum);
    imagelist := [];
    imagelist{reps} := groupelts;

    for i in [1 .. Length(components)] do
      if Length(components) = 1 then
        edges := DirectedEdges(rmsgraph);
        bicomps := Bicomponents(rmsgraph);
      else
        sub := InducedSubgraph(rmsgraph, components[i]);
        perm := MappingPermListList(VertexNames(sub), Vertices(sub));
        edges := OnTuplesTuples(DirectedEdges(sub), perm ^ -1);
        bicomps := OnTuplesTuples(Bicomponents(sub), perm ^ -1);
      fi;

      defined := [];
      orb := [reps[i]];
      j := 0;

      repeat
        j := j + 1;
        Last := orb[j];
        involved := Filtered(edges, x -> x[1] = Last and not x in defined);
        if not involved = [] then

          verts := List(involved, x -> x[2]);
          Append(orb, Filtered(verts, x -> not x in orb));

          for k in [1 .. Length(verts)] do
            v := verts[k];

            if Last in bicomps[1] then
              new := mat2[v ^ l - m][Last ^ l]
                     * imagelist[Last] * (mat1[v - m][Last] ^ g) ^ -1;
            else
              new := (mat2[Last ^ l - m][v ^ l]) ^ -1
                     * imagelist[Last] * (mat1[Last - m][v] ^ g);
            fi;

            if not IsBound(imagelist[v]) then
              imagelist[v] := new;
            elif not imagelist[v] = new then
              return fail;
            fi;
            defined := Union(defined, [involved[k], Reversed(involved[k])]);
          od;
        fi;
      until defined = edges;
    od;
    return imagelist;
  end);
fi;

#############################################################################
# 2. Automorphism group functions
#############################################################################

if not IsBound(GAPInfo.PackagesLoaded.genss) then
  InstallMethod(AutomorphismGroup, "for a Rees 0-matrix semigroup",
  [IsReesZeroMatrixSemigroup],
  function(R)
    Info(InfoWarning, 1, "the GENSS package is not loaded, ",
         "and so this function does not work");
    return fail;
  end);
else
  # don't hit F5 for local variables here, it crashes vim!!
  InstallMethod(AutomorphismGroup, "for a Rees 0-matrix semigroup",
  [IsReesZeroMatrixSubsemigroup],
  function(R)
    local G, m, n, aut_graph, components, t, aut_group, stab_aut_graph,
          hom, stab_aut_group, i, V, A, gens1, gens2, U, T, tester,
          g, x, map, cart;

    G := UnderlyingSemigroup(R);

    if not (IsReesZeroMatrixSemigroup(R) and IsPermGroup(G)
            and IsZeroSimpleSemigroup(R)) then
      TryNextMethod(); #TODO write such a method
    fi;

    m := Length(Rows(R));
    n := Length(Columns(R));

    # automorphism group of the graph . . .
    SEMIGROUPS_Info(2, false, "finding automorphisms of the graph . . . ");

    aut_graph := AutGroupGraph(RZMSGraph(R), [[1 .. m], [m + 1 .. n + m]]);

    if Length(GeneratorsOfGroup(aut_graph)) = 0 then
      aut_graph := Group(());
    fi;
    SEMIGROUPS_Info(2, true, Size(aut_graph), " found");

    Size(aut_graph); # for genss

    # stabiliser of the matrix under rearranging rows and columns by elements
    # of the automorphism group of the graph
    if m * n < 1000 then
      stab_aut_graph := SEMIGROUPS_StabOfRMSMatrix(aut_graph, R);
    else
      stab_aut_graph := Group(());
    fi;

    # automorphism group of the underlying group
    SEMIGROUPS_Info(2, false, "finding the automorphism group of the group",
                              " . . . ");
    aut_group := AutomorphismGroup(G);
    SEMIGROUPS_Info(2, true, "found ", Size(aut_group));

    # pointwise stabiliser of the entries in the matrix of R under the
    # automorphism group of the group
    stab_aut_group := SEMIGROUPS_StabOfRMSEntries(aut_group, R);

    # homomorphism from Aut(G) to a perm rep of Aut(G) / Inn(G)
    # gaplint: ignore 2
    hom := NaturalHomomorphismByNormalSubgroupNC(aut_group,
             InnerAutomorphismsAutomorphismGroup(aut_group));
    hom := CompositionMapping(IsomorphismPermGroup(ImagesSource(hom)), hom);

    # V is isomorphic to Aut(Gamma) x (Aut(G) / Inn(G))
    # U is a subgroup of V contained in the subgroup we are looking for.
    V := DirectProduct(aut_graph, Image(hom));

    if IsTrivial(stab_aut_graph) and IsTrivial(stab_aut_group) then
      U := Group(());
    else
      gens1 := GeneratorsOfGroup(stab_aut_graph);
      gens2 := GeneratorsOfGroup(Image(hom, stab_aut_group));
      U := Group(Concatenation(Images(Embedding(V, 1), gens1),
                               Images(Embedding(V, 2), gens2)));
    fi;

    components := ConnectedComponents(RZMSGraph(R));
    t := Length(components);
    Info(InfoSemigroups, 2, "the graph has ", t, " connected components");

    T := RightTransversal(G, Centre(G));
    A := [];

    ##########################################################################
    # test whether for <x> in <V> there is a <map> such that
    # phi := [x ^ Projection(V, 1), x ^ Projection(V, 2) ^ hom ^ -1, map]
    # is a RZMSIsoByTriple, and add <phi> to <A> if it is!
    ##########################################################################

    tester := function(x)
      local y, found, g, i, tmp, map;
      y := PreImagesRepresentative(hom, x ^ Projection(V, 2));
      x := x ^ Projection(V, 1);

      tmp := [];
      found := false;
      for g in T do
        map := SEMIGROUPS_RZMSInducedFunction(R, x, y, g, components[1]);
        if map <> fail then
          tmp := tmp + map;
          found := true;
          break;
        fi;
      od;
      i := 1;
      while found and i < t do
        i := i + 1;
        found := false;
        for g in G do
          map := SEMIGROUPS_RZMSInducedFunction(R, x, y, g, components[i]);
          if map <> fail then
            tmp := tmp + map;
            found := true;
            break;
          fi;
        od;
      od;

      if found then
        AddSet(A, RZMSIsoByTriple(R, R, [x, y, tmp]));
      fi;
      return found;
    end;

    ##########################################################################

    if U <> V then # some search required
      Info(InfoSemigroups, 2, "backtracking in the direct product of size ",
           Size(V), " . . . ");
      BacktrackSearchStabilizerChainSubgroup(StabilizerChain(V),
                                             tester,
                                             SEMIGROUPS_RMSIsoPruner(U, V));
    else # U = V
      Perform(GeneratorsOfGroup(V), tester);
    fi;

    cart := [[]];
    for g in T do
      map := SEMIGROUPS_RZMSInducedFunction(R,
                                            One(aut_graph),
                                            One(aut_group),
                                            g,
                                            components[1]);
      if map <> fail then
        Add(cart[1], map);
      fi;
    od;

    for i in [2 .. t] do
      cart[i] := [];
      for g in G do
        map := SEMIGROUPS_RZMSInducedFunction(R,
                                              One(aut_graph),
                                              One(aut_group),
                                              g,
                                              components[i]);
        if map <> fail then
          Add(cart[i], map);
        fi;
      od;
    od;

    # put B together
    for map in EnumeratorOfCartesianProduct(cart) do
      x := RZMSIsoByTriple(R, R, [One(aut_graph), One(aut_group), Sum(map)]);
      AddSet(A, x);
    od;

    A := Group(A);
    SetIsAutomorphismGroupOfRMSOrRZMS(A, true);
    SetIsFinite(A, true);

    return A;
  end);
fi;

#

if not IsBound(GAPInfo.PackagesLoaded.genss) then
  InstallMethod(AutomorphismGroup, "for a Rees matrix semigroup",
  [IsReesMatrixSemigroup],
  function(R)
    Info(InfoWarning, 1, "the GENSS package is not loaded, ",
         "and so this function does not work");
    return fail;
  end);
else
  # don't hit F5 for local variables here, it crashes vim!!
  InstallMethod(AutomorphismGroup, "for a Rees matrix semigroup",
  [IsReesMatrixSemigroup],
  function(R)
    local G, m, n, aut_graph, aut_group, stab_aut_graph,
          hom, stab_aut_group, V, A, gens1, gens2, U, T, tester,
          g, map, gens;

    G := UnderlyingSemigroup(R);
    if not (IsReesMatrixSemigroup(R) and IsPermGroup(G)
            and IsSimpleSemigroup(R)) then
      TryNextMethod(); #TODO write such a method
    fi;

    m := Length(Rows(R));
    n := Length(Columns(R));

    # automorphism group of the graph . . .
    # this is easy since the graph is complete bipartite
    SEMIGROUPS_Info(2, false, "finding automorphisms of the graph . . . ");

    if n = 1 and m = 1 then
      gens := GeneratorsOfGroup(AutomorphismGroup(G));
      if IsEmpty(gens) then
        gens := [IdentityMapping(G)];
      fi;
      A := Group(List(gens, x -> RMSIsoByTriple(R, R,
                                                [(), x, [One(G), One(G)]])));
      SetIsAutomorphismGroupOfRMSOrRZMS(A, true);
      SetIsFinite(A, true);
      return A;
    elif n = 2 and m = 1 then
      aut_graph := Group((2, 3));
      SetSize(aut_graph, 2);
    elif n > 2 and m = 1 then
      aut_graph := Group((2, 3),
                         PermList(Concatenation([1], [3 .. n + m], [2])));
      SetSize(aut_graph, Factorial(n));
    else
      aut_graph := DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
    fi;
    SEMIGROUPS_Info(2, true, Size(aut_graph), " found");
    Size(aut_graph); # for genss

    # stabiliser of the matrix under rearranging rows and columns by elements
    # of the automorphism group of the graph
    if m * n < 1000 then
      stab_aut_graph := SEMIGROUPS_StabOfRMSMatrix(aut_graph, R);
    else
      stab_aut_graph := Group(());
    fi;

    # automorphism group of the underlying group
    SEMIGROUPS_Info(2, false, "finding the automorphism group of the group",
                              " . . . ");
    aut_group := AutomorphismGroup(G);
    SEMIGROUPS_Info(2, true, "found ", Size(aut_group));

    # pointwise stabiliser of the entries in the matrix of R under the
    # automorphism group of the group
    stab_aut_group := SEMIGROUPS_StabOfRMSEntries(aut_group, R);

    # homomorphism from Aut(G) to a perm rep of Aut(G) / Inn(G)
    # gaplint: ignore 2
    hom := NaturalHomomorphismByNormalSubgroupNC(aut_group,
             InnerAutomorphismsAutomorphismGroup(aut_group));
    hom := CompositionMapping(IsomorphismPermGroup(ImagesSource(hom)), hom);

    # V is isomorphic to Aut(Gamma) x (Aut(G) / Inn(G))
    # U is a subgroup of V contained in the subgroup we are looking for.
    V := DirectProduct(aut_graph, Image(hom));

    if IsTrivial(stab_aut_graph) and IsTrivial(stab_aut_group) then
      U := Group(());
    else
      gens1 := GeneratorsOfGroup(stab_aut_graph);
      gens2 := GeneratorsOfGroup(Image(hom, stab_aut_group));
      U := Group(Concatenation(Images(Embedding(V, 1), gens1),
                               Images(Embedding(V, 2), gens2)));
    fi;

    T := RightTransversal(G, Centre(G));
    A := [];

    ##########################################################################
    # test whether for <x> in <V> there is a <map> such that
    # phi := [x ^ Projection(V, 1), x ^ Projection(V, 2) ^ hom ^ -1, map]
    # is a RMSIsoByTriple, and add <phi> to <A> if it is!
    ##########################################################################

    tester := function(x)
      local y, map, g;
      y := PreImagesRepresentative(hom, x ^ Projection(V, 2));
      x := x ^ Projection(V, 1);
      for g in T do
        map := SEMIGROUPS_RMSInducedFunction(R, x, y, g);
        if map <> fail then
          AddSet(A, RMSIsoByTriple(R, R, [x, y, map]));
          return true;
        fi;
      od;
      return false;
    end;

    ##########################################################################

    if U <> V then
      Info(InfoSemigroups, 2, "backtracking in the direct product of size ",
           Size(V), ". . .");
      BacktrackSearchStabilizerChainSubgroup(StabilizerChain(V),
                                             tester,
                                             SEMIGROUPS_RMSIsoPruner(U, V));
    else # U = V
      Perform(GeneratorsOfGroup(V), tester);
    fi;

    for g in T do
      map := SEMIGROUPS_RMSInducedFunction(R,
                                           One(aut_graph),
                                           One(aut_group),
                                           g);
      if map <> fail then
        AddSet(A, RMSIsoByTriple(R, R, [One(aut_graph), One(aut_group), map]));
      fi;
    od;

    A := Group(A);
    SetIsAutomorphismGroupOfRMSOrRZMS(A, true);
    SetIsFinite(A, true);

    return A;
  end);
fi;

#############################################################################
# 3. Methods for automorphism groups
#############################################################################

InstallMethod(ViewObj,
"for the automorphism group of a Rees (0-)matrix semigroup",
[IsAutomorphismGroupOfRMSOrRZMS],
function(G)
  Print("<automorphism group of ");
  ViewObj(Source(G.1));
  Print(" with ", Length(GeneratorsOfGroup(G)), " generator");
  if Length(GeneratorsOfGroup(G)) > 1 then
    Print("s");
  fi;
  Print(">");
end);

InstallMethod(IsomorphismPermGroup,
"for the automorphism group of a Rees (0-)matrix semigroup",
[IsAutomorphismGroupOfRMSOrRZMS],
function(G)
  local R, H, iso, x;

  R := Source(Representative(G)); # the Rees (0-)matrix semigroup
  H := [];
  for x in GeneratorsOfGroup(G) do
    Add(H, Permutation(x, R, POW));
  od;
  H := Group(H);

  iso := GroupHomomorphismByImagesNC(G,
                                     H,
                                     GeneratorsOfGroup(G),
                                     GeneratorsOfGroup(H));
  SetInverseGeneralMapping(iso,
                           GroupHomomorphismByImagesNC(H,
                                                       G,
                                                       GeneratorsOfGroup(H),
                                                       GeneratorsOfGroup(G)));
  SetNiceMonomorphism(G, iso);
  SetIsHandledByNiceMonomorphism(G, true);
  UseIsomorphismRelation(G, H);
  return iso;
end);

#

InstallMethod(IdentityMapping, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
function(R)
  local G;
  G := UnderlyingSemigroup(R);
  return RMSIsoByTriple(R, R,
                        [(),
                         IdentityMapping(G),
                         List([1 .. Length(Columns(R)) + Length(Rows(R))],
                              x -> One(G))]);
end);

#

InstallMethod(IdentityMapping, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local G, tup;
  G := UnderlyingSemigroup(R);
  tup := List([1 .. Length(Columns(R)) + Length(Rows(R))], x -> One(G));
  return RZMSIsoByTriple(R, R, [(), IdentityMapping(G), tup]);
end);

#############################################################################
# 4. Isomorphisms
#############################################################################

InstallMethod(IsomorphismSemigroups, "for Rees matrix semigroups",
[IsReesMatrixSemigroup, IsReesMatrixSemigroup],
function(R1, R2)
  local mat, m, n, g, g1, g2, iso, isograph, isogroup, map, l, tup;

  if Size(R1) = Size(R2) and Columns(R1) = Columns(R2)
      and Rows(R1) = Rows(R2) then
    mat := Matrix(R1);
    m := Length(mat[1]);
    n := Length(mat);

    if R1 = R2 then
      g := UnderlyingSemigroup(R1);
      return RMSIsoByTriple(R1, R2, [(),
                                     IdentityMapping(g),
                                     List([1 .. m + n], x -> One(g))]);
    else
      g1 := UnderlyingSemigroup(R1);
      g2 := UnderlyingSemigroup(R2);
      iso := IsomorphismGroups(g1, g2);

      # for RMS without 0 the graphs are always isomorphic,
      # being complete bipartite.

      if not iso = fail then
        isograph := DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
        #all isomorphisms from g1 to g2
        isogroup := List(Elements(AutomorphismGroup(g1)), x -> x * iso);

        #find an induced function, if there is one
        for l in isograph do
          for g in isogroup do
            for tup in Elements(g2) do
              map := SEMIGROUPS_RMSInducedFunction(R2, l, g, tup);
              if map <> fail then
                return RMSIsoByTriple(R1, R2, [l, g, map]);
              fi;
            od;
          od;
        od;
      fi;
    fi;
  fi;
  return fail;
end);

#
if not (IsGrapeLoaded and IsGrapeCompiled) then

  InstallMethod(IsomorphismSemigroups, "for Rees 0-matrix semigroups",
  [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup],
  function(R1, R2)
    Info(InfoWarning, 1, GrapeIsNotCompiledString);
    return fail;
  end);

else

  InstallMethod(IsomorphismSemigroups, "for Rees 0-matrix semigroups",
  [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup],
  function(R1, R2)
    local G1, G2, mat, m, n, f, groupiso, graph1, graph2, g, graphiso, tuples,
          map, l, tup;

    G1 := UnderlyingSemigroup(R1);
    G2 := UnderlyingSemigroup(R2);

    if not (IsRegularSemigroup(R1) and IsGroup(G1) and IsRegularSemigroup(R2)
            and IsGroup(G2)) then
      ErrorMayQuit("Semigroups: IsomorphismSemigroups: usage,\n",
                   "the arguments must be regular Rees 0-matrix semigroups ",
                   "over groups,");
    fi;

    if not (Size(R1) = Size(R2) and Columns(R1) = Columns(R2)
            and Rows(R1) = Rows(R2)) then
      return fail;
    fi;

    mat := Matrix(R1);
    m := Length(mat[1]);
    n := Length(mat);

    if R1 = R2 then
      return RZMSIsoByTriple(R1, R2, [(),
                                      IdentityMapping(G1),
                                      List([1 .. m + n], x -> One(G2))]);
    fi;

    # every isomorphism of the groups
    f := IsomorphismGroups(G1, G2);
    if f = fail then
      return fail;
    fi;
    groupiso := List(AutomorphismGroup(G1), x -> x * f);

    # every isomorphism of the graphs
    graph1 := RZMSGraph(R1);
    graph2 := RZMSGraph(R2);
    if UndirectedEdges(graph1) <> UndirectedEdges(graph2) then
      g := GraphIsomorphism(graph1, graph2);
      if g = fail then
        return fail;
      fi;
    else
      g := ();
    fi;
    graphiso := List(AutGroupGraph(graph1, [[1 .. m], [m + 1 .. n + m]]),
                     x -> x * g);

    # gaplint: ignore 2
    tuples := EnumeratorOfCartesianProduct(
                List([1 .. Length(ConnectedComponents(graph1))], x -> G2));
    #find an induced function, if there is one
    for l in graphiso do
      for g in groupiso do
        for tup in tuples do #FIXME it should be possible to cut this down
          map := SEMIGROUPS_RZMStoRZMSInducedFunction(R1, R2, l, g, tup);
          if not map = false then
            return RZMSIsoByTriple(R1, R2, [l, g, map]);
          fi;
        od;
      od;
    od;
    return fail;
  end);
fi;

#############################################################################
# 5. RMS/RZMSIsoByTriple
#############################################################################

InstallGlobalFunction(RMSIsoByTriple,
function(R1, R2, triple)
  local fam, out;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(R1)),
                               ElementsFamily(FamilyObj(R2)));
  out := Objectify(NewType(fam, IsRMSIsoByTriple), rec(triple := triple));
  SetSource(out, R1);
  SetRange(out, R2);

  return out;
end);

#

InstallGlobalFunction(RZMSIsoByTriple,
function(R1, R2, triple)
  local fam, out;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(R1)),
                               ElementsFamily(FamilyObj(R2)));
  out := Objectify(NewType(fam, IsRZMSIsoByTriple), rec(triple := triple));
  SetSource(out, R1);
  SetRange(out, R2);
  return out;
end);

#

InstallMethod(ELM_LIST, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple, IsPosInt],
function(x, i)
  return x!.triple[i];
end);

#

InstallMethod(ELM_LIST, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple, IsPosInt],
function(x, i)
  return x!.triple[i];
end);

#

InstallMethod(\=, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple, IsRMSIsoByTriple],
function(x, y)

  if Source(x) <> Source(y) or Range(x) <> Range(y) then
    return false;
  fi;

  if x[1] = y[1] and x[2] = y[2] and x[3] = y[3] then
    return true;
  fi;

  return OnTuples(GeneratorsOfSemigroup(Source(x)), x)
       = OnTuples(GeneratorsOfSemigroup(Source(x)), y);
end);

#

InstallMethod(\=, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(x, y)

  if Source(x) <> Source(y) or Range(x) <> Range(y) then
    return false;
  fi;

  if x[1] = y[1] and x[2] = y[2] and x[3] = y[3] then
    return true;
  fi;

  return OnTuples(GeneratorsOfSemigroup(Source(x)), x)
         = OnTuples(GeneratorsOfSemigroup(Source(x)), y);
end);

#

InstallMethod(\<, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple, IsRMSIsoByTriple],
function(x, y)
  return (x[1] < y[1]) or (x[1] = y[1] and x[2] < y[2])
    or (x[1] = y[1] and x[2] = y[2] and x[3] < y[3]);
end);

#

InstallMethod(\<, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(x, y)
  return (x[1] < y[1]) or (x[1] = y[1] and x[2] < y[2])
    or (x[1] = y[1] and x[2] = y[2] and x[3] < y[3]);
end);

#

InstallMethod(CompositionMapping2, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple, IsRMSIsoByTriple],
function(x, y)
  local n;
  n := Length(Rows(Source(x))) + Length(Columns(Source(x)));
  return RMSIsoByTriple(Source(x),
                        Range(y),
                        [x[1] * y[1],
                         x[2] * y[2],
                         List([1 .. n],
                              i -> y[3][i ^ x[1]] * x[3][i] ^ y[2])]);
end);

#

InstallMethod(CompositionMapping2, "for objects in `IsRZMSIsoByTriple'",
IsIdenticalObj, [IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(x, y)
  local n;
  n := Length(Rows(Source(x))) + Length(Columns(Source(x)));
  return RZMSIsoByTriple(Source(x),
                         Range(y),
                         [x[1] * y[1],
                          x[2] * y[2],
                          List([1 .. n],
                               i -> y[3][i ^ x[1]] * x[3][i] ^ y[2])]);
end);

#

InstallMethod(ImagesElm, "for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function(triple, x)
  return [ImagesRepresentative(triple, x)];
end);

#

InstallMethod(ImagesElm, "for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(triple, x)
  return [ImagesRepresentative(triple, x)];
end);

#

InstallMethod(ImagesRepresentative,
"for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function(map, elt)
  local m;
  m := Length(Rows(Source(map)));
  return RMSElementNC(Range(map),
                      elt[1] ^ map[1],
                      map[3][elt[1]] * ImageElm(map[2], elt[2]) /
                        map[3][elt[3]],
                      (elt[3] + m) ^ map[1] - m);
end);

#

InstallMethod(ImagesRepresentative,
"for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(map, elt)
  local m;

  if elt <> MultiplicativeZero(Source(map)) and map[3][elt[1]] <> 0
      and map[3][elt[3]] <> 0 then
    m := Length(Rows(Source(map)));
    return RMSElementNC(Range(map),
                        elt[1] ^ map[1],
                        map[3][elt[1]] * ImageElm(map[2], elt[2]) /
                          map[3][elt[3]],
                        (elt[3] + m) ^ map[1] - m);
  fi;
  return elt;
end);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'",
[IsEndoGeneralMapping and IsRMSIsoByTriple],
function(map)
  local n;

  n := Length(Rows(Source(map))) + Length(Columns(Source(map)));

  return RMSIsoByTriple(Range(map),
                        Source(map),
                        [map[1] ^ -1,
                         map[2] ^ -1,
                         List([1 .. n],
                              i -> (map[3][i ^ map[1]] ^ (map[2] ^ -1))
                                    ^ -1)]);
end);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'",
[IsEndoGeneralMapping and IsRMSIsoByTriple and IsOne], x -> x);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(map)
  local n;

  n := Length(Rows(Source(map))) + Length(Columns(Source(map)));

  return RZMSIsoByTriple(Range(map),
                         Source(map),
                         [map[1] ^ -1,
                          map[2] ^ -1,
                          List([1 .. n],
                               i -> (map[3][i ^ map[1]] ^ (map[2] ^ -1))
                                    ^ -1)]);
end);

#

InstallMethod(IsOne, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple],
function(map)
  return IsOne(map[1]) and IsOne(map[2]) and ForAll(map[3], IsOne);
end);

#

InstallMethod(IsOne, "for objects in `IsRZMSIsoByTriple'",
[IsEndoGeneralMapping and IsRZMSIsoByTriple],
function(map)
  return IsOne(map[1]) and IsOne(map[2]) and ForAll(map[3], IsOne);
end);

#

InstallMethod(PreImagesRepresentative,
"for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function(map, x)
  return ImagesRepresentative(map ^ -1, x);
end);

#

InstallMethod(PreImagesRepresentative,
"for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(map, x)
  return ImagesRepresentative(map ^ -1, x);
end);

#

InstallMethod(PrintObj, "for an object in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple],
function(map)
  Print("RMSIsoByTriple ( ", Source(map), ", ", Range(map), ", [", map[1],
        ", ", map[2], ", ", map[3], "])");
  return;
end);

#

InstallMethod(PrintObj, "for an object in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(map)
  Print("RZMSIsoByTriple ( ", Source(map), ", ", Range(map), ", ", map[1],
        ", ", map[2], ", ", map[3], " )");
  return;
end);

#

InstallMethod(ViewObj, "for an object in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple],
function(map)
  Print("(", map[1], ", ", map[2], ", ", map[3], ")");
end);

#

InstallMethod(ViewObj, "for object in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(map)
  Print("(", map[1], ", ", map[2], ", ", map[3], ")");
end);
