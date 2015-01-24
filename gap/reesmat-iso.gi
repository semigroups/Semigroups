############################################################################
##
#W  reesmat-iso.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains functions for isomorphisms and automorphisms of Rees matrix
# and 0-matrix semigroup.

#InstallGlobalFunction(HashFunctionMatrixOfRMS,
#function(P, data)
#  return Sum(List(P, x-> ORB_HashFunctionForPlainFlatList(x, data))) mod
# data+1;
#end);

#

#InstallMethod(ChooseHashFunction,
#"for Rees matrix semigroup matrix, and integer",
#[IsListOrCollection and IsDenseList and IsHomogeneousList, IsInt],
#function(P, data)
#  return rec(func:=HashFunctionMatrixOfRMS, data:=data);
#end);

#

InstallMethod(ViewObj, "for the automorphism group of a Rees matrix semigroup",
[IsAutomorphismGroupOfRMS],
function(A)
  Print("<automorphism group of ");
  ViewObj(Source(A.1));
  Print( " with ", Length(GeneratorsOfGroup(A)), " generator");
  if Length(GeneratorsOfGroup(A)) > 1 then
    Print("s");
  fi;
  Print(">");
  return;
end);

#

InstallMethod(IsomorphismPermGroup,
"for the automorphism group of a Rees matrix semigroup",
[IsAutomorphismGroupOfRMS],
function(A)
  local B, R, iso, x;

  B := [];
  R := Source(A.1);
  for x in GeneratorsOfGroup(A) do
    Add(B, Permutation(x, R, POW));
  od;
  B := Group(B);

  iso := GroupHomomorphismByImagesNC(A, B, GeneratorsOfGroup(A),
   GeneratorsOfGroup(B));
  SetInverseGeneralMapping(iso,
    GroupHomomorphismByImagesNC(B, A, GeneratorsOfGroup(B),
    GeneratorsOfGroup(A)));
  SetNiceMonomorphism(A, iso);
  SetIsHandledByNiceMonomorphism(A, true);
  UseIsomorphismRelation(A, B);
  return iso;
end);

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
  InstallMethod(AutomorphismGroup, "for a Rees matrix semigroup",
  [IsReesMatrixSemigroup],
  function(R)
    local G, mat, m, n, agroup, A, hom, agraph, OnMatrix, S1, S2, mat_elts, U,
    V, iso, inv, T, tester, y, D, P, elts, B, g, x, proj1, proj2, stab, gens, i,
    blist, right, pruner, lambda, gamma, entries;

    G := UnderlyingSemigroup(R);
    if not IsGroup(G) then
      return fail;
    fi;
    mat := Matrix(R);
    m := Length(mat[1]);
    n := Length(mat);

    if n = 1 and m = 1 then
      return Group(List(GeneratorsOfGroup(AutomorphismGroup(G)), x ->
       RMSIsoByTriple(R, R, [(), x, [One(G), One(G)]])));
    elif n = 2 and m = 1 then
      agraph := Group((2,3));
      SetSize(agraph, 2);
    elif n > 2 and m = 1 then
      agraph := Group((2,3), PermList(Concatenation([1],[3 .. n + m],[2])));
      SetSize(agraph, Factorial(n));
    else
      agraph := DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
    fi;

    Info(InfoSemigroups, 2,
    "calculating the automorphism group of the group...");
    agroup := AutomorphismGroup(G);
    Info(InfoSemigroups, 3, "...it has size", Size(agroup));

    OnMatrix := function(mat, x)
      local x2;
      mat := StructuralCopy(mat);
      x2 := Permutation(x, [1 .. n], function(i, p)
                                       return (i + m) ^ p - m;
                                     end);

      return List(Permuted(mat, x2), y -> Permuted(y, x));
    end;

    Info(InfoSemigroups, 2, "calculating the stabilizer of the matrix...");
    S1 := Stab(agraph, mat, OnMatrix);
    Info(InfoSemigroups, 2, "...it has size ", Size(S1.stab));

    Info(InfoSemigroups, 2, "calculating an isomorphism from the automorphisms",
    " of the group to a\n#I  perm group...");
    hom := NaturalHomomorphismByNormalSubgroupNC(agroup,
     InnerAutomorphismsAutomorphismGroup(agroup));
    iso := IsomorphismPermGroup(ImagesSource(hom));
    #iso:=IsomorphismPermGroup(agroup);
    inv := InverseGeneralMapping(iso);

    Info(InfoSemigroups, 2,
    "calculating the stabilizer of the matrix entries...");
    S2 := agroup;
    entries := MatrixEntries(R);
    if entries[1] = () then
      i := 1;
    else
      i := 0;
    fi;

    while not IsTrivial(S2) and i < Length(entries) do
      i := i + 1;
      S2 := Stabilizer(S2, entries[i], OnPoints);
    od;

    Info(InfoSemigroups, 2, "...it has size ", Size(S2));

    V := DirectProduct(agraph, Image(iso));
    if S1.size <> 1 or not IsTrivial(S2) then
      U := Group(Concatenation(
        Images(Embedding(V, 1), GeneratorsOfGroup(S1.stab)),
        #Images(Embedding(V, 2), GeneratorsOfGroup(Image(iso, S2)))));
        Images(Embedding(V, 2),
          GeneratorsOfGroup(Image(iso, Image(hom, S2))))));
    else
      U := Group(());
    fi;

    proj1 := Projection(V, 1);
    proj2 := Projection(V, 2);
    T := RightTransversal(G, Centre(G));

    Info(InfoSemigroups, 2,
    "calculating a stabilizer chain for the direct product",
    " of the automorphism\n#I  groups of the group and the graph...");
    stab := StabilizerChain(V);

    if Size(U) <> Size(V) then
      if Size(U) = 1 then
        pruner := false;
      else
        blist := BlistList([1 .. Index(V, U)], [1]);
        right := RightTransversal(V, U);
        pruner := function(chain, j, x, y, word)
          local pos;
          pos := PositionCanonical(right, x);
          if blist[pos] then  # we visited this coset before...
            return false;     # don't continue searching in this subtree...
          else
            blist[pos] := true;
            return true;
          fi;
        end;
      fi;

      tester := function(x)
        local g;
        for g in T do
          if RMSInducedFunction(R, x,
            PreImagesRepresentative(hom, (x ^ proj2) ^ inv), g)[1] then
          #if RMSInducedFunction(R, x, (x^proj2)^inv, g)[1] then
            return true;
          fi;
        od;
        return false;
      end;

      Info(InfoSemigroups, 2, "backtracking in the direct product of size ",
      Size(V), "...");
      stab := BacktrackSearchStabilizerChainSubgroup(stab, tester, pruner);
    fi;

    V := Group(stab!.orb!.gens);

    Info(InfoSemigroups, 2, "converting generators of the subgroup...");
    A := [];
    for x in GeneratorsOfGroup(V) do
      lambda := x ^ proj1;
      #gamma:=(x^proj2)^inv;
      gamma := PreImagesRepresentative(hom, (x ^ proj2) ^ inv);
      for g in T do
        x := RMSInducedFunction(R, lambda, gamma, g);
        if x[1] then
          x := RMSIsoByTriple(R, R, [lambda, gamma, x[2]]);
          AddSet(A, x);
          break;
        fi;
      od;
    od;

    for g in T do
      x := RMSInducedFunction(R, One(agraph), One(agroup), g);
      if x[1] then
        x := RMSIsoByTriple(R, R, [One(agraph), One(agroup), x[2]]);
        AddSet(A, x);
      fi;
    od;

    A := Group(A);
    SetIsGroupOfAutomorphisms(A, true);
    SetIsAutomorphismGroupOfRMS(A, true);
    SetIsFinite(A, true);

    return A;
  end);
fi;

#

InstallMethod(IdentityMapping, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
function(R)
  local G;
  G := UnderlyingSemigroup(R);
  return RMSIsoByTriple(R, R, [(), IdentityMapping(G),
   List([1 .. Length(Columns(R)) + Length(Rows(R))], x -> One(G))]);
end);

#

InstallMethod(IdentityMapping, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local G;
  G := UnderlyingSemigroup(R);
  return RZMSIsoByTriple(R, R, [(), IdentityMapping(G),
   List([1 .. Length(Columns(R)) + Length(Rows(R))], x -> One(G))]);
end);

#

InstallGlobalFunction(RMSInducedFunction,
function(R, l, g, x)
  local mat, m, n, out, j, i;

  mat := Matrix(R);
  m := Length(mat[1]);
  n := Length(mat);
  out := EmptyPlist(m + n);
  out[1] := x;
  #FIXME for loop for this and the next
  out{[m + 1 .. n + m]} :=
   List([m + 1 .. n + m],
   v -> mat[v ^ l - m][1 ^ l] * x * (mat[v - m][1] ^ g) ^ - 1);

  out{[2 .. m]} := List([2 .. m],
   v -> (mat[(m + 1) ^ l - m][v ^ l]) ^ - 1 * out[m + 1] *
    (mat[1][v] ^ g));

  for j in [m + 2 .. n + m] do
    for i in [2 .. m] do
      if mat[j ^ l - m][i ^ l] <> out[j] * mat[j - m][i] ^ g * out[i] ^ - 1 then
        return [false, out];
      fi;
    od;
  od;

  return [true, out];
end);

#

if not IsGrapeLoaded then

  InstallGlobalFunction(RZMSInducedFunction,
  function(R, l, g, x, component)
    Info(InfoWarning, 1, GrapeIsNotLoadedString);
    return fail;
  end);

  InstallGlobalFunction(RZMStoRZMSInducedFunction,
  function(rms1, rms2, l, g, groupelts)
    Info(InfoWarning, 1, GrapeIsNotLoadedString);
    return fail;
  end);

else

  InstallGlobalFunction(RZMSInducedFunction,
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
      edges := OnTuplesTuples(DirectedEdges(sub), perm ^ - 1);
      bicomps := OnTuplesTuples(Bicomponents(sub), perm ^ - 1);
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
                  * (mat[v - m][Last] ^ g) ^ - 1;
          else
            new := (mat[Last ^ l - m][v ^ l]) ^ - 1 * out[Last]
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

  InstallGlobalFunction(RZMStoRZMSInducedFunction,
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
      Error("Semigroups: RZMStoRZMSInducedFunction: usage,\n",
            "the 3rd arg must be a list of length ", Length(components),
            ",");
      return;
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
        edges := OnTuplesTuples(DirectedEdges(sub), perm ^ - 1);
        bicomps := OnTuplesTuples(Bicomponents(sub), perm ^ - 1);
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
                     * imagelist[Last] * (mat1[v - m][Last] ^ g) ^ - 1;
            else
              new := (mat2[Last ^ l - m][v ^ l]) ^ - 1
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

#

InstallMethod(IsomorphismSemigroups, "for Rees matrix semigroups",
[IsReesMatrixSemigroup, IsReesMatrixSemigroup],
function(R1, R2)
  local mat, m, n, g, g1, g2, iso, isograph, isogroup, candidate, l, tup;

  if (Size(R1) = Size(R2) and
   ColumnsOfReesMatrixSemigroup(R1) = ColumnsOfReesMatrixSemigroup(R2) and
   RowsOfReesMatrixSemigroup(R1) = RowsOfReesMatrixSemigroup(R2)) then

    mat := Matrix(R1);
    m := Length(mat[1]);
    n := Length(mat);

    if R1 = R2 then
      g := UnderlyingSemigroup(R1);
      return RMSIsoByTriple(R1, R2, [(), IdentityMapping(g), List([1 .. m + n],
       x -> One(g))]);
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
              candidate := RMSInducedFunction(R2, l, g, tup);
              if not candidate[1] = false then
                return RMSIsoByTriple(R1, R2, [l, g, candidate[2]]);
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
    local G1, G2, mat, m, n, f, groupiso, graph1, graph2, g, graphiso,
    candidate, l, tup;

    G1 := UnderlyingSemigroup(R1);
    G2 := UnderlyingSemigroup(R2);

    if not (IsRegularSemigroup(R1) and IsGroup(G1) and IsRegularSemigroup(R2)
      and IsGroup(G2)) then
      Error("Semigroups: IsomorphismSemigroups: usage,\n",
            "the arguments must be regular Rees 0-matrix semigroups ",
            "over groups,");
      return;
    fi;

    if not (Size(R1) = Size(R2) and Columns(R1) = Columns(R2)
      and Rows(R1) = Rows(R2)) then
      return fail;
    fi;

    mat := Matrix(R1);
    m := Length(mat[1]);
    n := Length(mat);

    if R1 = R2 then
      return RZMSIsoByTriple(R1, R2, [(), IdentityMapping(G1),
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
      graphiso := List(AutGroupGraph(graph1,
                  [[1 .. m],[m + 1 .. n + m]]), x -> x * g);
    fi;

    #find an induced function, if there is one
    for l in graphiso do
      for g in groupiso do
        for tup in Elements(G2) do
          candidate := RZMStoRZMSInducedFunction(R1, R2, l, g, [tup]);
          if not candidate = false then
            return RZMSIsoByTriple(R1, R2, [l, g, candidate]);
          fi;
        od;
      od;
    od;
    return fail;
  end);
fi;

#

InstallGlobalFunction(RMSIsoByTriple,
function(R1, R2, triple)
  local fam, out;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(R1)),
   ElementsFamily(FamilyObj(R2)));
  out := Objectify(NewType(fam, IsRMSIsoByTriple), rec(triple := triple));
  SetSource(out, R1);
  SetRange(out, R2);
  #IsOne(out);

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

  if 1 <= i and i <= 3 then
    return x!.triple[i];
  fi;
  Error("Semigroups: ELM_LIST: usage,\n",
        "the index <i> must be at most 3,");
  return;
end);

#

InstallMethod(ELM_LIST, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple, IsPosInt],
function(x, i)

  if 1 <= i and i <= 3 then
    return x!.triple[i];
  fi;
  Error("Semigroups: ELM_LIST: usage,\n",
        "the index <i> must be at most 3,");
  return;
end);

#

InstallMethod(\=, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj,
[IsRMSIsoByTriple, IsRMSIsoByTriple],
function(triple1, triple2)

  if triple1!.triple[1] = triple2!.triple[1]
    and triple1!.triple[2] = triple2!.triple[2]
    and triple1!.triple[3] = triple2!.triple[3] then
    return true;
  fi;
  return OnTuples(GeneratorsOfSemigroup(Source(triple1)),
   triple1) = OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
end);

#

InstallMethod(\=, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj,
[IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(triple1, triple2)

  if triple1!.triple[1] = triple2!.triple[1]
    and triple1!.triple[2] = triple2!.triple[2]
    and triple1!.triple[3] = triple2!.triple[3] then
    return true;
  fi;
  return OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple1)
   = OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
end);

#

InstallMethod(\<, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj,
[IsRMSIsoByTriple, IsRMSIsoByTriple],
function(triple1, triple2)

  return (triple1!.triple[1] < triple2!.triple[1]) or
   (triple1!.triple[1] = triple2!.triple[1] and
    triple1!.triple[2] < triple2!.triple[2]) or
   (triple1!.triple[1] = triple2!.triple[1] and
    triple1!.triple[2] = triple2!.triple[2] and
    triple1!.triple[3] < triple2!.triple[3]);
end);

#

InstallMethod(\<, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj,
[IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(triple1, triple2)

  return (triple1!.triple[1] < triple2!.triple[1]) or
   (triple1!.triple[1] = triple2!.triple[1] and
    triple1!.triple[2] < triple2!.triple[2]) or
   (triple1!.triple[1] = triple2!.triple[1] and
    triple1!.triple[2] = triple2!.triple[2] and
    triple1!.triple[3] < triple2!.triple[3]);
end);

#

InstallMethod(CompositionMapping2, "for objects in `IsRMSIsoByTriple'",
IsIdenticalObj, [IsRMSIsoByTriple, IsRMSIsoByTriple],
function(a1, a2)
  local n, l1, l2, g1, g2, f1, f2;

  n := Length(Rows(Source(a1))) + Length(Columns(Source(a1)));

  l1 := a1!.triple[1];
  l2 := a2!.triple[1];
  g1 := a1!.triple[2];
  g2 := a2!.triple[2];
  f1 := a1!.triple[3];
  f2 := a2!.triple[3];

  return RMSIsoByTriple(Source(a1), Range(a2), [l1 * l2, g1 * g2, List([1 .. n],
   x -> f2[x ^ l1] * f1[x] ^ g2)]);
end);

#

InstallMethod(CompositionMapping2, "for objects in `IsRZMSIsoByTriple'",
IsIdenticalObj, [IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(a1, a2)
  local n, l1, l2, g1, g2, f1, f2;

  n := Length(Rows(Source(a1))) + Length(Columns(Source(a1)));

  l1 := a1!.triple[1];
  l2 := a2!.triple[1];
  g1 := a1!.triple[2];
  g2 := a2!.triple[2];
  f1 := a1!.triple[3];
  f2 := a2!.triple[3];

  return RZMSIsoByTriple(Source(a1), Range(a2), [l1 * l2, g1 * g2,
   List([1 .. n], x -> f2[x ^ l1] * f1[x] ^ g2)]);
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
function( triple, x)
  local g, i, j, lambda, gamma, f, m;

  m := Length(Rows(Source(triple)));

  i := x[1];
  g := x[2];
  j := x[3] + m;
  lambda := triple[1];
  gamma := triple[2];
  f := triple[3];
  return RMSElement(Range(triple), i ^ lambda, f[i] * ImageElm(gamma,g) / f[j],
   j ^ lambda - m);
end);

#

InstallMethod(ImagesRepresentative,
"for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(triple, x)
  local g, i, j, lambda, gamma, f, m;

  if not x = MultiplicativeZero(Source(triple)) then
    m := Length(Rows(Source(triple)));

    i := x[1];
    g := x[2];
    j := x[3] + m;
    lambda := triple[1];
    gamma := triple[2];
    f := triple[3];

    if f[i] = 0 or f[j] = 0 then
      return MultiplicativeZero(Source(triple));
    else
      return RMSElement(Range(triple),
      i ^ lambda, f[i] * ImageElm(gamma,g) / f[j], j ^ lambda - m);
    fi;
  else
    return x;
  fi;
end);


#

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'",
[IsEndoGeneralMapping and IsRMSIsoByTriple],
function(a)
  local l, g, f, n;

  n := Length(Rows(Source(a))) + Length(Columns(Source(a)));

  l := a[1];
  g := a[2];
  f := a[3];

  return RMSIsoByTriple(Range(a), Source(a), [l ^ - 1, g ^ - 1,
   List([1 .. n], x -> (f[x ^ l] ^ (g ^ - 1)) ^ - 1)]);
end);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'",
[IsEndoGeneralMapping and IsRMSIsoByTriple and IsOne], x -> x);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(a)
  local l, g, f, n;

  n := Length(Rows(Source(a))) + Length(Columns((Source(a))));

  l := a[1];
  g := a[2];
  f := a[3];

  return RZMSIsoByTriple(Range(a), Source(a), [l ^ - 1, g ^ - 1, List([1 .. n],
   x -> (f[x ^ l] ^ (g ^ - 1)) ^ - 1)]);
end);

#

InstallMethod(IsOne, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple], 99, #JDM why the 99?
function(triple)
  return IsOne(triple[1]) and IsOne(triple[2]) and
   ForAll(triple[3], IsOne);
end);

#

InstallMethod(IsOne, "for objects in `IsRZMSIsoByTriple'",
[IsEndoGeneralMapping and IsRZMSIsoByTriple],
function(triple)
  return IsOne(triple[1]) and IsOne(triple[2]) and
   ForAll(triple[3], IsOne);
end);

#

InstallMethod(PreImagesRepresentative,
"for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function(triple, x)
  return ImagesRepresentative(triple ^ - 1, x);
end);

#

InstallMethod(PreImagesRepresentative,
"for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(triple, x)
  return ImagesRepresentative(triple ^ - 1, x);
end);

#

InstallMethod(PrintObj, "for object in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple],
function( obj )
  Print( "RMSIsoByTriple ( ", Source(obj), ",", Range(obj), "," , obj[1], " ",
    obj[2], " ",  obj[3], " )" );
  return;
end);

#

InstallMethod(PrintObj, "for object in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function( obj )
  Print( "RZMSIsoByTriple ( ", Source(obj) , ",", Range(obj), "," , obj[1], " ",
    obj[2], " ",  obj[3], " )" );
  return;
end);

#

InstallMethod( ViewObj, "for object in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple],
function( obj )
  Print( "(", obj[1],", ",  obj[2],", ",  obj[3], ")" );
end );

#

InstallMethod( ViewObj, "for object in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(obj)
  Print( "(", obj[1], ", ", obj[2], ", ", obj[3], ")" );
  return;
end);

#EOF
