#############################################################################
##
#W  rms-translat.gi
#Y  Copyright (C) 2016-17                                         Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
## This file contains special methods for translations semigroups and 
## translational hulls of completely 0-simple seimgroups.
## 
## These methods are based on the constructions given in 
## Petrich, M. (1968) 
## ‘The translational hull of a completely 0-simple semigroup’,
## Glasgow Mathematical Journal, 9(01), p. 1. 
## doi: 10.1017/s0017089500000239
##
#############################################################################

# For RZMS, don't calculate AsList when LeftTranslations is called
# Just get generators
InstallMethod(LeftTranslations, "for a RZMS semigroup",
[IsSemigroup and IsFinite and IsZeroSimpleSemigroup],
function(S) 
  local L;
  
  L := LeftTranslationsSemigroup(S);
  GeneratorsOfSemigroup(L);
  
  return L;
end);

# For RZMS, don't calculate AsList when RightTranslations is called
# Just get generators
InstallMethod(RightTranslations, "for a RZMS semigroup",
[IsSemigroup and IsFinite and IsZeroSimpleSemigroup],
function(S) 
  local R;
  
  R := RightTranslationsSemigroup(S);
  GeneratorsOfSemigroup(R);
  
  return R;
end);

# The generators are generators of partial transformation monoid to act on the
# index sets, together with functions to the generators of the group.
InstallMethod(GeneratorsOfSemigroup, "for the semigroup of left/right translations of a finite 0-simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S, L, n, iso, inv, reesMatSemi, semiList, gens, t, f, groupGens,
        e, a, fa, G, zero;
        
  S := UnderlyingSemigroup(T);
  if not (IsZeroSimpleSemigroup(S) and IsFinite(S)) then
    TryNextMethod();
  fi;
  
  semiList := AsList(S);
  iso := IsomorphismReesZeroMatrixSemigroup(S);
  inv := InverseGeneralMapping(iso);
  reesMatSemi := Range(iso);
  zero := MultiplicativeZero(reesMatSemi);
  L := IsLeftTranslationsSemigroup(T);
  if L then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;
  
  gens := [];
  G := UnderlyingSemigroup(reesMatSemi);
  groupGens := GeneratorsOfGroup(G);
  
  for t in GeneratorsOfMonoid(PartialTransformationMonoid(n)) do
    if L then
      f := function(x)
        if (x = zero or x[1]^t = n+1) then 
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1]^t, 
            x[2], x[3]);
      end;
      Add(gens, LeftTranslation(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else
      f := function(x)
        if (x = zero or x[3]^t = n+1) then 
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1], 
          x[2], x[3]^t); 
      end;
      Add(gens, RightTranslation(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;
  
  for a in groupGens do
    fa := function(x)
      if x = 1 then
        return a;
      fi; 
      return MultiplicativeNeutralElement(G);
    end;
    if L then
      f := function(x)
        if x = zero then 
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1], 
            fa(x[1])*x[2], x[3]);
      end;
      Add(gens, LeftTranslationNC(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else
      f := function(x)
        if x = zero then 
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1], 
          x[2]*fa(x[3]), x[3]); 
      end;
      Add(gens, RightTranslationNC(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;
  return gens;
end);

InstallMethod(Size, "for the semigroup of left or right translations of a completely 0-simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily], 1,
function(T)
  local S, G, reesMatSemi, n;
  S := UnderlyingSemigroup(T);
  if not (IsZeroSimpleSemigroup(S) and IsFinite(S)) then
    TryNextMethod();
  fi;
  reesMatSemi := Range(IsomorphismReesZeroMatrixSemigroup(S));
  G := UnderlyingSemigroup(reesMatSemi);
  if IsLeftTranslationsSemigroup(T) then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;
  return (n * Size(G) + 1)^n;
end);

# Finds the transformations on the indices of a finite 0-simple semigroup
# which are candidates for translations, when combined with a function from
# the index sets to the group.
# TODO: swap rows/columns if more convenient.
SEMIGROUPS.FindTranslationTransformations := function(S)
  local iso, reesMatSemi, mat, simpleRows, simpleColumns, nrRows, nrCols,
    transList, partColumns, partColumn, pc, linkedTransList, col, cols, 
    possibleCols, t, q, w, c, x, k, v, f, i, j,
    isPartialSuccess, extend, reject, bt;
  iso := IsomorphismReesZeroMatrixSemigroup(S);
  reesMatSemi := Range(iso);
  mat := Matrix(reesMatSemi);
  simpleRows := List(mat, ShallowCopy);
  nrRows := Length(simpleRows);
  nrCols := Length(simpleRows[1]);
  transList := [];
  for i in [1..Length(simpleRows)] do
    for j in [1..Length(simpleRows[i])] do
      if simpleRows[i][j] <> 0 then
        simpleRows[i][j] := 1;
      fi;
    od;
  od;
  #TODO: just use transpose
  simpleColumns := [];
  for i in [1..Length(simpleRows[1])] do
    c := [];
    for j in [1..Length(simpleRows)] do
      c[j] := simpleRows[j][i];
    od;
    Add(simpleColumns, c);
  od;
  
  #TODO: keep track of the partial columns in the extend/reject functions
  isPartialSuccess := function(x)
    partColumns := [];
    for i in [1..Length(simpleRows[1])] do
      partColumn := [];
      for j in [1..Length(x)] do
        if x[j] = nrRows + 1 then
          #treat rows not in the domain like a row of zeros
          partColumn[j] := 0;
        else
          partColumn[j] := simpleRows[x[j]][i];
        fi;
      od;
      Add(partColumns, partColumn);
    od;
    
    for pc in partColumns do
      #Only check the columns which contain a 1 since
      #all-zero column can be made by column not in domain of transformation.
      #No ones in one partial matrix but not in the other...
      if 1 in pc and ForAll(simpleColumns, c -> 1 in c{[1..Length(pc)]} + pc) then
        return false;
      fi;
    od;
    return true;
  end;
  
  extend := function(w)
    Add(w, 1);
    return w;
  end;
  
  reject := function(q)
    q := ShallowCopy(q);
    k := Length(q);
    if q[k] <= nrRows then
      q[k] := q[k] + 1;
    elif k > 1 then
      q := reject(q{[1 .. k - 1]});
    else return 0;
    fi;
    return q;
  end;
  
  bt := function(x)
    if x = 0 then
      return 0;
    fi;
    if not isPartialSuccess(x) then
      x := reject(x);  
    elif Length(x) = nrRows then
      return x;
    else 
      x := extend(x);
    fi;
    return bt(x);
  end;
  
  v := 1;
  transList := [bt([1])];
  while transList[v] <> 0 do
    v := v + 1;
    transList[v] := bt(reject(transList[v-1]));
  od;
  
  linkedTransList := [];
  # Given each row transformation, look for matching column transformations.
  # Last element of transList will be 0, ignore.
  for k in [1 .. Length(transList) - 1] do
    t := transList[k];
    cols := [];
    for i in [1 .. Length(simpleRows[1])] do
      col := [];
      for j in [1 .. nrRows] do
        if t[j] = nrRows + 1 then
          col[j] := 0;
        else 
          col[j] := simpleRows[t[j]][i];
        fi;
      od;
      Add(cols, col);
    od;
    possibleCols := [];
    for i in [1..nrCols] do
      possibleCols[i] := [];
      for j in [1..nrCols] do  
        if not 1 in cols[i] + simpleColumns[j] then
          Add(possibleCols[i], j);
        fi;
        if not 1 in cols[i] then 
          Add(possibleCols[i], nrCols+1);
        fi;
      od;
    od;
    linkedTransList[k] := IteratorOfCartesianProduct(possibleCols);
  od;
  
  return [transList{[1 .. Length(transList) - 1]}, linkedTransList];
end;

# For a pair of transformations on the indices of a completely 0-simple semigroup
# determine the functions to the group associated with the RMS representation
# which will form translations when combined with the transformations.
# Connected components here means points in the domains which are related
# through the linked pair condition given by Petrich.
SEMIGROUPS.FindTranslationFunctionsToGroup := function(S, t1, t2, failedcomponents)
  local reesmatsemi, mat, gplist, gpsize, nrrows, nrcols, invmat,
        rowtransformedmat, zerorow, zerocol, pos, satisfied,
        rels, edges, rowrels, relpoints, rel, i, j, k, l, x, y, r, n, q, c,
        funcs, digraph, cc, comp, iterator, foundfuncs, failedcomps, vals,
        failedtocomplete, relpointvals,
        relssatisfied, funcsfromrelpointvals, fillin;
  reesmatsemi := Range(IsomorphismReesZeroMatrixSemigroup(S));
  mat := Matrix(reesmatsemi);
  gplist := AsList(UnderlyingSemigroup(reesmatsemi));
  gpsize := Size(gplist);
  nrrows := Length(mat);
  nrcols := Length(mat[1]);
  invmat := List(mat, ShallowCopy);
  for i in [1..nrrows] do
    for j in [1..nrcols] do
      invmat[i][j] := Inverse(invmat[i][j]);
    od;
  od; 
  rowtransformedmat := [];
  zerorow := [];
  zerocol := [];
  for i in [1..nrcols] do
    zerorow[i] := 0;
  od;
  for i in [1..nrrows] do
    zerocol[i] := 0;
  od;
  for i in [1..nrrows] do
    if t1[i] <> nrrows + 1 then 
      rowtransformedmat[i] := mat[t1[i]];
    else
      rowtransformedmat[i] := zerorow;
    fi;
  od;

  #for ease of checking the constraints, set up lists of linked indices
  rels := [];
  for i in [1..nrrows] do
    for j in [1..nrcols] do
      if rowtransformedmat[i][j] <> 0 then
        Add(rels, [i,j]);
      fi;
    od;
  od;
  rowrels := [];
  for i in [1..nrrows] do
    rowrels[i] := [];
    for j in [1..nrrows] do
      for k in [1..nrcols] do
        if [i,k] in rels and [j,k] in rels then
          rowrels[i][j] := k;
          break;
        fi;
      od;
    od;
  od;
  
  #get connected components
  edges := List(rels, r -> [r[1], r[2] + nrrows]);
  digraph := DigraphByEdges(edges, nrrows + nrcols);
  cc := DigraphConnectedComponents(digraph);
  
  #only deal with enough elements to hit each relation
  relpoints := [];
  for i in cc.comps do
    if Length(i) > 1 then 
      Add(relpoints, i[1]);
    fi;
  od;
  
  # Check whether these connected components have already been found to fail
  # (on the same values).
  for i in [1..Length(failedcomponents)] do
    for j in [1..Length(failedcomponents[i][1])] do
      c := failedcomponents[i][1][j];
      if ForAny(cc.comps, comp -> IsSubset(comp, c)) then
        vals := [[],[]];
        for k in c do
          if k <= nrrows then
            Add(vals[1], k);
          else
            Add(vals[2], k - nrrows);
          fi;
        od;
        if t1{vals[1]} = failedcomponents[i][2]{vals[1]} 
          and t2{vals[2]} = failedcomponents[i][3]{vals[2]} then
          return [];
        fi;
      fi;
    od;
  od;
  
  #given a choice of relpoints, fill in everything else possible
  fillin := function(funcs)
    x := ShallowCopy(funcs[1]);
    y := ShallowCopy(funcs[2]);
    for r in relpoints do
      comp := cc.comps[cc.id[r]];
      failedtocomplete := false;
      for n in [2..Length(comp)] do
        j := comp[n];
        if j <= nrrows then
          for q in comp do
            if IsBound(rowrels[q][j]) and not IsBound(x[j]) then
              k := rowrels[q][j];
              x[j] := mat[j][t2[k]] * invmat[q][t2[k]] * x[q] *
                      rowtransformedmat[q][k] * invmat[t1[j]][k];
              break;
            fi;
          od;
        if not IsBound(x[j]) then
          failedtocomplete := true;
        fi;
        else
          j := j - nrrows;
          if not IsBound(y[j]) then
            for rel in rels do
              if rel[1] in comp and rel[2] = j then
                i := rel[1];
                y[j] := invmat[i][t2[j]] * x[i] * rowtransformedmat[i][j];
                break;
              fi;
            od;
          fi;
        fi;
      od;
      # Might need several passes to fill in everything, since the filling in
      # propagates through the connected components.
      if failedtocomplete then
        funcs := fillin([x,y]);
        x := funcs[1];
        y := funcs[2];
      fi;
    od;
    return [x,y];
  end;
  
  relssatisfied := function(funcs)
    failedcomps := [];
    satisfied := true;
    x := funcs[1];
    y := funcs[2];
    for rel in rels do
      i := rel[1];
      j := rel[2];
      if IsBound(x[i]) and IsBound(y[j]) and not 
          x[i] * rowtransformedmat[i][j] = mat[i][t2[j]] * y[j] then
          Add(failedcomps, cc.comps[cc.id[i]]);
          satisfied := false;
      fi;
    od;
    return satisfied;
  end;
  
  funcsfromrelpointvals := function(relpointvals)
    x := [1..nrrows];
    y := [1..nrcols];
    
    for i in [1..nrrows] do
      Unbind(x[i]);
    od;
    for i in [1..nrcols] do
      Unbind(y[i]);
    od;
    for i in [1..Length(relpoints)] do
      x[relpoints[i]] := gplist[relpointvals[i]];
    od;
    return fillin([x,y]);
  end;
  
  foundfuncs := [];
  iterator := IteratorOfCartesianProduct(List(relpoints, i -> [1..gpsize]));
  
  # If S is a commutative group and fails on one choice of relpointvals,
  # it will fail on all of them.
  if IsDoneIterator(iterator) then
    return [];
  else
    funcs := funcsfromrelpointvals(NextIterator(iterator));
    if not relssatisfied(funcs) then
      if not IsCommutative(S) then
        return [0, Set(failedcomps), t1, t2];
      fi;
    else
      Add(foundfuncs, funcs);
    fi;
  fi;
  
  while not IsDoneIterator(iterator) do
    funcs := funcsfromrelpointvals(NextIterator(iterator));
    if relssatisfied(funcs) then
      Add(foundfuncs, funcs);
    fi;
  od;
  
  return foundfuncs;
end;

# Combine the previous methods to form the translational hull
# Performance suffers greatly as the size of the group increases.
SEMIGROUPS.TranslationalHullElementsOfZeroSimple := function(H)
  local S, tt, failedcomponents, iterator, transfuncs, unboundpositions, gplist,
        L, R, linkedpairs, i, j, c, linkedpairfromfuncs, iso, inv, nrrows,
        nrcols, reesmatsemi, zero, fl, fr, l, r, t1, t2, funcs, fx, fy,
        partialfunciterator, funcvals;
  
  S := UnderlyingSemigroup(H);
  if not (IsFinite(S) and IsZeroSimpleSemigroup(S)) then
    TryNextMethod();
  fi;
  iso := IsomorphismReesZeroMatrixSemigroup(S);
  inv := InverseGeneralMapping(iso);
  reesmatsemi := Range(iso);
  gplist := AsList(UnderlyingSemigroup(reesmatsemi));
  nrrows := Length(Matrix(reesmatsemi));
  nrcols := Length(Matrix(reesmatsemi)[1]);
  zero := MultiplicativeZero(reesmatsemi);
  L := LeftTranslationsSemigroup(S);
  R := RightTranslationsSemigroup(S);
  tt := SEMIGROUPS.FindTranslationTransformations(S);
  failedcomponents := [];
  linkedpairs := [];
  
  linkedpairfromfuncs := function(t1, t2, fx, fy)
    fl := function(x)
      if x = zero then
        return zero;
      fi;
      if t2[x[1]] <> nrcols + 1 then
        return RMSElement(reesmatsemi, t2[x[1]], fy[x[1]] * x[2], x[3]);
      else
        return zero;
      fi;
    end;
    
    fr := function(x)
      if x = zero then
        return zero;
      fi;
      if t1[x[3]] <> nrrows + 1 then
        return RMSElement(reesmatsemi, x[1], x[2] * fx[x[3]], t1[x[3]]);
      else
        return zero;
      fi;
    end;
    
    l := LeftTranslationNC(L, CompositionMapping(inv, MappingByFunction(
      reesmatsemi, reesmatsemi, fl), iso));
    
    r := RightTranslationNC(R, CompositionMapping(inv, MappingByFunction(
      reesmatsemi, reesmatsemi, fr), iso));
    
    return TranslationalHullElementNC(H, l, r);
  end;
  
  
  for i in [1..Length(tt[1])] do
    t1 := tt[1][i];
    iterator := tt[2][i];
    while not IsDoneIterator(iterator) do
      t2 := NextIterator(iterator);
      transfuncs := SEMIGROUPS.FindTranslationFunctionsToGroup(S, t1, t2, 
                                                              failedcomponents);
      if not IsEmpty(transfuncs) then
        if not transfuncs[1] = 0 then
          for funcs in transfuncs do
            fx := funcs[1];
            fy := funcs[2];
            unboundpositions := [];
            for j in [1..nrcols] do
              if not IsBound(fy[j]) then 
                Add(unboundpositions, j);
              fi;
            od;
            if Length(unboundpositions) > 0 then
              c := List([1..Length(unboundpositions)], i -> [1..Length(gplist)]);
              partialfunciterator := IteratorOfCartesianProduct(c);
              while not IsDoneIterator(partialfunciterator) do
                funcvals := NextIterator(partialfunciterator);
                for j in [1..Length(unboundpositions)] do
                  fy[unboundpositions[j]] := gplist[funcvals[j]];
                od;
                Add(linkedpairs, linkedpairfromfuncs(t1, t2, fx, fy));
              od;
            else
              Add(linkedpairs, linkedpairfromfuncs(t1, t2, fx, fy));
            fi;
          od;
        else
          Add(failedcomponents, transfuncs{[2,3,4]}); 
        fi;
      fi;
    od;
  od;
  
  return Set(linkedpairs);
end;
