#############################################################################
##
##  cartan.gi
##  Copyright (C) 2024                                   Balthazar Charles
##                                                             Joseph Ruiz
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This implementation of generalized conjugacy classes is very rundamentary
# and is practically unused to compute the character table or Cartan matrix.
# This object is to be a placeholder to eventually hold all the elements
# which are in the same generalized conjugacy class. This will then allow
# the monoid characters to work like characters in the case of groups.
BindGlobal("GeneralizedConjugacyClassType",
NewType(NewFamily("GeneralizedConjugacyClassFamily"),
        IsGeneralizedConjugacyClass and
        IsAttributeStoringRep));

InstallMethod(GeneralizedConjugacyClass,
              "for a semigroup and a multiplicative element",
[IsSemigroup, IsMultiplicativeElement],
function(S, s)
  local result;
  result := Objectify(GeneralizedConjugacyClassType, rec());
  SetRepresentative(result, s);
  SetParentAttr(result, S);
  return result;
end);

InstallMethod(ViewString, "for a generalized conjugacy class",
[IsGeneralizedConjugacyClass],
function(generalizedconjugacyclass)
  return StringFormatted(
      "<generalized conjugacy class in {} for representative {}>",
      ParentAttr(generalizedconjugacyclass),
      Representative(generalizedconjugacyclass));
end);

InstallMethod(DisplayString, "for a generalized conjugacy class",
[IsGeneralizedConjugacyClass],
ViewString);

InstallMethod(GeneralizedConjugacyClassesRepresentatives, "for a semigroup",
[IsSemigroup],
function(S)
  local D, out, C, map, invmap;

  D := List(RegularDClasses(S), GroupHClass);
  D := List(D, IsomorphismPermGroup);
  out := [];
  for map in D do
    C := List(ConjugacyClasses(OrdinaryCharacterTable(Range(map))),
                               Representative);
    # Ugly fix: ensures that the conjugacy classes are computed
    # in the same order each time.
    invmap := InverseGeneralMapping(map);
    C := List(C, x -> x ^ invmap);
    Append(out, C);
  od;

  return out;
end);

InstallMethod(GeneralizedConjugacyClasses, "for a semigroup",
[IsSemigroup],
function(S)
  return List(GeneralizedConjugacyClassesRepresentatives(S),
                 x -> GeneralizedConjugacyClass(S, x));
end);

BindGlobal("MonoidCharacterTableType",
NewType(NewFamily("MonoidCharacterTableFamily"),
        IsMonoidCharacterTable and
        IsAttributeStoringRep));

InstallMethod(MonoidCharacterTable,  "for a semigroup",
[IsMonoidAsSemigroup],
function(S)
  local result;

  result := Objectify(MonoidCharacterTableType, rec());
  SetParentAttr(result, S);

  return result;
end);

InstallMethod(ViewString, "for a monoid character table",
[IsMonoidCharacterTable],
function(ct)
  return StringFormatted("MonoidCharacterTable( {} )",
  ParentAttr(ct));
end);

# Notes to consider when changing the code for the display string
# for IsMonoidCharacterTable.
#
# The following conventions were observed in the character tables of
# groups.
# Integer entries are never truncated and make their column bigger
# -/A prefix makes a column bigger
# Checking for redunacnies under *M is not implemented. However
# character tables of groups do check for *M redundancies.
# Column headers do not get padded to match wider columns.

InstallMethod(DisplayString, "for a monoid character table",
[IsMonoidCharacterTable],
function(ct)
  local str, columnlabels, rowlabels, strarray, sizetable, i, j, ctmatrix,
  rosetastone, coltable, columnwidth, rowlabelwidth, currentwidth, currentpage,
  screensizeassume, quotientcolumnwidthsums, temp, temp2, temp3, temp4;

  str := StringFormatted("MonoidCharacterTable( {} )",
  ParentAttr(ct));

  if HasIrr(ct) then
    sizetable := Length(Irr(ct));

    strarray := List([1 .. sizetable], x -> List([1 .. sizetable], y -> "."));
    ctmatrix := List(Irr(ct), ValuesOfMonoidClassFunction);
    rosetastone := Filtered(Unique(Concatenation(ctmatrix)),
                                   x -> not IsInt(x));

    columnlabels := List([1 .. 2], x -> List([1 .. sizetable], y -> " "));
    rowlabels := List([1 .. (sizetable + 2)], x -> " ");

    for i in [1 .. sizetable] do
      rowlabels[i + 2] := Concatenation("X.", String(i));
    od;

    for j in [1 .. sizetable] do
      columnlabels[1, j] := Concatenation("c.", String(j));
    od;

    for j in [1 .. sizetable] do
      columnlabels[2, j] := " ";
    od;

    for i in [1 .. sizetable] do
      for j in [1 .. sizetable] do
        if IsInt(ctmatrix[i, j]) then
          if not IsZero(ctmatrix[i, j]) then
            strarray[i, j] := String(ctmatrix[i, j]);
          fi;
        else
          strarray[i, j] := WordAlp("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                    Position(rosetastone, ctmatrix[i, j]));
        fi;
      od;
    od;

    coltable := Concatenation(columnlabels, strarray);

    columnwidth := List(List(TransposedMat(coltable),
                        x -> List(x, Length)), Maximum) + 1;

    rowlabelwidth := Maximum(List(rowlabels, Length));

    for i in [1 .. Length(rowlabels)] do

      rowlabels[i] := Concatenation(rowlabels[i],
                            WordAlp(" ", rowlabelwidth - Length(rowlabels[i])));
    od;

    for i in [1 .. Length(coltable)] do
      for j in [1 .. sizetable] do
        coltable[i, j] := Concatenation(WordAlp(" ",
                                       columnwidth[j] - Length(coltable[i, j])),
                                       coltable[i, j]);
      od;
    od;

    screensizeassume := Maximum(SizeScreen()[1], 20) - rowlabelwidth;
    currentwidth := 0;
    currentpage := 0;
    quotientcolumnwidthsums := List(columnwidth, x -> 0);
    for i in [1 .. sizetable] do
      currentwidth := currentwidth + columnwidth[i];
      if currentwidth + 1 < screensizeassume then
        quotientcolumnwidthsums[i] := currentpage;
      else
        currentwidth := columnwidth[i];
        currentpage := currentpage + 1;
        quotientcolumnwidthsums[i] := currentpage;
      fi;
    od;

    temp := Concatenation(List([0 .. Last(quotientcolumnwidthsums)],
    k -> List(coltable,
    x -> Concatenation(x{Positions(quotientcolumnwidthsums, k)}))));

    temp2 := List(temp, x -> Concatenation(x, "\n"));

    temp3 := Concatenation(List([1 .. Length(temp2)],
           x -> Concatenation(rowlabels[((x - 1) mod Length(rowlabels)) + 1],
                              temp2[x])));

    temp4 := List([1 .. Length(rosetastone)],
                  x -> Concatenation(WordAlp("ABCDEFGHIJKLMNOPQRSTUVWXYZ", x),
                                     " := ", String(rosetastone[x]), "\n"));

    str := Concatenation(temp3, "\n", Concatenation(temp4));

  fi;

  return str;
end);

BindGlobal("MonoidCartanMatrixType",
NewType(NewFamily("MonoidCartanMatrixFamily"),
        IsMonoidCartanMatrix and
        IsAttributeStoringRep));

InstallMethod(MonoidCartanMatrix,  "for a semigroup",
[IsMonoidAsSemigroup],
function(S)
  local result;

  result := Objectify(MonoidCartanMatrixType, rec());
  SetParentAttr(result, S);

  return result;
end);

InstallMethod(ViewString, "for a monoid cartan matrix",
[IsMonoidCartanMatrix],
function(cm)
  return StringFormatted("MonoidCartanMatrix( {} )",
  ParentAttr(cm));
end);

InstallMethod(DisplayString, "for a monoid cartan matrix",
[IsMonoidCartanMatrix],
function(cm)
  local str, columnlabels, rowlabels, strarray, sizetable, i, j, cmmatrix,
  coltable, columnwidth, rowlabelwidth, currentwidth, currentpage,
  screensizeassume, quotientcolumnwidthsums, temp, temp2;

  str := StringFormatted("MonoidCartanMatrix( {} )",
  ParentAttr(cm));

  if HasPims(cm) then
    sizetable := Length(Pims(cm));

    strarray := List([1 .. sizetable], x -> List([1 .. sizetable], y -> "."));
    cmmatrix := List(Pims(cm), ValuesOfCompositionFactorsFunction);

    columnlabels := List([1 .. 2], x -> List([1 .. sizetable], y -> " "));
    rowlabels := List([1 .. (sizetable + 2)], x -> " ");

    for i in [1 .. sizetable] do
      rowlabels[i + 2] := Concatenation("P.", String(i));
    od;

    for j in [1 .. sizetable] do
      columnlabels[1, j] := Concatenation("X.", String(j));
    od;

    for j in [1 .. sizetable] do
      columnlabels[2, j] := " ";
    od;

    for i in [1 .. sizetable] do
      for j in [1 .. sizetable] do
        if not IsZero(cmmatrix[i, j]) then
          strarray[i, j] := String(cmmatrix[i, j]);
        fi;
      od;
    od;

    coltable := Concatenation(columnlabels, strarray);

    columnwidth := List(List(TransposedMat(coltable),
                             x -> List(x, Length)), Maximum) + 1;

    rowlabelwidth := Maximum(List(rowlabels, Length));

    for i in [1 .. Length(rowlabels)] do
      rowlabels[i] := Concatenation(rowlabels[i],
                            WordAlp(" ", rowlabelwidth - Length(rowlabels[i])));
    od;

    for i in [1 .. Length(coltable)] do
      for j in [1 .. sizetable] do
        coltable[i, j] := Concatenation(WordAlp(" ",
                                       columnwidth[j] - Length(coltable[i, j])),
                                       coltable[i, j]);
      od;
    od;

    screensizeassume := Maximum(SizeScreen()[1], 20) - rowlabelwidth;
    currentwidth := 0;
    currentpage := 0;
    quotientcolumnwidthsums := List(columnwidth, x -> 0);
    for i in [1 .. sizetable] do
      currentwidth := currentwidth + columnwidth[i];
      if currentwidth + 1 < screensizeassume then
        quotientcolumnwidthsums[i] := currentpage;
      else
        currentwidth := columnwidth[i];
        currentpage := currentpage + 1;
        quotientcolumnwidthsums[i] := currentpage;
      fi;
    od;

    temp := Concatenation(List([0 .. Last(quotientcolumnwidthsums)],
    k -> List(coltable,
    x -> Concatenation(x{Positions(quotientcolumnwidthsums, k)}))));

    temp2 := List(temp, x -> Concatenation(x, "\n"));

    str := Concatenation(List([1 .. Length(temp2)],
           x -> Concatenation(rowlabels[((x - 1) mod Length(rowlabels)) + 1],
                              temp2[x])));

  fi;

  return str;
end);

BindGlobal("MonoidCharacterType",
NewType(NewFamily("MonoidCharacterFamily"),
        IsMonoidCharacter and
        IsAttributeStoringRep));

InstallMethod(MonoidCharacter,  "for a monoid character table and dense list",
[IsMonoidCharacterTable, IsDenseList],
function(ct, values)
  local result;

  result := Objectify(MonoidCharacterType, rec());
  SetParentAttr(result, ct);
  SetValuesOfMonoidClassFunction(result, values);

  return result;
end);

InstallMethod(ViewString, "for a monoid character",
[IsMonoidCharacter],
function(char)
  local str;
  if HasValuesOfMonoidClassFunction(char) then
    str := StringFormatted("MonoidCharacter( {} , {} )",
           ViewString(ParentAttr(char)),
           ValuesOfMonoidClassFunction(char));
  elif HasProjectiveCoverOf(char) then
    str := StringFormatted("MonoidCharacter( {} , Projective Cover Of {} )",
           ViewString(ParentAttr(char)),
           ViewString(ProjectiveCoverOf(char)));
  fi;

  return str;
end);

InstallMethod(DClassBicharacter, "for a D-class",
[IsGreensDClass],
function(D)
  local S, C, cS, M, i, j;

  S   := ParentAttr(D);
  C   := GeneralizedConjugacyClassesRepresentatives(S);
  cS  := Length(C);
  M   := List([1 .. cS], x -> List([1 .. cS], y -> 0));

  for i in [1 .. cS] do
    for j in [1 .. cS] do
      M[i][j] := Number(D, d -> C[i] * d * C[j] = d);
    od;
  od;

  return M;
end);

InstallMethod(DClassBicharacter, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local S, C, G, cardG, CG, cG, cS, d,
        l_mults, lp_mults, l, lp, r_mults, rp_mults, r, rp,
        LRec, RRec, h, k, i, j, g, pos, Diag;

  S   := ParentAttr(D);
  C   := GeneralizedConjugacyClassesRepresentatives(S);
  G   := SchutzenbergerGroup(D);
  cardG := Size(G);
  CG  := ConjugacyClasses(G);
  cG  := Length(CG);
  cS  := Length(C);

  d   := Representative(D);

  l_mults  := List(HClassReps(LClass(S, d)),
                   h -> LeftGreensMultiplierNC(S, d, h));
  lp_mults := List(HClassReps(LClass(S, d)),
                   h -> LeftGreensMultiplierNC(S, h, d));
  r_mults  := List(HClassReps(RClass(S, d)),
                   h -> RightGreensMultiplierNC(S, d, h));
  rp_mults := List(HClassReps(RClass(S, d)),
                   h -> RightGreensMultiplierNC(S, h, d));

  LRec := List([1 .. cS], x -> List([1 .. cG], y -> 0));

  for i in [1 .. cS] do
    h := C[i];
    for j in [1 .. Length(l_mults)] do
      l  := l_mults[j];
      lp := lp_mults[j];
      if h * l * d in RClass(S, l * d) then
        g := Inverse(LambdaPerm(S)(d, lp * h * l * d));
        pos := Position(CG, ConjugacyClass(G, g));
        LRec[i][pos] := LRec[i][pos] + 1;
      fi;
    od;
  od;

  RRec := List([1 .. cG], x -> List([1 .. cS], y -> 0));

  for i in [1 .. cS] do
    k := C[i];
    for j in [1 .. Length(r_mults)] do
      r  := r_mults[j];
      rp := rp_mults[j];
      if d * r * k in LClass(S, d * r) then
        g   := LambdaPerm(S)(d, d * r * k * rp);
        pos := Position(CG, ConjugacyClass(G, g));
        RRec[pos][i] := RRec[pos][i] + 1;
      fi;
    od;
  od;

  Diag := DiagonalMat(List(CG, x -> cardG / Size(x)));

  return LRec * Diag * RRec;
end);

# M[i][j] := Number(S, s -> C[i] * s * C[j] = s);

InstallMethod(RegularRepresentationBicharacter, "for a semigroup",
[IsSemigroup],
function(S)
  local C, D, c, mat;

  C := GeneralizedConjugacyClassesRepresentatives(S);
  c := Length(C);
  mat := List([1 .. c], x -> List([1 .. c], x -> 0));

  for D in DClasses(S) do
    mat := mat + DClassBicharacter(D);
  od;

  return mat;
end);

# M[i][j] := Number(RClass(S, e), r -> CG[i] * r * CS[j] = r);

InstallMethod(RClassBicharacterOfGroupHClass, "for group H-class",
[IsGroupHClass],
function(H)
  local S, e, CS, map, invmap, HH, r_mults, rp_mults,
        cS, CG, cG, M, CHH, CardCentralizer,
        i, j, k, r, rp, y, c;

  S   := ParentAttr(H);
  CS  := GeneralizedConjugacyClassesRepresentatives(S);
  e   := MultiplicativeNeutralElement(H);
  map := IsomorphismPermGroup(H);
  HH  := Range(map);

  cS   := Length(CS);
  CHH  := ConjugacyClasses(OrdinaryCharacterTable(HH));

  invmap := InverseGeneralMapping(map);

  CG   := List(List(CHH, Representative), x -> x ^ invmap);
  cG   := Length(CG);
  M    := List([1 .. cG], x -> List([1 .. cS], x -> 0));

  CardCentralizer := List(CG, c -> Size(Centralizer(HH, c ^ map)));

  r_mults  := List(HClassReps(RClassOfHClass(H)),
                  h -> RightGreensMultiplierNC(S, e, h));
  rp_mults := List(HClassReps(RClassOfHClass(H)),
                  h -> RightGreensMultiplierNC(S, h, e));

  for j in [1 .. cS] do
      for k in [1 .. Length(r_mults)] do
        r  := r_mults[k];
        rp := rp_mults[k];
        if e * r * CS[j] in HClass(S, e * r) then
          y := Inverse((e * r * CS[j] * rp) ^ map);
          c := ConjugacyClass(HH, y);
          i := Position(CHH, c);
          M[i][j] := M[i][j] + CardCentralizer[i];
        fi;
      od;
  od;

  return M;
end);

InstallMethod(RClassRadicalOfGroupHClass,  "for group H-class",
[IsGroupHClass],
function(H)
  local S, e, ord, HH, LHH, map,
        l_mults, r_mults, rp_mults, nl, nr,
        M, Rad, c, j, r, k, i, l, x;

  S   := ParentAttr(H);
  e   := MultiplicativeNeutralElement(H);
  ord := Size(H);
  map := IsomorphismPermGroup(H);
  HH  := Range(map);
  LHH := List(HH);

  l_mults  := List(HClassReps(LClassOfHClass(H)),
                   h -> LeftGreensMultiplierNC(S, e, h) * e);
  r_mults  := List(HClassReps(RClassOfHClass(H)),
                   h -> e * RightGreensMultiplierNC(S, e, h));
  rp_mults := List(HClassReps(RClassOfHClass(H)),
                   h -> RightGreensMultiplierNC(S, h, e) * e);
  nl := Length(l_mults);
  nr := Length(r_mults);

  M := List([1 .. ord * nl], x -> List([1 .. ord * nr], x -> 0));

  c := 0;
  for k in H do
    for i in [1 .. nl] do
      l := l_mults[i];
      for j in [1 .. nr] do
        r  := r_mults[j];
        if (r * l) in H then
          x := (k ^ map) * ((r * l) ^ map) ^ (-1);
          M[i + nl * c][(j - 1) * ord + Position(LHH, x)] := 1;
        fi;
      od;
    od;
    c := c + 1;
  od;

  Rad := NullspaceMat(TransposedMatMutable(M));

  return rec(rad := Rad,
             transitions := r_mults,
             returns := rp_mults,
             HList := LHH);
end);

# M[i][j] := Trace of action x -> CG[i] * x * CS[j];

InstallMethod(RClassRadicalBicharacterOfGroupHClass,  "for group H-class",
[IsGroupHClass],
function(H)
  local S, e, Rec, Rad, LHH, map, invmap, r_mults, rp_mults,
        ListLClass, n, HH, CHH, ord, B, dim,
        CS, cS, CG, cG, mat, compt,
        h, k, chi, ind_r, r, row, i, coeff,
        ind_transition, ind_groupe, x, lp, g, ind_l_class;

  S   := ParentAttr(H);
  e   := MultiplicativeNeutralElement(H);

  CS   := GeneralizedConjugacyClassesRepresentatives(S);
  cS   := Length(CS);

  map := IsomorphismPermGroup(H);
  invmap := InverseGeneralMapping(map);

  HH  := Range(map);
  CHH  := ConjugacyClasses(OrdinaryCharacterTable(HH));

  CG   := List(List(CHH, Representative), x -> x ^ invmap);
  cG   := Length(CG);

  Rec := RClassRadicalOfGroupHClass(H);
  Rad := Rec.rad;
  LHH := Rec.HList;
  r_mults  := Rec.transitions;
  rp_mults := Rec.returns;

  ListLClass := List(r_mults, r -> LClass(S, e * r));

  if Length(Rad) = 0 then
    Rad := [[0]];
  fi;
  n    := Length(Rad[1]);
  ord  := Length(LHH);
  B    := Basis(VectorSpace(Rationals, Rad));
  dim  := Length(B);

  mat  := List([1 .. cG], x -> List([1 .. cS], x -> 0));

  for h in CS do
    for k in CG do
      chi := 0;

      # Computing the contribution to the trace of each basis vector
      for ind_r in [1 .. dim] do
        r   := B[ind_r];
        row := List([1 .. n], x -> 0);
        compt := 0;
        # Computing the image of the vector
        for i in [1 .. n] do
          coeff := r[i];
          if coeff = 0 then continue;
          fi;
          ind_transition := QuoInt(i - 1, ord) + 1;
          ind_groupe := RemInt(i - 1, ord) + 1;
          x := k * (LHH[ind_groupe] ^ invmap) * r_mults[ind_transition] * h;
          ind_transition := Position(ListLClass, LClass(S, x));
          # Changed from not ind_transition = fail
          if ind_transition <> fail then
            compt := compt + 1;

            lp := rp_mults[ind_transition];

            g  := (e * x * lp) ^ map;
            ind_groupe  := Position(LHH, g);
            ind_l_class := (ind_transition - 1) * ord + ind_groupe;
            row[ind_l_class] := row[ind_l_class] + coeff;
          fi;
        od;
        chi := chi + Coefficients(B, row)[ind_r];
      od;
      mat[Position(CG, k)][Position(CS, h)] := chi;
    od;
  od;

  return mat;
end);

InstallMethod(BlockDiagonalMatrixOfCharacterTables,  "for a semigroup",
[IsSemigroup],
function(S)
  # Removed loval variable CS
  local transversalHclasses, maps, groups, charactertables,
      irrs, mats;

  # Removed following line of code as a part of linting.
  # The following line of code was run early to ensures that the
  # conjugacy classes were computed in the same order each time.
  # As I have learned more about the GAP language this step might be
  # unnecessary. Until I am sure, I will leave this line here with
  # this comment.
  # CS := GeneralizedConjugacyClassesRepresentatives(S);

  transversalHclasses := List(RegularDClasses(S), GroupHClass);
  maps := List(transversalHclasses, IsomorphismPermGroup);
  groups := List(maps, Range);
  charactertables := List(groups, CharacterTable);
  irrs := List(charactertables, Irr);
  mats := List(irrs, x -> List(x, ValuesOfClassFunction));

  return DirectSumMat(mats);
end);

InstallMethod(Irr,  "for a monoid character table",
[IsMonoidCharacterTable],
function(ct)
  local R, Rrad, D, transversalHclasses, irrvalues;

  D := BlockDiagonalMatrixOfCharacterTables(ParentAttr(ct));
  transversalHclasses := List(RegularDClasses(ParentAttr(ct)), GroupHClass);
  R := Concatenation(List(transversalHclasses, RClassBicharacterOfGroupHClass));
  Rrad := Concatenation(List(transversalHclasses,
                        RClassRadicalBicharacterOfGroupHClass));
  irrvalues := Inverse(TransposedMat(D)) * (R - Rrad);
  return List(irrvalues, x -> MonoidCharacter(ct, x));
end);

InstallMethod(PimMonoidCharacter,
"for a monoid character table, dense list, and monoid character",
[IsMonoidCharacterTable, IsDenseList, IsMonoidCharacter],
function(ct, values, char)
  local result;

  result := Objectify(MonoidCharacterType, rec());
  SetParentAttr(result, ct);
  SetValuesOfCompositionFactorsFunction(result, values);
  SetProjectiveCoverOf(result, char);

  return result;
end);

InstallMethod(Pims,  "for a monoid Cartan matrix",
[IsMonoidCartanMatrix],
function(cm)
  local C, S, ct, M, out;

  S := ParentAttr(cm);
  ct := MonoidCharacterTable(S);
  C := List(Irr(ct), ValuesOfMonoidClassFunction);
  M := RegularRepresentationBicharacter(S);
  out := Inverse(TransposedMatMutable(C)) * M * Inverse(C);

  return List([1 .. Length(out)],
                n -> PimMonoidCharacter(ct, out[n], Irr(ct)[n]));
end);
