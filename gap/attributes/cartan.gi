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




























InstallMethod(TransversalIdempotents, "for a semigroup",
[IsSemigroup],
function(S)
  local out;

  out := List(List(RegularDClasses(S), GroupHClass), MultiplicativeNeutralElement);

  SetTransversalIdempotents(S,out);

  return out;
end);
























BindGlobal("GeneralisedConjugacyClassType",
NewType(NewFamily("GeneralisedConjugacyClassFamily"),
        IsGeneralisedConjugacyClass and
        IsAttributeStoringRep));

InstallMethod(GeneralisedConjugacyClass, " ",
[IsSemigroup, IsObject],
function(S,s)
  local result;

  result := Objectify(GeneralisedConjugacyClassType, rec());
  SetRepresentative(result, s);
  SetParentAttr(result, S);

  return result;
end);

InstallMethod(ViewString, "for a Generalised Conjugacy Class",
[IsGeneralisedConjugacyClass],
function(generalizedconjugacyclass)
  return StringFormatted("<Generalised Conjugacy Class in {} for representative {}>",
  ParentAttr(generalizedconjugacyclass),
  Representative(generalizedconjugacyclass));
end);


























InstallMethod(GeneralisedConjugacyClassesRepresentatives, "for a semigroup",
[IsSemigroup],
function(S)
  local D, out, C, map, invmap;

  D := List(RegularDClasses(S), GroupHClass);
  D := List(D, IsomorphismPermGroup);
  out := [];
  for map in D do
    C := List(ConjugacyClasses(OrdinaryCharacterTable(Range(map))), Representative);
    # Ugly fix: ensures that the conjugacy classes are computed 
    # in the same order each time. Also ensures the conjugacy classes of the 
    # group and the charater table are in the same order.
    invmap := InverseGeneralMapping(map);
    C := List(C, x -> x ^ invmap);
    Append(out, C);
  od;

  SetGeneralisedConjugacyClassesRepresentatives(S, out);

  return out;
end);





















InstallMethod(GeneralisedConjugacyClasses, " ",
[IsSemigroup],
function(S)
  local result;

  result := List(GeneralisedConjugacyClassesRepresentatives(S),
                 x -> GeneralisedConjugacyClass(S,x));

  SetGeneralisedConjugacyClasses(S, result);

  return result;
end);












































BindGlobal("MonoidCharacterTableType",
NewType(NewFamily("MonoidCharacterTableFamily"),
        IsMonoidCharacterTable and
        IsAttributeStoringRep));

InstallMethod(MonoidCharacterTable,  "for a semigroup",
[IsSemigroup],
function(S)
  local result;

  result := Objectify(MonoidCharacterTableType, rec());
  SetParentAttr(result, S);
  SetMonoidCharacterTable(S, result);

  return result;
end);

InstallMethod(ViewString, "for a Monoid Character Table",
[IsMonoidCharacterTable],
function(ct)
  return StringFormatted("MonoidCharacterTable( {} )",
  ParentAttr(ct));
end);

















BindGlobal("MonoidCharacterType",
NewType(NewFamily("MonoidCharacterFamily"),
        IsMonoidCharacter and
        IsAttributeStoringRep));

InstallMethod(MonoidCharacter,  " ",
[IsMonoidCharacterTable, IsDenseList],
function(ct,values)
  local result;

  result := Objectify(MonoidCharacterType, rec());
  SetParentAttr(result, ct);
  SetValuesOfMonoidClassFunction(result, values);

  return result;
end);

InstallMethod(ViewString, "for a Monoid Character",
[IsMonoidCharacter],
function(char)
  local str;
  if HasValuesOfMonoidClassFunction(char) then
    str := StringFormatted("MonoidCharacter( {} , {} )",
           ViewString(ParentAttr(char)),
           ValuesOfMonoidClassFunction(char));
  elif HasProjectiveCoverOf( char ) then
    str := StringFormatted("MonoidCharacter( {} , Projective Cover Of {} )",
           ViewString(ParentAttr(char)),
           ViewString(ProjectiveCoverOf( char )));
  fi;

  return str;
end);

















InstallMethod(DClassBicharacter, "for a D class",
[IsGreensDClass],
function(D)
  local S, C, G, cardG, CG, cG, cS, d, 
        l_mults, lp_mults, l, lp, r_mults, rp_mults, r, rp,
        LRec, RRec, h, k, i, j, g, pos, Diag;

  S   := ParentAttr(D);
  C   := GeneralisedConjugacyClassesRepresentatives(S);
  G   := SchutzenbergerGroup(D);
  cardG := Size(G);
  CG  := ConjugacyClasses(G);
  cG  := Length(CG);
  cS  := Length(C);

  d   := Representative(D);

  l_mults  := List(HClassReps(LClass(S, d)), h -> LeftGreensMultiplierNC(S, d, h));
  lp_mults := List(HClassReps(LClass(S, d)), h -> LeftGreensMultiplierNC(S, h, d));
  r_mults  := List(HClassReps(RClass(S, d)), h -> RightGreensMultiplierNC(S, d, h));
  rp_mults := List(HClassReps(RClass(S, d)), h -> RightGreensMultiplierNC(S, h, d));

  LRec := List([1..cS], x -> List([1..cG], x -> 0));

  for i in [1 .. cS] do
    h := C[i];
    for j in [1..Length(l_mults)] do
      l  := l_mults[j];
      lp := lp_mults[j];
      if h * l * d in RClass(S, l * d) then 
        g := Inverse(LambdaPerm(S)(d, lp * h * l * d));
        pos := Position(CG, ConjugacyClass(G, g));
        LRec[i][pos] := LRec[i][pos] + 1;
      fi;
    od;
  od;

  RRec := List([1..cG], x -> List([1..cS], x -> 0));

  for i in [1..cS] do
    k := C[i];
    for j in [1..Length(r_mults)] do
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
  SetDClassBicharacter(D,LRec * Diag * RRec);

  return LRec * Diag * RRec;
end);

































InstallMethod(RegularRepresentationBicharacter, "for a semigroup",
[IsSemigroup],
function(S)
  local C, D, c, mat, i, j;

  C := GeneralisedConjugacyClassesRepresentatives(S);
  c := Length(C);
  mat := List([1 .. c], x -> List([1 .. c], x -> 0));

  for D in DClasses(S) do
    mat := mat + DClassBicharacter(D);
  od;

  SetRegularRepresentationBicharacter(S, mat);

  return mat;
end);



































InstallMethod(RClassBicharacterOfGroupHClass, "for group H class",
[IsGroupHClass],
function(H)
  local S, e, CS, map, invmap, HH, r_mults, rp_mults,
        cS, CG, cG, M, CHH, CardCentralizer,
        i, j, k, r, rp, y, c;

  S   := ParentAttr(H);
  CS  := GeneralisedConjugacyClassesRepresentatives(S);
  e   := MultiplicativeNeutralElement(H);
  map := IsomorphismPermGroup(H);
  HH  := Range(map);

  r_mults := List(HClassReps(RClassOfHClass(H)), h -> RightGreensMultiplierNC(S, e, h));
  rp_mults     := List(HClassReps(RClassOfHClass(H)), h -> RightGreensMultiplierNC(S, h, e));

  cS   := Length(CS);
  CHH  := ConjugacyClasses(OrdinaryCharacterTable(HH));

  invmap := InverseGeneralMapping(map);

  CG   := List(List(CHH, Representative) , x -> x ^ invmap);
  cG   := Length(CG);
  M    := List([1 .. cG], x -> List([1 .. cS], x -> 0));

  CardCentralizer := List(CG, c -> CentralizerOrder(HH, c^map));

  for j in [1 .. cS] do
      for k in [1 .. Length(r_mults)] do
        r  := r_mults[k];
        rp := rp_mults[k];
        if e * r * CS[j] in HClass(S, e * r) then
          y := Inverse((e * r * CS[j] * rp)^map);
          c := ConjugacyClass(HH, y);
          i := Position(CHH, c);
          M[i][j] := M[i][j] + CardCentralizer[i];
        fi;
      od;
  od;

  SetRClassBicharacterOfGroupHClass(H,M);

  return M;
end);
























# Could be renamed to the natural map.

InstallMethod(RClassRadicalOfGroupHClass,  "for group H class",
[IsGroupHClass],
function(H)
  local S, e, D, ord, map, HH, LHH, invmap, 
        l_mults, r_mults, rp_mults, nl, nr,
        M, Rad, c, j, r, k, i, l, x, out;

  S   := ParentAttr(H);
  e   := MultiplicativeNeutralElement(H);
  D   := DClassOfHClass(H);
  ord := Size(H);
  map := IsomorphismPermGroup(H);
  HH  := Range(map);
  LHH := List(HH);
  invmap := InverseGeneralMapping(map);

  l_mults  := List(HClassReps(LClassOfHClass(H)), h -> LeftGreensMultiplierNC(S, e, h) * e);
  r_mults  := List(HClassReps(RClassOfHClass(H)), h -> e * RightGreensMultiplierNC(S, e, h));
  rp_mults := List(HClassReps(RClassOfHClass(H)), h -> RightGreensMultiplierNC(S, h, e) * e);
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

  out := rec( rad := Rad, 
          transitions := r_mults,
              returns := rp_mults, 
              HList := LHH);

  SetRClassRadicalOfGroupHClass(H, out);

  return out;
end);
















InstallMethod(RClassRadicalBicharacterOfGroupHClass,  "for group H class",
[IsGroupHClass],
function(H)
  local S, e, Rec, Rad, LHH, map, invmap, r_mults, rp_mults,
        ListLClass, n, HH, CHH, ord, B, dim,
        CS, cS, CG, cG, mat, compt,
        h, k, chi, ind_r, r, row, i, coeff,
        ind_transition, ind_groupe, x, lp, g, ind_l_class;

  S   := ParentAttr(H);
  e   := MultiplicativeNeutralElement(H);

  CS   := GeneralisedConjugacyClassesRepresentatives(S);
  cS   := Length(CS);

  map := IsomorphismPermGroup(H);
  invmap := InverseGeneralMapping(map);

  HH  := Range(map);
  CHH  := ConjugacyClasses(OrdinaryCharacterTable(HH));

  CG   := List(List(CHH, Representative) , x -> x ^ invmap);
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
        row := List([1 .. n], x-> 0);
        compt := 0;
        # Computing the image of the vector
        for i in [1 .. n] do
          coeff := r[i];
          if coeff = 0 then continue; fi;
          #Print(r, "\n");
          ind_transition := QuoInt(i - 1, ord) + 1;
          ind_groupe := RemInt(i - 1, ord) + 1;
          x := k * LHH[ind_groupe] * r_mults[ind_transition] * h;
          ind_transition := Position(ListLClass, LClass(S, x));
          if not ind_transition = fail then
            compt := compt + 1;
            #Print("-----------Here------------", ind_transition, " ", coeff , "\n\n");
            lp := rp_mults[ind_transition];
            #Print(e, x, lp, e*x*lp, Representative(H), "\n\n");
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

  SetRClassRadicalBicharacterOfGroupHClass(H,mat);

  return mat;
end);






























InstallMethod(DiagonalOfCharacterTables,  "for a semigroup",
[IsSemigroup],
function(S)
  # local CS, n, M, transversalHclasses, maps, map, XG, CG, h, k,
  # 	b, e, G, I, l, i, j;

  local CS, n, M, transversalHclasses, maps, groups, charactertables,
      irrs, mats;

  CS := GeneralisedConjugacyClassesRepresentatives(S);
  n := Length(CS);

  transversalHclasses := List(RegularDClasses(S), GroupHClass);
  maps := List(transversalHclasses, IsomorphismPermGroup);
  groups := List(maps, Range);
  charactertables := List(groups,CharacterTable);
  irrs := List(charactertables,Irr);
  mats := List(irrs,x -> List(x,ValuesOfClassFunction));
  M := DirectSumMat(mats);

  # b := 0;
  # for map in maps do
  #   G := Range(map);
  #   XG := CharacterTable(G);
  #   I  := Irr(XG);
  #   CG := ConjugacyClasses(XG);
  #   l  := Length(I);
  #   for i in [1..l] do
  #     h := ConjugacyClass(G, CS[i+b] ^ map);
  #     for j in [1..l] do
  #       k := ConjugacyClass(G, CS[j+b] ^ map);
  #       M[i+b][j+b] := I[Position(CG, h)][Position(CG, k)];
  #     od;
  #   od;
  #   b := b + l;
  # od;

  

  SetDiagonalOfCharacterTables(S,M);

  return M;
end);

























#############################################################################
##
#A  MonoidCharacterTable( <M> )
##
##  <#GAPDoc Label="MonoidCharacterTable">
##  <ManSection>
##  <Attr Name="MonoidCharacterTable" Arg='M'/>
##
##  <Description>
##  Called with a finite monoid <A>M</A>,
##  <Ref Attr="MonoidCharacterTable"/> returns the character table of the monoid 
##  that is, the matrix dim Hom(P,Q)/dim End(P / rad(FM)), where P
##  and Q run over the left indecomposable projective modules of FM.
##  <P/>
##  If <A>M</A> is the only argument then
##  <Ref Attr="MonoidCartanMatrix"/> returns the Cartan matrix of the monoid 
##  algebra FM, where F is a splitting field of M over the rationals.
##  <P/>
##  At the moment, methods are available for the following cases:
##  if <A>F</A> is not given (i.e. it defaults to the splitting field) and 
##  <A>G</A> is a finite monoid,
##  the method of _____________ is used.
##  <P/>
##  Otherwise, if <A>F</A> and <A>M</A> are both finite,
##  MeatAxe methods are used which can make
##  this an expensive operation.
##  <P/>
##  For other cases no methods are implemented yet.
##  <P/>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##

InstallMethod(Irr,  "for a semigroup",
[IsMonoidCharacterTable],
function(ct)
  local R, Rrad, D, transversalHclasses, out, irrvalues;

  D := DiagonalOfCharacterTables(ParentAttr(ct));

  transversalHclasses := List(RegularDClasses(ParentAttr(ct)), GroupHClass);


  R := Concatenation(List(transversalHclasses, RClassBicharacterOfGroupHClass));
  Rrad := Concatenation(List(transversalHclasses, RClassRadicalBicharacterOfGroupHClass));

  irrvalues := Inverse(TransposedMat(D)) * (R - Rrad);

  out := List(irrvalues, x -> MonoidCharacter(ct,x));

  SetIrr(ct,out);

  return out;
end);















#############################################################################
##
#A  MonoidCartanMatrix( <M>[, <F>] )
##
##  <#GAPDoc Label="MonoidCartanMatrix">
##  <ManSection>
##  <Attr Name="MonoidCartanMatrix" Arg='M[, F]'/>
##
##  <Description>
##  Called with a finite monoid <A>M</A> and a field <A>F</A>,
##  <Ref Attr="MonoidCartanMatrix"/> returns the Cartan matrix of the monoid 
##  algebra FM that is, the matrix dim Hom(P,Q)/dim End(P / rad(FM)), where P
##  and Q run over the left indecomposable projective modules of FM.
##  <P/>
##  If <A>M</A> is the only argument then
##  <Ref Attr="MonoidCartanMatrix"/> returns the Cartan matrix of the monoid 
##  algebra FM, where F is a splitting field of M over the rationals.
##  <P/>
##  At the moment, methods are available for the following cases:
##  if <A>F</A> is not given (i.e. it defaults to the splitting field) and 
##  <A>G</A> is a finite monoid,
##  the method of _____________ is used.
##  <P/>
##  Otherwise, if <A>F</A> and <A>M</A> are both finite,
##  MeatAxe methods are used which can make
##  this an expensive operation.
##  <P/>
##  For other cases no methods are implemented yet.
##  <P/>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##



InstallMethod(PimMonoidCharacter,  " ",
[IsMonoidCharacterTable, IsDenseList, IsMonoidCharacter],
function(ct,values,char)
  local result;

  result := Objectify(MonoidCharacterType, rec());
  SetParentAttr(result, ct);
  SetValuesOfCompositionFactorsFunction(result, values);
  SetProjectiveCoverOf(result,char);

  return result;
end);



InstallMethod(Pims,  "for a semigroup",
[IsMonoidCharacterTable],
function(ct)
  local C, S, M, out, pims;

  S := ParentAttr(ct);

  C := List(Irr(ct),ValuesOfMonoidClassFunction);

  M := RegularRepresentationBicharacter(S);

  out := Inverse(TransposedMatMutable(C)) * M * Inverse(C);

  pims := List([1..Length(out)], n -> PimMonoidCharacter(ct, out[n], Irr(ct)[n]));

  SetPims(ct,pims);

  return pims;
end);




InstallMethod(MonoidCartanMatrix,  "for a semigroup",
[IsSemigroup],
function(S)
  local out;

  out := List(Pims(MonoidCharacterTable(S)),ValuesOfCompositionFactorsFunction);

  # SetMonoidCartanMatrix(S,out);

  return out;
end);



