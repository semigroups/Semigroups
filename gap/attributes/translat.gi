############################################################################
##
#W  translat.gi
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
# (a * b) f = (a) f * b

#TODO: deal with infinite underlying semigroups


InstallMethod(LeftTranslationsSemigroup, "for a semigroup", 
[IsSemigroup], 
function(S) 
  local fam, type, L;
  
  fam := NewFamily("LeftTranslationsSemigroupElementsFamily",
                    IsLeftTranslationsSemigroupElement);
  
  #create the semigroup of left translations
  L := Objectify(NewType(CollectionsFamily(fam), IsLeftTranslationsSemigroup
                         and IsWholeFamily and IsAttributeStoringRep), rec());
    
  #store the type of the elements in the semigroup
  type := NewType(fam, IsLeftTranslationsSemigroupElement);
  
  fam!.type := type;
  SetTypeLeftTranslationsSemigroupElements(L, type);
  SetLeftTranslationsSemigroupOfFamily(fam, L); 
  
  SetUnderlyingSemigroup(L, S);
  return L;
end);


InstallGlobalFunction(LeftTranslation,
#why not filters? And should it be checked that this really is a translation?
function(L, f)
  local semiList, mapAsTransList, i;
  if not (IsLeftTranslationsSemigroup(L)) then
    Error("Semigroups: LeftTranslation: \n",
          "the first argument must be a semigroup of left translations");
    return;
  fi;
  
  if not (UnderlyingSemigroup(L) = Source(f) and Source(f) = Range(f)) then
    Error("Semigroups: LeftTranslation: \n",
          "the second argument must be a function from the underlying ",
          "semigroup of the semigroup of left translations to itself");
  fi;
  
  #TODO: this is a bit dodgy - what about infinite underlying semigroups?
  semiList:=AsList(UnderlyingSemigroup(L));
  mapAsTransList := [];
  for i in [1..Length(semiList)] do
    mapAsTransList[i] := Position(semiList, semiList[i]^f);
  od;
  
  return Objectify(TypeLeftTranslationsSemigroupElements(L),             
    [Transformation(mapAsTransList)]);
end);

InstallMethod(RightTranslationsSemigroup, "for a semigroup", 
[IsSemigroup], 
function(S) 
  local fam, type, R;
  
  fam := NewFamily( "RightTranslationsSemigroupElementsFamily",
          IsRightTranslationsSemigroupElement);
  
  #create the semigroup of right translations
  R := Objectify(NewType(CollectionsFamily(fam), IsRightTranslationsSemigroup 
    and IsWholeFamily and IsAttributeStoringRep ), rec() );
    
  #store the type of the elements in the semigroup
  type := NewType(fam, IsRightTranslationsSemigroupElement);
  
  fam!.type := type;
  SetTypeRightTranslationsSemigroupElements(R, type);
  SetRightTranslationsSemigroupOfFamily(fam, R); 
  
  SetUnderlyingSemigroup(R, S);
  return R;
end);


InstallGlobalFunction(RightTranslation,
#why not filters? And should it be checked that this really is a translation?
function(R, f)
  local semiList, mapAsTransList, i;
  if not (IsRightTranslationsSemigroup(R)) then
    Error("Semigroups: RightTranslation: \n",
          "the first argument must be a semigroup of right translations");
    return;
  fi;
  
  if not (UnderlyingSemigroup(R) = Source(f) and Source(f) = Range(f)) then
    Error("Semigroups: RightTranslation: \n",
          "the second argument must be a function from the underlying ",
          "semigroup of the semigroup of left translations to itself");
  fi;
  
  #this is a bit dodgy
  semiList:=AsList(UnderlyingSemigroup(R));
  mapAsTransList := [];
  for i in [1..Length(semiList)] do
    mapAsTransList[i] := Position(semiList, semiList[i]^f);
  od;
  
  return Objectify(TypeRightTranslationsSemigroupElements(R), 
  [Transformation(mapAsTransList)]);
end);

InstallMethod(TranslationalHull, "for a semigroup",
[IsSemigroup],
function(S)
  local fam, type, H;
  
  fam := NewFamily( "TranslationalHullElementsFamily", 
          IsTranslationalHullElement);
  
  #create the translational hull
   H := Objectify ( NewType ( CollectionsFamily( fam ), IsTranslationalHull and
     IsWholeFamily and IsAttributeStoringRep ), rec() );
   
   type := NewType(fam, IsTranslationalHullElement);
   
   fam!.type := type;
   SetTypeTranslationalHullElements(H, type);
   SetTranslationalHullOfFamily(fam, H);
  SetUnderlyingSemigroup(H, S);
  
  return H;
end);

InstallGlobalFunction(TranslationalHullElement, 
function(H, l, r) 
  local S, L, R;
  
  if not IsTranslationalHull(H) then 
    Error("Semigroups: TranslationalHullElement: \n",
          "the first argument must be a translational hull");
  fi;
  
  if not (IsLeftTranslationsSemigroupElement(l) and 
            IsRightTranslationsSemigroupElement(r)) then
    Error("Semigroups: TranslationalHullElement: \n",
          "the second argument must be a left translation",
          " and the third argument must be a right translation");
    return;
  fi;
  
  L := LeftTranslationsSemigroupOfFamily(FamilyObj(l));
  R := RightTranslationsSemigroupOfFamily(FamilyObj(r));
  
  if not UnderlyingSemigroup(L) = UnderlyingSemigroup(R) then
      Error("Semigroups: TranslationalHullElement: \n",
            "each argument must have the same underlying semigroup");
  fi;
  
  return Objectify(TypeTranslationalHullElements(H), [l, r]);
end);


InstallMethod(ViewObj, "for the semigroup of left or right translations of a rectangular band", 
[IsTranslationsSemigroup and IsWholeFamily], 
function(T)
  local S;
  S:=UnderlyingSemigroup(T);
  if not IsRectangularBand(S) then 
    TryNextMethod(); 
  fi;
  
  Print("<the semigroup of");
  if IsLeftTranslationsSemigroup(T) then Print(" left");
  else Print(" right"); fi;
  Print(" translations of a ", NrRClasses(S), "x", NrLClasses(S));
  Print(" rectangular band>"); 

end);

InstallMethod(PrintObj, "for the semigroup of left or right translations of a rectangular band",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S;
  S:=UnderlyingSemigroup(T);
  if not IsRectangularBand(S) then 
    TryNextMethod();
  fi;
  
  Print("<the semigroup of");
  if IsLeftTranslationsSemigroup(T) then Print(" left");
  else Print(" right"); fi;
  Print(" translations of ", S);
  Print(">");
  return;
end);

InstallMethod(ViewObj, "for a semigroup of translations", 
[IsTranslationsSemigroup], PrintObj);

InstallMethod(PrintObj, "for a semigroup of translations",
[IsTranslationsSemigroup and HasGeneratorsOfSemigroup],
function(T)
  Print("<semigroup of ");
  if IsLeftTranslationsSemigroup(T) then Print("left ");
  else Print("right ");
  fi;
  Print("translations of ", UnderlyingSemigroup(T), " with ",
    Length(GeneratorsOfSemigroup(T)),
    " generators");
  if Length(GeneratorsOfSemigroup(T)) > 1 then
    Print("s");
  fi;
  Print(">");
  return;
end);

InstallMethod(ViewObj, "for a translation", 
[IsTranslationsSemigroupElement], PrintObj);

InstallMethod(PrintObj, "for a translation",
[IsTranslationsSemigroupElement],
function(t)
  local L, S;
  L := IsLeftTranslationsSemigroupElement(t); 
  if L then 
    S := UnderlyingSemigroup(LeftTranslationsSemigroupOfFamily(FamilyObj(t)));
    Print("<left ");
  else 
    S := UnderlyingSemigroup(RightTranslationsSemigroupOfFamily(FamilyObj(t)));
    Print("<right ");
  fi;
  
  Print("translation on ", ViewString(S), ">");
end);

InstallMethod(ViewObj, "for a translational hull", 
[IsTranslationalHull], PrintObj);

InstallMethod(PrintObj, "for a translational hull",
[IsTranslationalHull and IsWholeFamily],
function(H)
  Print("<translational hull over ", ViewString(UnderlyingSemigroup(H)), ">");
end);

InstallMethod(ViewObj, "for a translational hull element", 
[IsTranslationalHullElement], PrintObj);

InstallMethod(PrintObj, "for a translational hull element",
[IsTranslationalHullElement],
function(t)
  local H;
  H := TranslationalHullOfFamily(FamilyObj(t));
  Print("<linked pair of translations on ", ViewString(UnderlyingSemigroup(H)), ">");
end);


#do I actually need to define Size/enumerator or will it inherit it from IsSemigroup?

InstallMethod(Size, "for the semigroup of left or right translations of a rectangular band", 
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S, n;
  S := UnderlyingSemigroup(T);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;
  if IsLeftTranslationsSemigroup(T) then
    n := NrRClasses(S);
  else n := NrLClasses(S);
  fi;
  
  return n^n;
end);

InstallMethod(Size, "for the semigroup of left or right translations of a completely 0-simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
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

InstallMethod(Size, "for the translational hull of a rectangular band",
[IsTranslationalHull and IsWholeFamily],
function(H)
  local S, L, R;
  S := UnderlyingSemigroup(H);
  L := LeftTranslationsSemigroup(S);
  R := RightTranslationsSemigroup(S);
  return Size(L) * Size(R);
end);
  
InstallMethod(Enumerator, "for the semigroup of left or right translations of a rectangular band", 
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S, semiList, iso, inv, reesMatSemi, L, size, 
    i, r, s, mapAsReesTransList, f;

  if not IsRectangularBand(UnderlyingSemigroup(T)) then
    TryNextMethod();
  fi;
  
  S := UnderlyingSemigroup(T);
  semiList := AsList(S);
  iso := IsomorphismReesMatrixSemigroup(S);
  inv := InverseGeneralMapping(iso);
  reesMatSemi := Range(iso);
  L := IsLeftTranslationsSemigroup(T);
  if L then
    size := Length(Rows(reesMatSemi));
  else
    size := Length(Columns(reesMatSemi));
  fi;
  
  return EnumeratorByFunctions(T, rec(
    enum := Enumerator(FullTransformationMonoid(size)),
    
    #TODO: find a better way of doing this - can probably use generators
    NumberElement := function(enum, x)
      mapAsReesTransList := [];
      if L then
        for i in [1..size] do
          r := RMSElement(S, i, (), 1);
          s := semiList[Position(semiList, (r^inv))^x![1]]^iso;
          mapAsReesTransList[i] := s[1];
        od;
      else 
        for i in [1..size] do
          r := RMSElement(S, 1, (), i);
          s := semiList[Position(semiList, (r^inv))^x![1]]^iso;
          mapAsReesTransList[i] := s[3];
        od;
      fi;
      return Position(enum!.enum, Transformation(mapAsReesTransList));
    end,
    
    ElementNumber := function(enum, n)
      if L then
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1]^enum!.enum[n], 
            (), x[3]);
        end;
        return LeftTranslation(T, CompositionMapping(inv, 
          MappingByFunction(reesMatSemi, reesMatSemi, f), iso));
      else 
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1], 
            (), x[3]^enum!.enum[n]);
        end;
        return RightTranslation(T, CompositionMapping(inv, 
          MappingByFunction(reesMatSemi, reesMatSemi, f), iso));
      fi;
    end,
    
    Length := enum -> Length(enum!.enum),
    
    PrintObj := function(enum)
      Print("<enumerator of translations of a rectangular band>");
      return;
    end));
end); 

InstallMethod(Enumerator, "for the translational hull of a rectangular band",
[IsTranslationalHull], 
function(H)
  local S;
  S := UnderlyingSemigroup(H);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;
  
  return EnumeratorByFunctions(H, rec(
    
    enum:=EnumeratorOfCartesianProduct(Enumerator(LeftTranslationsSemigroup(S)),
      Enumerator(RightTranslationsSemigroup(S))),
      
    NumberElement := function(enum, x)
      return Position(enum!.enum, [x![1], x![2]]);
    end,
    
    ElementNumber := function(enum, n)
      return Objectify(TypeTranslationalHullElements(H), enum!.enum[n]);
    end,
    
    Length := enum -> Length(enum!.enum),
    
    PrintObj := function(enum)
      Print("<enumerator of translational hull>");
      return;
    end));
end);

InstallMethod(GeneratorsOfSemigroup, "for the semigroup of left or right translations of a rectangular band",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S, L, n, iso, inv, reesMatSemi, semiList, gens, t, f;
  S := UnderlyingSemigroup(T);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;

  semiList := AsList(S);
  iso := IsomorphismReesMatrixSemigroup(S);
  inv := InverseGeneralMapping(iso);
  reesMatSemi := Range(iso);
  L := IsLeftTranslationsSemigroup(T);
  if L then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;
  
  gens := [];
  for t in GeneratorsOfMonoid(FullTransformationMonoid(n)) do
    if L then
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1]^t, 
            (), x[3]);
        end;
        Add(gens, LeftTranslation(T, CompositionMapping(inv, 
        MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else 
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1], 
            (), x[3]^t);
        end;
        Add(gens, RightTranslation(T, CompositionMapping(inv, 
          MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;
  return gens;
end);      

InstallMethod(GeneratorsOfSemigroup, "for the left/right translations of a finite 0-simple semigroup",
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
      Add(gens, LeftTranslation(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else
      f := function(x)
        if x = zero then 
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1], 
          x[2]*fa(x[3]), x[3]); 
      end;
      Add(gens, RightTranslation(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;
  return gens;
end);

InstallMethod(GeneratorsOfSemigroup, "for the translational hull of a rectangular band", 
[IsTranslationalHull],
function(H)
  local S, leftGens, rightGens, l, r, gens;
  
  S := UnderlyingSemigroup(H);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;

  leftGens := GeneratorsOfSemigroup(LeftTranslationsSemigroup(S));
  rightGens := GeneratorsOfSemigroup(RightTranslationsSemigroup(S));
  gens := [];
  
  for l in leftGens do
    for r in rightGens do
      Add(gens, TranslationalHullElement(H, l, r));
    od;
  od;
  
  return gens;
end);


InstallMethod(\*, "for translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y)
  return Objectify(FamilyObj(x)!.type, [x![1]*y![1]]);
end);

InstallMethod(\=, "for translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y) 
  return x![1] = y![1];
end);

InstallMethod(\<, "for translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y) 
  return x![1] < y![1];
end);

InstallMethod(\*, "for translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y)
  return Objectify(FamilyObj(x)!.type, [x![1]*y![1]]);
end);

InstallMethod(\=, "for translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y) 
  return x![1] = y![1];
end);

InstallMethod(\<, "for translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y) 
  return x![1] < y![1];
end);

InstallMethod(\*, "for translation hull elements (linked pairs)",
IsIdenticalObj,
[IsTranslationalHullElement, IsTranslationalHullElement],
function(x, y)
  return Objectify(FamilyObj(x)!.type, [x![1]*y![1], x![2]*y![2]]);
end);

InstallMethod(\=, "for translational hull elements (linked pairs)",
IsIdenticalObj,
[IsTranslationalHullElement, IsTranslationalHullElement],
function(x, y)
  return x![1] = y![1] and x![2] = y![2];
end);

InstallMethod(\<, "for translational hull elements (linked pairs)",
IsIdenticalObj,
[IsTranslationalHullElement, IsTranslationalHullElement],
function(x, y)
  return x![1] < y![1] or (x![1] = y![1] and x![2] < y![2]);
end);

InstallMethod(IsWholeFamily, "for a semigroup of translations of a rectangular band",
[IsTranslationsSemigroup],
function(T)
  if IsLeftTranslationsSemigroup(T) then  
    return Size(T)=Size(LeftTranslationsSemigroupOfFamily(ElementsFamily(
      FamilyObj(T))));
  else return Size(T)=Size(RightTranslationsSemigroupOfFamily(ElementsFamily(
      FamilyObj(T)))); 
  fi;
end);

InstallMethod(IsWholeFamily, "for a subsemigroup of the translational hull of a rectangular band",
[IsTranslationalHull],
function(H) 
  return Size(H) = Size(TranslationalHullOfFamily(ElementsFamily(FamilyObj(H))));
end);

InstallMethod(UnderlyingSemigroup, "for a semigroup of left or right translations",
[IsTranslationsSemigroup],
function(T)
  if IsLeftTranslationsSemigroup(T) then
    return UnderlyingSemigroup(LeftTranslationsSemigroupOfFamily(ElementsFamily(
      FamilyObj(T))));
  else 
    return UnderlyingSemigroup(RightTranslationsSemigroupOfFamily(ElementsFamily(
      FamilyObj(T))));
  fi;
end);

InstallMethod(UnderlyingSemigroup, "for a subsemigroup of the translational hull",
[IsTranslationalHull],
function(H)
    return UnderlyingSemigroup(TranslationalHullOfFamily(FamilyObj(
      Enumerator(H)[1])));
end);

SEMIGROUPS.HashFunctionForTranslationalHullElements := function(x, data)
    return (ORB_HashFunctionForTransformations(x![1]![1], data)
      + ORB_HashFunctionForTransformations(x![2]![1], data)) mod data + 1;
end;

InstallMethod(ChooseHashFunction, "for a translational hull element and int",
[IsTranslationalHullElement, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForTranslationalHullElements,
             data := hashlen);
end);

#Finds the transformations on the indices of a completely 0-simple semigroup
#which are candidates for translations, when combined with a function from
#the index sets to the group.
#TODO: swap rows/columns if more convenient.
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
      #only check the columns which contain a 1
      #all-zero column can be made by column not in domain of transformation
      #no ones in one partial matrix but not in the other...
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
      q := reject(q{[1..k-1]});
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
  #last element of transList will be 0, ignore
  for k in [1..Length(transList) - 1] do
    t := transList[k];
    cols := [];
    for i in [1..Length(simpleRows[1])] do
      col := [];
      for j in [1..nrRows] do
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
  
  return [transList, linkedTransList];
end;

#For a pair of transformations on the indices of a completely 0-simple semigroup
#determine the functions to the group associated with the RMS representation
#which will form translations when combined with the transformations
SEMIGROUPS.FindTranslationFunctionsToGroup := function(S, t1, t2)
  local reesmatsemi, mat, gplist, gpsize, nrrows, nrcols, invmat,
        rowtransformedmat, coltransformedmat, zerorow, zerocol, pos,
        rels, edges, rowrels, relpoints, rel, i, j, k, l, x, y, v, r, n, 
        funcs, changedvals, add, digraph, cc, comp,
        extend, reject, relssatisfied, bt, foundfuncs;
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
  coltransformedmat := [];
  for i in [1..nrcols] do
    if t2[i] <> nrcols + 1 then
      coltransformedmat[i] := TransposedMat(mat)[t2[i]];
    else
      coltransformedmat[i] := zerocol;
    fi;
  od;
  coltransformedmat := TransposedMat(coltransformedmat);

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
  #TODO: check safe to assume components ordered?
  relpoints := [];
  for i in cc.comps do
    if Length(i) > 1 then 
      Add(relpoints, i[1]);
    fi;
  od;            
  
  extend := function(funcs)
    x := ShallowCopy(funcs[1]);
    y := ShallowCopy(funcs[2]);
    for r in relpoints do
      if not IsBound(x[r]) then
        x[r] := gplist[1];
        changedvals := [r];
        comp := cc.comps[cc.id[r]];
        for n in [2..Length(comp)] do
          j := comp[n];
          if j <= nrrows then
            k := rowrels[r][j];
            x[j] := coltransformedmat[j][k] * invmat[r][t2[k]] * x[r] *
                    rowtransformedmat[r][k] * invmat[t1[j]][k];
            Add(changedvals, j);
          else
            break;
          fi;
        od;
        for rel in rels do
          if rel[1] in changedvals then
            i := rel[1];
            j := rel[2];
            y[j] := invmat[i][t2[j]] * x[i] * rowtransformedmat[i][j];
          fi;
        od;
      return [x,y];
      fi;
    od;
    return fail;
  end;
  
  #TODO: factor out the updating of the functions
  reject := function(funcs)
    x := ShallowCopy(funcs[1]);
    y := ShallowCopy(funcs[2]);
    for k in [1..Length(relpoints)] do
      if not IsBound(x[relpoints[k]]) then
        k := k-1;
        break;
      fi;
    od;
    r := relpoints[k];
    pos := Position(gplist, x[r]);
    if pos < gpsize then
      x[r] := gplist[pos + 1];
      changedvals := [r];
      comp := cc.comps[cc.id[r]];
      for n in [2..Length(comp)] do
        j := comp[n];
        if j <= nrrows then
          k := rowrels[r][j];
          x[j] := coltransformedmat[j][k] * invmat[r][t2[k]] * x[r] *
                  rowtransformedmat[r][k] * invmat[t1[j]][k];
          Add(changedvals, j);
        else
          break;
        fi;
      od;
      for rel in rels do
        if rel[1] in changedvals then
            i := rel[1];
            j := rel[2];
            y[j] := invmat[i][t2[j]] * x[i] * rowtransformedmat[i][j];
        fi;
      od;
      funcs := [x,y];
    elif k > 1 then
      funcs := reject([x{[1..relpoints[k-1]]}, y]);
    else return 0;
    fi;
  
    return funcs;
  end;
  
  relssatisfied := function(funcs)
    x := funcs[1];
    y := funcs[2];
    for rel in rels do
      i := rel[1];
      j := rel[2];
      
      if IsBound(x[i]) and IsBound(y[j]) and not 
          x[i] * rowtransformedmat[i][j] = coltransformedmat[i][j] * y[j] then
        return false;
      fi;
      
    od;
    return true;
  end;
  
  bt := function(funcs)
    if funcs = 0 then
      return 0;
    fi;
    if not relssatisfied(funcs) then
      funcs := reject(funcs);
    elif Length(funcs[1]) = nrrows then
      return funcs;
    else
      funcs := extend(funcs);
    fi;
    return bt(funcs);
  end;
  
  foundfuncs := [bt([[],[]])];
  v := 1;
  while foundfuncs[v] <> 0 do
    foundfuncs[v+1] := bt(reject(foundfuncs[v]));
    v := v + 1;
  od;
  return foundfuncs;
end;

InstallMethod(TranslationalHull2, "for a rectangular band",
[IsRectangularBand], 
function(S)
  local iso, reesMatSemi, reesMat, sizeI, sizeL, leftGens, rightGens, map,
   mapAsTransList, semiList, leftHullGens, rightHullGens, hull, 
   reesMappingFunction, i, j, k, l;
  
  iso := IsomorphismReesMatrixSemigroup(S);
  reesMatSemi := Range(iso);
  reesMat := Matrix(reesMatSemi);
  sizeI := Length(reesMat[1]);
  sizeL := Length(reesMat);
  
  leftGens := ShallowCopy(GeneratorsOfMonoid(FullTransformationMonoid(sizeI)));
  rightGens := ShallowCopy(GeneratorsOfMonoid(FullTransformationMonoid(sizeL)));
  Add(leftGens, IdentityTransformation);
  Add(rightGens, IdentityTransformation);
  
  leftHullGens:=[];
  semiList:= AsList(S);
  for i in [1..Length(leftGens)] do
    reesMappingFunction := function(x)
      return ReesMatrixSemigroupElement(reesMatSemi, x[1]^leftGens[i], (), x[3]);
    end;
    map := CompositionMapping(InverseGeneralMapping(iso), MappingByFunction(
      reesMatSemi, reesMatSemi, reesMappingFunction), iso);
    mapAsTransList := [];
    for l in [1..Length(semiList)] do
      mapAsTransList[l] := Position(semiList, semiList[l]^map);
    od;
    Add(leftHullGens, Transformation(mapAsTransList));
  od;   
  
  rightHullGens:=[];
  for j in [1..Length(rightGens)] do
    reesMappingFunction := function(x)
      return ReesMatrixSemigroupElement(reesMatSemi, x[1], (), x[3]^rightGens[j]);
    end;
    map := CompositionMapping(InverseGeneralMapping(iso), MappingByFunction(
      reesMatSemi, reesMatSemi, reesMappingFunction), iso);
    mapAsTransList := [];
    for l in [1..Length(semiList)] do
      mapAsTransList[l] := Position(semiList, semiList[l]^map);
    od;
    Add(rightHullGens, Transformation(mapAsTransList));
  od;
  
  hull := DirectProduct(Semigroup(leftHullGens), Semigroup(rightHullGens));
  return hull;

end);

InstallMethod(LeftTranslations, "for a semigroup with known generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local digraph, n, nrgens, out, colors, gens, i, j;

  digraph := RightCayleyGraphSemigroup(S);
  n       := Length(digraph);
  nrgens  := Length(digraph[1]);
  out     := [];
  colors  := [];

  for i in [1 .. n] do
    out[i]    := [];
    colors[i] := 1;
    for j in [1 .. nrgens] do
      out[i][j] := n + nrgens * (i - 1) + j;
      out[n + nrgens * (i - 1) + j] := [digraph[i][j]];
      colors[n + nrgens * (i - 1) + j] := j + 1;
    od;
  od;
  gens := GeneratorsOfEndomorphismMonoid(Digraph(out), colors);
  Apply(gens, x -> RestrictedTransformation(x, [1 .. n]));
  return Semigroup(gens, rec(small := true));
end);

InstallMethod(RightTranslations, "for a semigroup with known generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local digraph, n, nrgens, out, colors, gens, i, j;

  digraph := LeftCayleyGraphSemigroup(S);
  n       := Length(digraph);
  nrgens  := Length(digraph[1]);
  out     := [];
  colors  := [];

  for i in [1 .. n] do
    out[i]    := [];
    colors[i] := 1;
    for j in [1 .. nrgens] do
      out[i][j] := n + nrgens * (i - 1) + j;
      out[n + nrgens * (i - 1) + j] := [digraph[i][j]];
      colors[n + nrgens * (i - 1) + j] := j + 1;
    od;
  od;
  gens := GeneratorsOfEndomorphismMonoid(Digraph(out), colors);
  Apply(gens, x -> RestrictedTransformation(x, [1 .. n]));
  return Semigroup(gens, rec(small := true));
end);
