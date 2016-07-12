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

InstallMethod(TranslationalHullSemigroup, "for a semigroup",
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
  
  return [transList{[1..Length(transList) - 1]}, linkedTransList];
end;

#TODO: sort out where the failedComponents is going
#For a pair of transformations on the indices of a completely 0-simple semigroup
#determine the functions to the group associated with the RMS representation
#which will form translations when combined with the transformations
SEMIGROUPS.FindTranslationFunctionsToGroup := function(S, t1, t2, failedComponents)
  local reesmatsemi, mat, gplist, gpsize, nrrows, nrcols, invmat,
        rowtransformedmat, zerorow, zerocol, pos, satisfied,
        rels, edges, rowrels, relpoints, rel, i, j, k, l, x, y, r, n, q,
        funcs, digraph, cc, comp, iterator, foundfuncs, failedcomps,
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
  #TODO: check safe to assume components ordered?
  relpoints := [];
  for i in cc.comps do
    if Length(i) > 1 then 
      Add(relpoints, i[1]);
    fi;
  od;
  
  #check whether these connected components have already been found to fail
  for i in [1..Length(failedComponents)] do
    if ForAny(failedComponents[i][1], comp -> comp in cc.comps) then
      if failedComponents[i][2] = t1{relpoints} and 
        failedComponents[i][3] = t2{relpoints} then
        return [];
      fi;
    fi;
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
  
  #TODO: prove you only need to check the relations once
  #for all choices of x{relpoints}
  #i.e., the only possible problem is an inconsistency 
  #which if it exists will always occur
  
  if IsDoneIterator(iterator) then
    return [];
  elif not relssatisfied(funcsfromrelpointvals(NextIterator(iterator))) then
    return [0, Set(failedcomps), t1{relpoints}, t2{relpoints}];
  fi;
  
  while not IsDoneIterator(iterator) do
    Add(foundfuncs, funcsfromrelpointvals(NextIterator(iterator)));
  od;
  
  return foundfuncs;
end;

#TODO: work out what to actually call this
InstallMethod(TranslationalHull, "for a finite 0-simple semigroup",
[IsFinite and IsZeroSimpleSemigroup],
function(S)
  local tt, failedcomponents, iterator, transfuncs, unboundpositions,
        L, R, H, linkedpairs, i, j, c, linkedpairfromfuncs, iso, inv, nrrows,
        nrcols, reesmatsemi, zero, fl, fr, l, r, t1, t2, funcs, fx, fy,
        partialfunciterator, funcvals;
  
  iso := IsomorphismReesZeroMatrixSemigroup(S);
  inv := InverseGeneralMapping(iso);
  reesmatsemi := Range(iso);
  nrrows := Length(Matrix(reesmatsemi));
  nrcols := Length(Matrix(reesmatsemi)[1]);
  zero := MultiplicativeZero(reesmatsemi);
  L := LeftTranslationsSemigroup(S);
  R := RightTranslationsSemigroup(S);
  H := TranslationalHullSemigroup(S);
  tt := SEMIGROUPS.FindTranslationTransformations(S);
  failedcomponents := [];
  linkedpairs := [];
  
  linkedpairfromfuncs := function(t1, t2, fx, fy)
    fl := function(x)
      if x[1]^t1 <> nrrows + 1 then
        return RMSElement(reesmatsemi, x[1]^t1, fx[x[1]] * x[2], x[3]);
      else
        return zero;
      fi;
    end;
    
    fr := function(x)
      if x[3]^t2 <> nrcols + 1 then
        return RMSElement(reesmatsemi, x[1], x[2] * fy[x[3]], x[3]^t2);
      else
        return zero;
      fi;
    end;
    
    l := LeftTranslation(L, CompositionMapping(inv, MappingByFunction(
      reesmatsemi, reesmatsemi, fl), iso));
    
    r := RightTranslation(R, CompositionMapping(inv, MappingByFunction(
      reesmatsemi, reesmatsemi, fr), iso));
    
    return TranslationalHullElement(H, l, r);
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
            fx := transfuncs[1];
            fy := transfuncs[2];
            unboundpositions := [];
            for j in [1..Length(transfuncs[2])] do
              if not IsBound(transfuncs[2]) then 
                Add(unboundpositions, i);
              fi;
            od;
            if Length(unboundpositions) > 0 then 
              c := List([1..Length(unboundpositions)], i -> [1..Length(fy)]);
              partialfunciterator := IteratorOfCartesianProduct(c);
              while not IsDoneIterator(partialfunciterator) do
                funcvals := NextIterator(partialfunciterator);
                for j in [1..Length(unboundpositions)] do
                  fy[unboundpositions[j]] := funcvals[j];
                od;
                Add(linkedpairs, linkedpairfromfuncs(t1, t2, fx, fy));
              od;
            else
              Add(linkedpairs, linkedpairfromfuncs(t1, t2, fx, fy));
            fi;
          od;
        fi;
      fi;
    od;
  od;
  
  return linkedpairs;
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
