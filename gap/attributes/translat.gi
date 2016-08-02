############################################################################
##
#W  translat.gi
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(LeftTranslations, "for a semigroup", 
[IsSemigroup and IsFinite], 
function(S) 
  local L;
  
  L := SEMIGROUPS.LeftTranslationsSemigroup(S);
  TranslationalElements(L);
  
  return L;
end);

SEMIGROUPS.LeftTranslationsSemigroup := function(S)
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
end;

InstallGlobalFunction(LeftTranslation,
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
  
  semiList:=AsList(UnderlyingSemigroup(L));
  mapAsTransList := [];
  for i in [1..Length(semiList)] do
    mapAsTransList[i] := Position(semiList, semiList[i]^f);
  od;
  
  return LeftTranslationNC(L, Transformation(mapAsTransList));
end);

InstallGlobalFunction(LeftTranslationNC,
function(L, t)
  return Objectify(TypeLeftTranslationsSemigroupElements(L), [t]);
end);

InstallMethod(RightTranslations, "for a semigroup", 
[IsSemigroup and IsFinite], 
function(S) 
  local R;
  
  R := SEMIGROUPS.RightTranslationsSemigroup(S);
  TranslationalElements(R);
  
  return R;
end);

SEMIGROUPS.RightTranslationsSemigroup := function(S)
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
end;

InstallGlobalFunction(RightTranslation,
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

  semiList:=AsList(UnderlyingSemigroup(R));
  mapAsTransList := [];
  for i in [1..Length(semiList)] do
    mapAsTransList[i] := Position(semiList, semiList[i]^f);
  od;
  
  return RightTranslationNC(R, Transformation(mapAsTransList));
end);

InstallGlobalFunction(RightTranslationNC,
function(R, t)
  return Objectify(TypeRightTranslationsSemigroupElements(R), [t]);
end);

InstallMethod(TranslationalHull, "for a semigroup",
[IsSemigroup and IsFinite],
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
  TranslationalElements(H);
  
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

InstallMethod(ViewObj, "for a semigroup of left or right translations",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  Print("<the semigroup of");
  if IsLeftTranslationsSemigroup(T) then Print(" left");
  else Print(" right"); fi;
  Print(" translations of ", ViewString(UnderlyingSemigroup(T)), ">");
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
  Print("translations of ", ViewString(UnderlyingSemigroup(T)), " with ",
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

InstallMethod(Enumerator, "for a semigroup of left or right translations",
[IsTranslationsSemigroup],
function(T)
  return Enumerator(TranslationalElements(T));
end);

InstallMethod(Enumerator, "for a semigroup of translational hull elements",
[IsTranslationalHull],
function(H)
  return Enumerator(TranslationalElements(H));
end);

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

InstallMethod(Size, "for the translational hull of a rectangular band",
[IsTranslationalHull and IsWholeFamily],
function(H)
  local S, L, R;
  S := UnderlyingSemigroup(H);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;
  L := LeftTranslations(S);
  R := RightTranslations(S);
  return Size(L) * Size(R);
end);

InstallMethod(Size, "for the left/right translations of a semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  return Size(TranslationalElements(T));
end);

InstallMethod(Size, "for the translational hull of a semigroup",
[IsTranslationalHull and IsWholeFamily],
function(T)
  return Size(TranslationalElements(T));
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

InstallMethod(GeneratorsOfSemigroup, "for the translational hull of a rectangular band", 
[IsTranslationalHull],
function(H)
  local S, leftGens, rightGens, l, r, gens;
  
  S := UnderlyingSemigroup(H);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;

  leftGens := GeneratorsOfSemigroup(LeftTranslations(S));
  rightGens := GeneratorsOfSemigroup(RightTranslations(S));
  gens := [];
  
  for l in leftGens do
    for r in rightGens do
      Add(gens, TranslationalHullElement(H, l, r));
    od;
  od;
  
  return gens;
end);

InstallMethod(TranslationalElements, "for the semigroup of left/right translations of a rectangular band",
[IsTranslationsSemigroup and IsWholeFamily], 1,
function(T)
  if not IsRectangularBand(UnderlyingSemigroup(T)) then
    TryNextMethod();
  fi;
  return Semigroup(GeneratorsOfSemigroup(T));
end);

InstallMethod(TranslationalElements, "for the translational hull of a rectangular band",
[IsTranslationalHull and IsWholeFamily], 1,
function(H)
  if not IsRectangularBand(UnderlyingSemigroup(H)) then
    TryNextMethod();
  fi;
  return Semigroup(GeneratorsOfSemigroup(H));
end);


InstallMethod(\*, "for left translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y)
  return Objectify(FamilyObj(x)!.type, [y![1]*x![1]]);
end);

InstallMethod(\=, "for left translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y) 
  return x![1] = y![1];
end);

InstallMethod(\<, "for left translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y) 
  return x![1] < y![1];
end);

InstallMethod(\*, "for right translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y)
  return Objectify(FamilyObj(x)!.type, [x![1]*y![1]]);
end);

InstallMethod(\=, "for right translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y) 
  return x![1] = y![1];
end);

InstallMethod(\<, "for right translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y) 
  return x![1] < y![1];
end);

InstallMethod(\^, "for a semigroup element and a translation",
[IsAssociativeElement, IsTranslationsSemigroupElement],
function(x, t)
  local list;
  if IsLeftTranslationsSemigroupElement(t) then
    list := AsList(UnderlyingSemigroup(LeftTranslationsSemigroupOfFamily(FamilyObj(t))));
  else
    list := AsList(UnderlyingSemigroup(RightTranslationsSemigroupOfFamily(FamilyObj(t))));
  fi;
  if not x in list then
    Error("Semigroups: ^ for a semigroup element and translation: \n",
          "the first argument must be an element of the domain of the second");
  fi;
  return list[Position(list, x)^t![1]];
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
    return Size(T) = Size(LeftTranslationsSemigroupOfFamily(ElementsFamily(
                                                                FamilyObj(T))));
  else return Size(T) = Size(RightTranslationsSemigroupOfFamily(ElementsFamily(
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

InstallMethod(TranslationalElements, "for the translational hull of an arbitrary finite semigroup",
[IsTranslationalHull and IsWholeFamily],
function(H)
  local S, multtable, transpose, reps, repspos, dclasses, lclasses, rclasses,
        d, f, g, i, j, k, m, n, p, r, s, slist, fposrepk, gposrepk,
        possiblefrepvals, possiblegrepvals, whenboundfvals, whenboundgvals, pos,
        fvalsi, gvalsi, ftransrestrictionatstage, gtransrestrictionatstage, 
        flinkedrestrictionatstage, glinkedrestrictionatstage, 
        extendf, extendg, reject, propagatef, propagateg, restrictfromf,
        restrictfromg, bt, unrestrict, linkedpairs;
  
  S := UnderlyingSemigroup(H);                  
  n := Size(S);
  slist := ShallowCopy(AsList(S));
  Sort(slist);
  multtable := MultiplicationTable(S);
  transpose := TransposedMat(multtable);
  #for now, choose the reps by L/R classes 
  dclasses := DClasses(S);
  reps := [];
  repspos := [];
  
  for d in dclasses do
    lclasses := ShallowCopy(LClasses(d));
    rclasses := ShallowCopy(RClasses(d));
    for i in [1 .. Minimum(Size(lclasses), Size(rclasses)) - 1] do
      r := Representative(Intersection(lclasses[1], rclasses[1]));
      Add(reps, r);
      Add(repspos, Position(slist, r));
      Remove(lclasses, 1);
      Remove(rclasses, 1);
    od;
    if Size(lclasses) > Size(rclasses) then
      #Size(rclasses) = 1
      for j in [1 .. Size(lclasses)] do
        r := Representative(Intersection(lclasses[1], rclasses[1]));
        Add(reps, r);
        Add(repspos, Position(slist, r));
        Remove(lclasses, 1);
      od;
    else
      #Size(lclasses) = 1
      for j in [1 .. Size(rclasses)] do
        r := Representative(Intersection(lclasses[1], rclasses[1]));
        Add(reps, r);
        Add(repspos, Position(slist, r));
        Remove(rclasses, 1);
      od;
    fi;
  od;
  
  m := Size(reps);
    
  extendf := function(k)
    f[repspos[k + 1]] := possiblefrepvals[k + 1][1];
    return k + 1;
  end;
  
  propagatef := function(k)
    for s in S do
      pos := Position(slist, reps[k] * s);
      if IsBound(f[pos]) then
        if not f[pos] = Position(slist, slist[f[repspos[k]]] * s) then
          UniteSet(glinkedrestrictionatstage[k][k], possiblegrepvals[k]);
          possiblegrepvals[k] := [];
          return fail;
        fi;
      else
        f[pos] := Position(slist, slist[f[repspos[k]]] * s);
        whenboundfvals[pos] := k;
        if pos in repspos then
          j := Position(repspos, pos);
          UniteSet(ftransrestrictionatstage[j][k],
                   Difference(possiblefrepvals[j], [Position(slist, 
                                                slist[f[repspos[k]]] * s)]));
          possiblefrepvals[j] := Intersection(possiblefrepvals[j], 
                                              [Position(slist, 
                                                slist[f[repspos[k]]] * s)]);
        fi;
      fi;
    od;
    return k;
  end;
  
  propagateg := function(k)
    for s in S do
      pos := Position(slist, s * reps[k]);
      if IsBound(g[pos]) then
        if not g[pos] = Position(slist, s * slist[g[repspos[k]]]) then
          return fail;
        fi;
      else
        g[pos] := Position(slist, s * slist[g[repspos[k]]]);
        whenboundgvals[pos] := k;
        if pos in repspos then
          j := Position(repspos, pos);
          UniteSet(gtransrestrictionatstage[j][k],
                   Difference(possiblegrepvals[j], [Position(slist, 
                                                s * slist[g[repspos[k]]])]));
          possiblegrepvals[j] := Intersection(possiblegrepvals[j], 
                                              [Position(slist, 
                                                s * slist[g[repspos[k]]])]);
        fi;
      fi;
    od;
    return k;
  end;
  
  restrictfromf := function(k)
    for i in [k + 1 .. m] do
      for s in S do
        #restrict by the translation condition
        for p in PositionsProperty(multtable[repspos[i]], 
                  x -> x = Position(slist, reps[k] * s)) do
          fvalsi := PositionsProperty(transpose[p], 
                      x -> x = Position(slist, slist[f[repspos[k]]] * s));
          UniteSet(ftransrestrictionatstage[i][k], 
                    Difference(possiblefrepvals[i], fvalsi));
          possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
        od;
        
        #deal with the cases reps[i] = reps[k] * s and reps[i] * t = reps[k]
        if reps[i] = reps[k] * s then 
          fvalsi := [Position(slist, slist[f[repspos[k]]] * s)];
          UniteSet(ftransrestrictionatstage[i][k],
                    Difference(possiblefrepvals[i], fvalsi));
          possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
        fi;
      od;
      for p in PositionsProperty(multtable[repspos[i]], 
                                   x -> x = repspos[k]) do
        fvalsi := PositionsProperty(transpose[p], x -> x = f[repspos[k]]);  
        UniteSet(ftransrestrictionatstage[i][k], 
                  Difference(possiblefrepvals[i], fvalsi));
        possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
      od;
      if Size(possiblefrepvals[i]) = 0 then
        return fail;
      fi;
    od;
    for i in [k .. m] do
      for s in S do
        #restrict by the linked pair condition
        gvalsi := PositionsProperty(transpose[Position(slist, reps[k] * s)],
                    x -> x = Position(slist, 
                      reps[i] * slist[f[Position(slist, reps[k] * s)]]));  
        UniteSet(glinkedrestrictionatstage[i][k], 
                  Difference(possiblegrepvals[i], gvalsi));
        possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
      od;
      #deal with linked condition on reps[k]
      gvalsi := PositionsProperty(transpose[Position(slist, reps[k])],
                  x -> x= Position(slist, reps[k] * slist[f[repspos[k]]]));
      UniteSet(glinkedrestrictionatstage[i][k],
                Difference(possiblegrepvals[i], gvalsi));
      possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
      if Size(possiblegrepvals[i]) = 0 then
        return fail;
      fi; 
    od;
    return k;
  end;
  
  restrictfromg := function(k)
    for i in [k + 1 .. m] do
      for s in S do
        for p in PositionsProperty(transpose[repspos[i]], 
                  x -> x = Position(slist, s * reps[k])) do
          gvalsi := PositionsProperty(multtable[p], 
                      x -> x = Position(slist, s * slist[g[repspos[k]]]));
          UniteSet(gtransrestrictionatstage[i][k],
                    Difference(possiblegrepvals[i], gvalsi));
          possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
        od;
        
        #deal with the cases reps[i] = s * reps[k] and s * reps[i] = reps[k]
        if reps[i] = s * reps[k] then 
          gvalsi := [Position(slist, s * slist[g[repspos[k]]])];
          UniteSet(gtransrestrictionatstage[i][k],
                    Difference(possiblegrepvals[i], gvalsi));
          possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
        fi;
        
        for p in PositionsProperty(transpose[repspos[i]], 
                                   x -> x = repspos[k]) do
          gvalsi := PositionsProperty(multtable[p], x -> x = g[repspos[k]]);  
          UniteSet(gtransrestrictionatstage[i][k],
                    Difference(possiblegrepvals[i], gvalsi));
          possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
        od;
        
        fvalsi := PositionsProperty(multtable[Position(slist, s * reps[k])],
                    x -> x = Position(slist, 
                      slist[g[Position(slist, s * reps[k])]] * reps[i]));  
        UniteSet(flinkedrestrictionatstage[i][k], 
                  Difference(possiblefrepvals[i], fvalsi));
        possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
      od;
      if Size(possiblefrepvals[i]) = 0 or Size(possiblegrepvals[i]) = 0 then
        return fail;
      fi;
    od;
    return k;
  end;
  
  unrestrict := function(k, unrestrictf)
    for i in [1 .. n] do
        if whenboundgvals[i] = k then
          Unbind(g[i]);
          whenboundgvals[i] := 0;
        fi;
      od;
      for i in [k .. m] do
        UniteSet(possiblegrepvals[i], gtransrestrictionatstage[i][k]);
        UniteSet(possiblefrepvals[i], flinkedrestrictionatstage[i][k]);
        gtransrestrictionatstage[i][k] := [];
        flinkedrestrictionatstage[i][k] := [];
      od; 
  
    if(unrestrictf) then
      for i in [1 .. n] do
        if whenboundfvals[i] = k then
          Unbind(f[i]);
          whenboundfvals[i] := 0;
        fi;
      od;
      for i in [k .. m] do
        UniteSet(possiblefrepvals[i], ftransrestrictionatstage[i][k]);
        UniteSet(possiblegrepvals[i], glinkedrestrictionatstage[i][k]);
        ftransrestrictionatstage[i][k] := [];
        glinkedrestrictionatstage[i][k] := [];
      od;
    fi;
  end;
  
  reject := function(k)
    if k = 0 then
      return 0;
    fi;
    fposrepk := Position(possiblefrepvals[k], f[repspos[k]]);
    if IsBound(g[repspos[k]]) then
      gposrepk := Position(possiblegrepvals[k], g[repspos[k]]);
    else
      gposrepk := 0;
    fi;
    if gposrepk < Size(possiblegrepvals[k]) then
      g[repspos[k]] := possiblegrepvals[k][gposrepk + 1];
      unrestrict(k, false);
      return k;
    elif fposrepk < Size(possiblefrepvals[k]) then
      f[repspos[k]] := possiblefrepvals[k][fposrepk + 1];
      Unbind(g[repspos[k]]);
      unrestrict(k, true);
      return k;
    else
      if whenboundfvals[repspos[k]] = 0 then
        Unbind(f[repspos[k]]);
      fi;
      if whenboundgvals[repspos[k]] = 0 then
        Unbind(g[repspos[k]]);
      fi;
      unrestrict(k, true);
      return reject(k - 1);
    fi;
  end;
    
  bt := function(k)
    if k = m then
      if not (propagatef(k) = fail or propagateg(k) = fail) then
        Add(linkedpairs, [ShallowCopy(f), ShallowCopy(g)]);
      fi;
      return bt(reject(k));
    elif k = 0 then
      return 0;
    elif not (propagatef(k) = fail or restrictfromf(k) = fail) then
      if not IsBound(g[repspos[k]]) then
        g[repspos[k]] := possiblegrepvals[k][1];
      fi;
      if not (propagateg(k) = fail or restrictfromg(k) = fail) then
        return bt(extendf(k));
      else
        return bt(reject(k));
      fi;
    else 
      return bt(reject(k));
    fi;
  end;
  #The actual search
  
  ftransrestrictionatstage := List([1..m], x -> List([1..m], y -> []));  
  flinkedrestrictionatstage := List([1..m], x -> List([1..m], y -> []));
  gtransrestrictionatstage := List([1..m], x -> List([1..m], y -> []));
  glinkedrestrictionatstage := List([1..m], x -> List([1..m], y -> []));
  possiblefrepvals := List([1 .. m], x -> [1 .. n]);
  possiblegrepvals := ShallowCopy(possiblefrepvals);
  whenboundfvals := List([1 .. n], x -> 0);
  whenboundgvals := ShallowCopy(whenboundfvals);
  linkedpairs := [];
  
  f := [];
  g := [];
  bt(extendf(0));
  return linkedpairs;
end);

InstallMethod(LeftTranslations, "for the left translations of a semigroup with known generators",
[IsLeftTranslationsSemigroup and IsWholeFamily],
function(L)
  local S, digraph, n, nrgens, out, colors, gens, i, j;
  
  S := UnderlyingSemigroup(L);
  if not HasGeneratorsOfSemigroup(S) then
    TryNextMethod();
  fi;
  
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
  Apply(gens, x -> LeftTranslationNC(L, RestrictedTransformation(x, [1 .. n])));
  return Semigroup(gens, rec(small := true));
end);

InstallMethod(TranslationalElements, "for the right translations of a semigroup with known generators",
[IsRightTranslationsSemigroup and IsWholeFamily],
function(R)
  local S, digraph, n, nrgens, out, colors, gens, i, j;

  S       := UnderlyingSemigroup(R);
  if not HasGeneratorsOfSemigroup(S) then
    TryNextMethod();
  fi;
  
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
  Apply(gens, x -> RightTranslationNC(R,RestrictedTransformation(x, [1 .. n])));
  return Semigroup(gens, rec(small := true));
end);
