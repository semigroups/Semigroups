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
	
	fam := NewFamily( "LeftTranslationsSemigroupElementsFamily",
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
		Error("usage: the first argument must be a semigroup of left translations");
		return;
	fi;
	
	if not (UnderlyingSemigroup(L) = Source(f) and Source(f) = Range(f)) then
		Error("usage: the second argument must be a function from the underlying ",
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
[IsRectangularBand], 
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
		Error("usage: the first argument must be a semigroup of right translations");
		return;
	fi;
	
	if not (UnderlyingSemigroup(R) = Source(f) and Source(f) = Range(f)) then
		Error("usage: the second argument must be a function from the underlying semigroup",
		" of the semigroup of right translations to itself");
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
		Error("usage: the first argument must be a translational hull");
	fi;
	
	if not (IsLeftTranslationsSemigroupElement(l) and 
						IsRightTranslationsSemigroupElement(r)) then
		Error("usage: the second argument must be a left translation",
			" and the third argument must be a right translation");
		return;
	fi;
	
	L := LeftTranslationsSemigroupOfFamily(FamilyObj(l));
	R := RightTranslationsSemigroupOfFamily(FamilyObj(r));
	
	if not UnderlyingSemigroup(L) = UnderlyingSemigroup(R) then
			Error("usage: each argument must have the same underlying semigroup");
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

InstallMethod(ViewObj, "for a semigroup of left translations", 
	[IsLeftTranslationsSemigroup], PrintObj);

InstallMethod(PrintObj, "for a semigroup of left translations",
	[IsLeftTranslationsSemigroup and HasGeneratorsOfSemigroup],
function(L)
	Print("<semigroup of left translations of ", 
		UnderlyingSemigroup(L), " with ",
		Length(GeneratorsOfSemigroup(L)),
		" generators");
	if Length(GeneratorsOfSemigroup(L)) > 1 then
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
[IsTranslationsSemigroup],
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
		return ORB_HashFunctionForTransformations(x![1]![1], data)
			+ ORB_HashFunctionForTransformations(x![2]![1], data) mod data + 1;
end;

InstallMethod(ChooseHashFunction, "for a translational hull element and int",
[IsTranslationalHullElement, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForTranslationalHullElements,
             data := hashlen);
end);



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
