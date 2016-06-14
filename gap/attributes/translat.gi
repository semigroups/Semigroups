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

InstallMethod(TranslationalHull, "for a rectangular band",
[IsRectangularBand], 
function(S)
	local iso, reesMatSemi, reesMat, sizeI, sizeL, leftGens, rightGens, map,
	 mapAsTransList, semiList, leftHullGens, rightHullGens, hull, reesMappingFunction, 
	 i, j, k, l;
	
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

InstallMethod(LeftTranslationsNew, "for a semigroup with known generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local digraph, n, nrgens, nrgraphs, out, colors, gens, i, tempi, revbinbitsi, 
  j, k;

  digraph  := RightCayleyGraphSemigroup(S);
  n        := Length(digraph);
  nrgens   := Length(digraph[1]);
  nrgraphs := LogInt(nrgens,2) + 1;
  out      := [];
  colors   := [];


  for i in [1 .. nrgraphs] do
  	for j in [1 .. n] do
  	  out[n * (i - 1) + j]    := [];
  	    for k in [1 .. nrgraphs] do
  	      if i <> k  then
  	      	Add(out[n * (i - 1) + j], n * (k - 1) + j);
  	      fi;
  	    colors[n * (i - 1) + j] := i;
  	    od;
  	od;
  od;
  
  		
  for i in [1 .. nrgens] do
  	tempi       := i;
  	revbinbitsi := [];
  	while tempi > 0 do
  		revbinbitsi[LogInt(tempi,2)+1] := 1;
  		tempi := tempi - 2^LogInt(tempi,2);
    od;
    for j in [1 .. nrgraphs] do
      if IsBound(revbinbitsi[j]) then
        for k in [1 .. n] do
        	Add(out[n * (j - 1) + k], n * (j - 1) + digraph[k][i]);
  		od;
  	  fi;
  	od;
  od;
  	
  gens := GeneratorsOfEndomorphismMonoid(Digraph(out), colors);
  Apply(gens, x -> RestrictedTransformation(x, [1 .. n]));
  return Semigroup(gens, rec(small := true));
  #DOESN'T PRESERVE ENDOMORPHISMS
  #DOES PRESERVE AUTOMORPHISMS? TODO: Prove
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
