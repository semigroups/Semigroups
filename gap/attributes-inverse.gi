#############################################################################
##
#W  attributes-inverse.gi
#Y  Copyright (C) 2013                                   James D. Mitchell,
##                                                       Wilf Wilson,
##                                                       Rhiannon Dougall,
##                                                       Robert Hancock
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#

InstallMethod(VagnerPrestonRepresentation, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  local gens, elts, out, iso, T, inv, i;

  gens:=GeneratorsOfInverseSemigroup(S);
  elts:=Elements(S);
  out:=EmptyPlist(Length(gens));
 
  iso:=function(x)
    local dom;
    dom:=Set(elts*(x^-1));
    return PartialPermNC(List(dom, y-> Position(elts, y)), 
     List(List(dom, y-> y*x), y-> Position(elts, y)));
  end;
  
  for i in [1..Length(gens)] do
    out[i]:=iso(gens[i]);
  od;

  T:=InverseSemigroup(out);

  inv:=x-> ResultOfStraightLineProgram(SemigroupElementSLP(T, x),
     GeneratorsOfSemigroup(S));

  return MagmaIsomorphismByFunctionsNC(S, T, iso, inv);
end);

#C method? JDM

InstallMethod(Minorants, 
"for an inverse semigroup of partial permutation and an element",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  local out, elts, i, j, k;
  
  if not f in s then 
    Error("the second argument is not an element of the first,");
    return;
  fi;

  if HasNaturalPartialOrder(s) then 
    elts:=Elements(s);
    i:=Position(elts, f);
    return elts{NaturalPartialOrder(s)[i]};
  fi;

  if IsIdempotent(f) then 
    out:=EmptyPlist(NrIdempotents(s));
    elts:=SSortedList(Idempotents(s));
  else 
    out:=EmptyPlist(Size(s));
    elts:=Elements(s);
  fi;

  i:=Position(elts, f);
  j:=0; 

  for k in [1..i-1] do 
    if NaturalLeqPartialPerm(elts[k], f) and f<>elts[k] then 
      j:=j+1;
      out[j]:=elts[k];
    fi;
  od;
  ShrinkAllocationPlist(out);
  return out;
end);

#JDM c method, don't document until generalised

InstallGlobalFunction(SupremumIdempotentsNC, 
function(coll)
  local dom;

  if IsList(coll) and IsEmpty(coll) then 
    return PartialPerm([]);
  fi;
  if not IsPartialPermCollection(coll) then 
    Error("the argument should be a collection of partial perms,");
  fi;
  dom:=DomainOfPartialPermCollection(coll);
  return PartialPerm(dom, dom);
end);

#

InstallMethod(SameMinorantsSubgroup, 
"for a group H-class of an inverse semigroup of partial perms",
[IsGroupHClass],
function(h)
  local e, F, out, f, i;

  e:=Representative(h);
  F:=Minorants(Parent(h), e);
  h:=Elements(h);
  out:=[e];

  for i in [2..Length(h)] do
    if ForAll(F, f-> NaturalLeqPartialPerm(f, h[i])) then 
      Add(out, h[i]);
    fi;
  od;
  return out;
end);

#

InstallMethod(IsMajorantlyClosed, 
"for an inverse subsemigroup of partial permutations and an inverse subsemigroup",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(S, T)

  return IsMajorantlyClosed(S, Elements(T));

end);

#

InstallMethod(IsMajorantlyClosed, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

	local t;

  if not IsSubset(S,T) then
    Error("The second argument should be a subset of the first");
  else
    return IsMajorantlyClosedNC(S,T);
  fi;
	
	return;

end);

#

InstallMethod(IsMajorantlyClosedNC, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

  local t, iter, u, i;

	if Size(S) = Size(T) then
		return true;
	fi;
	
  i:=0;
  for t in T do
    iter:=Iterator(S);
    for u in iter do
      i:=i+1;
      if NaturalLeqPartialPerm(t, u) and not u in T then
        return false;
      fi;
    od;
  od;

  return true;

end);

#

InstallMethod(MajorantClosure, 
"for an inverse subsemigroup of partial permutations and an inverse subsemigroup",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(S, T)
  return MajorantClosure(S, Elements(T));;
end);

#

InstallMethod(MajorantClosure, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)
	local t;

  if not IsSubset(S,T) then
    Error("The second argument should be a subset of the first");
  else
    return MajorantClosureNC(S,T);
  fi;
	
	return;
end);

#

InstallMethod(MajorantClosureNC, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

	local elts, n, out, t, i, val, ht, k;
	
	elts:=Elements(S);
	n:=Length(elts);
	out:=EmptyPlist(n);
	ht:=HTCreate(T[1]);
	k:=0;
	
	for t in T do
		HTAdd(ht, t, true);
  	Add(out, t);
  	k:=k+1;
 	od;

	for t in out do
		for i in [1..n] do
			if NaturalLeqPartialPerm(t, elts[i]) then
				val:=HTValue(ht, elts[i]);
				if val=fail then
					k:=k+1;
					Add(out, elts[i]);
					HTAdd(ht, elts[i], true);
					if k=Size(S) then
						return out;
					fi;
				fi;
			fi;
		od;
	od;
	
 return out;
 
end);

#

InstallMethod(RightCosetsOfInverseSemigroup, 
"for an inverse semigroup of partial permutations and an inverse subsemigroup",
[IsInverseSemigroup and IsPartialPermSemigroup, IsInverseSemigroup and IsPartialPermSemigroup],
function(S, T)

  local s, t, dupe, idem, elts, rep, usedreps, coset, out;
  
  if not IsSubset(S,T) then
    Error("The second argument should be a subsemigroup of the first");
    return;
  fi;
  if not IsMajorantlyClosed(S,T) then
  	Error("The second argument should be majorantly closed.");
  fi;

  elts:=Elements(T);
  idem:=Representative(MinimalIdeal(T));
  usedreps:=[];
  out:=[];
  
  for s in RClass(S, idem) do
    
    # Check if Ts is a duplicate coset
    dupe:=false;    
    for rep in [1..Length(usedreps)] do
      if s*usedreps[rep]^-1 in elts then
        dupe:=true;
        break;
      fi;
    od;
    	  	
    if dupe then continue; fi;	
    	
    Add(usedreps, s);
    	
    coset:=[];
    for t in elts do
      Add(coset, t*s);
    od;
    coset:=Set(coset);

    # Generate the majorant closure of Ts to create the coset

    coset:=MajorantClosure(S, coset);      
    Add(out, coset);

  od;

  return out;
end);

#

InstallMethod(IsJoinIrreducible, 
"for an inverse semigroup of partial perms and a partial perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(S, x)

  local elts, i, j, k, y, singleline, sup;

  if not x in S then
    Error("The second argument should be a partial permutation within the first argument");
  fi;
  if IsMultiplicativeZero(S,x) then return false; fi;

  y:=LeftOne(x);    
  elts:=Set(Idempotents(S));;
  i:=Position(elts, y);
  k:=0;
  singleline:=true;

  # Find an element smaller than y, k
  for j in [i-1,i-2 .. 1] do
    if NaturalLeqPartialPerm(elts[j], elts[i]) then
      k:=j;
      break;
    fi;
  od;
  
  # If there is no smaller element k: true
  if k = 0 then return true; fi;

  # Look for other elements smaller than y which are not smaller than k
  for j in [1..(k-1)] do 
    if NaturalLeqPartialPerm(elts[j], elts[i]) and not NaturalLeqPartialPerm(elts[j], elts[k]) then 
      singleline:=false; 
      break;
    fi;
  od;

  if singleline then return true; fi;

  if Size(HClass(S, y)) = 1 then return false; fi;

  sup:=SupremumIdempotentsNC(Minorants(S, y));
  if y = sup then return false; fi;

  if ForAny(HClass(S, y), x-> NaturalLeqPartialPerm(sup,x) and x<>y) then
    return true;
  fi;
  
  return false;

end);

#

InstallMethod(JoinIrreducibleDClasses, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)

  local D, elts, out, seen_zero, rep, i, k, minorants, singleline, d, j, p;
  
  D:=GreensDClasses(S);
  elts:=Set(Idempotents(S));;
  out:=EmptyPlist(Length(D));
  seen_zero:=false;

  for d in D do
    rep:=Representative(d);
    
    if not seen_zero and IsMultiplicativeZero(S, rep) then 
      seen_zero:=true;
      continue; 
    fi;
    
    i:=Position(elts, rep);
    k:=First([i-1,i-2 .. 1], j-> NaturalLeqPartialPerm(elts[j], rep));

    if k=fail then # d is the minimal non-trivial D-class
      Add(out, d);
      continue; 
    fi;

    minorants:=[k];
    singleline:=true;

    for j in [1..k-1] do 
      if NaturalLeqPartialPerm(elts[j], rep) then 
        if singleline and not NaturalLeqPartialPerm(elts[j], elts[k]) then 
          # rep is the lub of {elts[j], elts[k]}, not quite 
          singleline:=false;
          if IsTrivial(SchutzenbergerGroup(d)) then 
            break;
          fi;
        else
          Add(minorants, j);
        fi;
      fi;
    od;

    if singleline then
      Add(out, d);
      continue;
    elif IsTrivial(SchutzenbergerGroup(d)) then 
      continue;
    fi;

    minorants:=Union(List(minorants, j-> DomainOfPartialPerm(elts[j])));

    if DomainOfPartialPerm(rep)=minorants then 
      # rep=lub(minorants) but rep not in minorants
      continue; 
    fi;

    for p in SchutzenbergerGroup(d) do
      if p<>() and ForAll(MovedPoints(p), x -> not x in minorants) then 
        # rep*p<>rep and rep, rep*p>lub(minorants) and rep||rep*p and 
        # hence neither rep*p nor rep is of any set.
        Add(out, d); 
        break;
      fi; 
    od;
  od;

  return out;
end);

#

InstallMethod(SmallerDegreePartialPermRepresentation, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)

  local out, oldgens, newgens, D, He, sup, trivialse, sigma, sigmainv, rho,
        rhoinv, orbits, HeCosetReps, Fei, FeiSigma, HeCosetRepsSigma,
        HeCosetsReps, h, CosetsInHe, numcosets, j, reps, lookup, gen, offset,
        rep, box, subbox, T, d, e, i, k, m, schutz, psi, psiinv, nrcosets,
        cosets, stab, stabpp;
          
  oldgens:=Generators(S);
  newgens:=List(oldgens, x-> []);  
  D:=JoinIrreducibleDClasses(S);

  for d in D do

    e:=Representative(d);
    ## He is a group H-Class in our join-irreducible D-Class ##
    ## Sigma: isomorphism to a perm group (unfortunately necessary)
    ## Psi: homom from Schutzenberger Group corresponding to He, to a perm group
    ## Rho: isomorphism to a smaller degree perm group
    He:=GroupHClass(d);

    sigma:=IsomorphismPermGroup(He);
    sigmainv:=InverseGeneralMapping(sigma);
    
    schutz:=SchutzenbergerGroup(d);
    sup:=SupremumIdempotentsNC(Minorants(S, e));
    trivialse:=not ForAny(He, x-> NaturalLeqPartialPerm(sup, x) and x<>e);
    
    psi:=ActionHomomorphism(
     schutz, Difference(DomainOfPartialPerm(e), DomainOfPartialPerm(sup)));
    psiinv:=InverseGeneralMapping(psi);

    rho:=SmallerDegreePermutationRepresentation(Image(psi));
    rhoinv:=InverseGeneralMapping(rho);

    ##  Se is the subgroup of He whose elements have the same minorants as e
    if trivialse then
      orbits:=[[ActionDegree(He)+1]];
      cosets:=[e];
      #stab:=schutz;
      stabpp:=He;
    else 
      orbits:=Orbits(Image(rho));
    fi;

    for i in orbits do

      if not trivialse then
        stab:=ImagesSet(psiinv, ImagesSet(rhoinv, 
         Stabilizer(Image(rho), i[1])));
        cosets:=RightTransversal(schutz, stab);
        stabpp:=ImagesSet(sigmainv, stab);
      fi; 

      ##### Generate representatives for all the H-Classes in the R-Class of He
      h:=HClassReps( RClassNC(d, e) );
      nrcosets:=Size(h)*Length(cosets);
      
      #### Generate representatives for ALL the cosets the generator will act on  
      #### Divide every H-Class in the R-Class into 'cosets' like stab in He
      j:=0;
      reps:=[];
      lookup:=EmptyPlist(Length(LambdaOrb(d)));
      for k in [1..Size(h)] do
        lookup[Position(LambdaOrb(d), ImageSetOfPartialPerm(h[k]))]:= k;
        for m in [1..Length(cosets)] do
          j:=j+1;
          reps[j]:=cosets[m]*h[k];
        od;
      od;
      ShrinkAllocationPlist(lookup);

      ##### Loop over old generators of S to calculate its action on the cosets
      for j in [1..Length(oldgens)] do
    
        gen:=oldgens[j];
        offset:=Length(newgens[j]);

        # Loop over cosets to calculate the image of each under the generator
        for k in [1..nrcosets] do
          rep:=reps[k]*gen;
          # Will the new generator will be defined at this point?
          if not rep*rep^(-1) in stabpp then
            Add(newgens[j], 0);
          else
            box:=lookup[Position(LambdaOrb(d), ImageSetOfPartialPerm(rep))];
            if trivialse then
              subbox:=1;
            else
              ## Below, could be ^sigma instead of AsPermutation
              subbox:=PositionCanonical(cosets,
               AsPermutation((rep*h[box]^(-1)))
              );
            fi;
            Add(newgens[j], (box-1)*Length(cosets)+subbox+offset);  
          fi;
        od;
      od; 
    od;        
  od;


  T:=InverseSemigroup(List(newgens, x->PartialPermNC(x)));

  # Return identity mapping if nothing has been accomplished; else the result.
  if NrMovedPoints(T) > NrMovedPoints(S) or 
    (NrMovedPoints(T) = NrMovedPoints(S) and ActionDegree(T) >= ActionDegree(S))
  then

    return IdentityMapping(S);

  else
    
    return MagmaIsomorphismByFunctionsNC(S, T,
      x -> ResultOfStraightLineProgram(
             SemigroupElementSLP(S, x), GeneratorsOfSemigroup(T)),
      x -> ResultOfStraightLineProgram(
           SemigroupElementSLP(T, x), GeneratorsOfSemigroup(S))
    );
    
  fi;
  
end);

#

#EOF