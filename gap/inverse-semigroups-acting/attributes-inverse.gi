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

#Giving our function a name and stating the local variables

SemiCharTable:=function(S) 
  local d_classrep, useful, schutzconjclassrep, listgroupchars, inneruseful, h, iso, isoinverse, c_list, schutz_h, schutz_h_class, c_rep, guys, ccl, tbl, GroupCharacterMatrices, x, innergpmatrix, l, innerrow, t, p, C, gps, conjugators, ll, hh, gp_below, gp_below2, innerfilter, innerfilter2, conjgp_below, innerconjrep, ClassIncidenceMatrix, A, e, c, i, j, r, y, hrep, s, g, k;
 

  # computing the D-Class representatives and ordering them in terms of rank

  d_classrep:=ShallowCopy(DClassReps(S)); 
  Sort(d_classrep, function(x,y) return RankOfPartialPerm(x) >
   RankOfPartialPerm(y); end); 
  useful:=[];
  schutzconjclassrep:=[];
  listgroupchars:=[];

  # in the following loop, for each D-Class rep found above, we find its
  # H-Class, an isomorphism that maps its H-Class to its corresponding
  # permutation group in S_n, the Schutzemberger group of the H-Class and the
  # Schutzenberger conjugacy class representatives. This gives us a lot of
  # useful information for later and we place all the above in a list (of
  # lists) called useful.  Since we also have enough information at this point
  # to compute the group character tables, we compute those as well.

  for e in d_classrep do
    inneruseful:=[];
    h:=HClass(S, e);
    iso:=IsomorphismPermGroup(h);
    isoinverse:=InverseGeneralMapping(iso);
    c_list:=[];
    schutz_h:=Range(iso);
    schutz_h_class:=ConjugacyClasses(schutz_h);
    for c in schutz_h_class do
      c_rep:=Representative(c);
      guys:=Image(isoinverse,c_rep);
      Add(schutzconjclassrep,guys);
      Add(c_list,guys);
    od;
    inneruseful:=[e,iso,schutz_h_class,c_list];
    ccl:=schutz_h_class;
    SetConjugacyClasses(schutz_h, ccl);
    tbl:=CharacterTable(schutz_h);
    Add(listgroupchars, tbl);
    Add(useful, inneruseful);
  od;

  GroupCharacterMatrices:=[];;

  # In this loop, we create a list of group character matrices. Note that the
  # CharacterTable function returns a data structure, and we're extracting the
  # matrix from it, row by row.

  for tbl in listgroupchars do
    x:=IdentificationOfConjugacyClasses( tbl );
    innergpmatrix:=[];
    l:=Length(x);
    for i in [1..l] do
      innerrow:=[];
      for j in [1..l] do
        Add(innerrow, Irr(tbl)[i][Position(x,j)]);
      od;
      Add(innergpmatrix,innerrow);
    od;
    Append(GroupCharacterMatrices,innergpmatrix);
  od;

  t:=Length(GroupCharacterMatrices); p:=0; C:=[];

  # In this loop, we make the above list into a block matrix having the group
  # character tables as blocks, by placing zeros in the appropriate
  # places. 

  for r in [1..t] do
    l:=Length(GroupCharacterMatrices[r]);
    innerrow:=[];
    for i in [1..t] do
      if i <= p or i>(p+l) then 
        Add(innerrow, 0);
      else 
        Add(innerrow, GroupCharacterMatrices[r][i-p]);
      fi;
    od;
    Add(C,innerrow);
    if r=(p+l) then 
      p:=p+l;
    fi;
  od;

  gps:=[];

  conjugators:=[];

  # Now we compute the conjugators for the idempotent elements of our semigroup
  # S. The conjugator of an H-class conjugates the H-class to the one chosen in
  # the "useful" list. We also compute the H-classes for the idempotents of S
  # and put them in a list. 

  for e in Idempotents(S) do
    h:=HClass(S,e);
    Add(gps,h);
    ll:=LClass(S,e);
    hh:=HClassReps(ll);
    for y in useful do
      for hrep in hh do
        if y[1]/y[1]=hrep/hrep then
          Add(conjugators,hrep);
          break;
        fi;
      od;
    od;
  od;

  gp_below:=[]; 

  # In this loop, we compute all the group elements that are less than each of
  # the elements of our Schutzenberger group conjugacy class representatives.
  # Apparently the Elements command works faster than working with h directly,
  # but there seems to be space issues. This seems to be the slowest part of the
  # algorithm. 
  
  for s in schutzconjclassrep do
    innerfilter:=[];
    for h in gps do
      x:=RestrictedPartialPerm(s, DomainOfPartialPerm(Representative(h)));
      if x in h then 
        Add(innerfilter, [x]); 
      else
        Add(innerfilter, []);
      fi; 
      #Add(innerfilter,Filtered(Elements(h), x-> NaturalLeqPartialPerm(x, s)));
    od;
    Add(gp_below, innerfilter);
  od;

  conjgp_below:=[];;

  # Here we convert the group elements to their conjugacy class
  # representatives. We need this to build the "Class Incidence Matrix". 

  for x in gp_below do
    innerconjrep:=[];
    for i in [1..Length(x)] do
      h:=x[i];
      p:=conjugators[i];
      for g in h do
        for y in useful do
          if p*(g/p) in Source(y[2]) then
            for k in [1..Length(y[3])] do
              if Image(y[2],p*(g/p)) in y[3][k] then
                Add(innerconjrep,y[4][k]);
                break;
              fi;
            od;
            break;
          fi;
        od;
      od;
    od;
    Add(conjgp_below,innerconjrep);
  od;

  ClassIncidenceMatrix:=[];

 # Now we create the transpose of the Class Incidence Matrix, row by row.

  for x in conjgp_below do
    innerrow:=[];
    for y in schutzconjclassrep do
      Add(innerrow,Length(Positions(x,y)));
    od;
    Add(ClassIncidenceMatrix,innerrow);
  od;

  A:=TransposedMat(ClassIncidenceMatrix);;

  # multiplying the last two matrices gives our character table.
  return [C*A, schutzconjclassrep];
end;


#

InstallMethod(IsGreensDLeq, "for an inverse op acting semigroup",
[IsActingSemigroupWithInverseOp], 
function(S)
  local partial, o, comp_index;
  
  partial:=PartialOrderOfDClasses(S);
  o:=LambdaOrb(S);

  comp_index:=function(x, y)
    if y in partial[x] then
      return true;
    elif Length(partial[x])=1 and partial[partial[x][1]]=partial[x] then
      return false;
    fi;
    return ForAny(partial[x], z-> z<>x and comp_index(z,y));
  end;

  return function(x,y)
    return comp_index(OrbSCCLookup(o)[Position(o, LambdaFunc(S)(x))]-1,
      OrbSCCLookup(o)[Position(o, LambdaFunc(S)(y))]-1);
  end;
end);

#

InstallMethod(PrimitiveIdempotents, 
"for an acting semigroup with inverse op and generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(s)
  local o, scc, rank, min, l, min2, m;
  
  o:=LambdaOrb(s);      scc:=OrbSCC(o);
  rank:=LambdaRank(s);  min:=ActionDegree(s);

  if MultiplicativeZero(s)=fail then 
    for m in [2..Length(scc)] do 
      l:=rank(o[scc[m][1]]);
      if l<min then 
        min:=l;
      fi;
    od;
    return Idempotents(s, min);
  else
    for m in [2..Length(scc)] do 
      l:=rank(o[scc[m][1]]);
      if l<min then 
        min2:=min; min:=l;
      fi;
    od;
    return Idempotents(s, min2);
  fi;
end);

#

InstallMethod(PrimitiveIdempotents, 
"for an acting semigroup with inverse op and generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(s)
  local o, scc, rank, min, l, min2, m;
  
  o:=LambdaOrb(s);      scc:=OrbSCC(o);
  rank:=LambdaRank(s);  min:=ActionDegree(s);

  if MultiplicativeZero(s)=fail then 
    for m in [2..Length(scc)] do 
      l:=rank(o[scc[m][1]]);
      if l<min then 
        min:=l;
      fi;
    od;
    return Idempotents(s, min);
  else
    for m in [2..Length(scc)] do 
      l:=rank(o[scc[m][1]]);
      if l<min then 
        min2:=min; min:=l;
      fi;
    od;
    return Idempotents(s, min2);
  fi;
end);

#

InstallMethod(IsJoinIrreducible, 
"for an inverse semigroup of partial perms and a partial perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(S, x)
  local y, elts, i, k, singleline, sup, j;

  if not x in S then
    Error("Second argument should be a partial perm within first argument");
  fi;
  
  if IsMultiplicativeZero(S, x) then 
    return false; 
  fi;

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
    if  NaturalLeqPartialPerm(elts[j], elts[i]) and
    not NaturalLeqPartialPerm(elts[j], elts[k]) then 
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

InstallMethod(IsMajorantlyClosed, 
"for an inverse subsemigroup of partial perms and an inverse subsemigroup",
[IsInverseSemigroup and IsPartialPermSemigroup,
IsInverseSemigroup and IsPartialPermSemigroup],
function(S, T)
  return IsMajorantlyClosed(S, Elements(T));
end);

#

InstallMethod(IsMajorantlyClosed, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection],
function(S, T)
  if not IsSubset(S,T) then
    Error("The second argument should be a subset of the first");
  else
    return IsMajorantlyClosedNC(S,T);
  fi;
end);

#

InstallMethod(IsMajorantlyClosedNC, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection],
function(S, T)
  local i, iter, t, u;

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

InstallMethod(MajorantClosure, 
"for an inverse subsemigroup of partial perms and an inverse subsemigroup",
[IsInverseSemigroup and IsPartialPermSemigroup,
IsInverseSemigroup and IsPartialPermSemigroup],
function(S, T)
  return MajorantClosure(S, Elements(T));;
end);

#

InstallMethod(MajorantClosure, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection],
function(S, T)
  if not IsSubset(S,T) then
    Error("The second argument should be a subset of the first");
  else
    return MajorantClosureNC(S,T);
  fi;
end);

#

InstallMethod(MajorantClosureNC, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection],
function(S, T)
  local elts, n, out, ht, k, val, t, i;
	
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

#

InstallMethod(RightCosetsOfInverseSemigroup, 
"for an inverse semigroup of partial permutations and an inverse subsemigroup",
[IsInverseSemigroup and IsPartialPermSemigroup,
IsInverseSemigroup and IsPartialPermSemigroup],
function(S, T)
  local elts, idem, usedreps, out, dupe, coset, s, rep, t;
  
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

InstallMethod(SameMinorantsSubgroup, 
"for a group H-class of an inverse semigroup of partial perms",
[IsGroupHClass and IsPartialPermCollection],
function(h)
  local e, F, out, i;

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

InstallMethod(SmallerDegreePartialPermRepresentation, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  local oldgens, newgens, D, e, He, sigma, sigmainv, schutz, sup, trivialse, psi, psiinv, rho, rhoinv, orbits, cosets, stabpp, stab, h, nrcosets, j, reps, lookup, gen, offset, rep, box, subbox, T, d, i, k, m;
          
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
      
      # Generate representatives for ALL the cosets the generator will act
      #on  #### Divide every H-Class in the R-Class into 'cosets' like stab in
      #He
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
               AsPermutation((rep*h[box]^(-1))));
            fi;
            Add(newgens[j], (box-1)*Length(cosets)+subbox+offset);  
          fi;
        od;
      od; 
    od;        
  od;


  T:=InverseSemigroup(List(newgens, x->PartialPermNC(x)));

  # Return identity mapping if nothing has been accomplished; else the result.
  if NrMovedPoints(T) > NrMovedPoints(S)
    or (NrMovedPoints(T) = NrMovedPoints(S) 
        and ActionDegree(T) >= ActionDegree(S)) then
    return IdentityMapping(S);
  else
    return MagmaIsomorphismByFunctionsNC(S, T,
      x -> EvaluateWord(GeneratorsOfSemigroup(T), Factorization(S, x)),
      x -> EvaluateWord(GeneratorsOfSemigroup(S), Factorization(T, x)));
  fi;
  
end);

#

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

InstallMethod(VagnerPrestonRepresentation, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)
  local gens, elts, out, iso, T, inv, i;

  gens:=GeneratorsOfSemigroup(S);
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
  inv:=x-> EvaluateWord(GeneratorsOfSemigroup(S), Factorization(T, x));

  return MagmaIsomorphismByFunctionsNC(S, T, iso, inv);
end);

#



#EOF
