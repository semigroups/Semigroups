
momorphism of a group H Class (i.e. inverse semigroup He) into an inverse semigroup(group) with the same minorants of e, acting faithfully on Se. ###
### Function returns a 'mapping', haven't told gap this is a homomorphism. ###

SameMinorantHomomorphismOfGroupHClass:=function(H)
  local e, F, N, Y, Yc, S, f;

  # Argument should be a group H class
        
  if not IsGroupHClass(H) then 
    Error("usage: the argument should be a group H-class,"); 
    return;
  fi;

  e:=Representative(H);
  F:=[];
      
  # Find the minorants of e
  for f in Idempotents(ParentAttr(H)) do
    if NaturalLeqPP(f, e) and f<>e then
      Add(F, f);
    fi;
  od;
      
  N:=DomPP(e);
  Y:=Union(List(F, f->DomainOfPartialPerm(f)));
  Y:=Set(Y);
  Yc:=SetDifference(N,Y)

  H:=Elements(H);
  S:=InverseSemigroup(H);

  return MagmaHomomorphismByFunctionsNC(S,
   SymmetricInverseSemigroup(DegreeOfPartialPermSemigroup(S)),
   x -> RestrictedPartialPermNC(x,Yc)); 
end;

############ A modification of earlier function which is more successful in returning smaller and sometimes minimal degrees #################
### Needs optimised in terms of speed. Hopefully no big bugs left. ###

SmallerDegreePartialPermRepLong:= function(S)

  local out, D, e, h, i, j, k, m, lookup, box, subbox,
        H, Fei, He, Se, sigma, HeSigma, FeiSigma, sigmainv, phi, HePhi, psi, HePhiPsi, rho, HePhiPsiRho, phipsirhoinv,
        oldgens, newgens, gen, offset,
        orbits, cosets, HeCosetReps, HeCosetRepsSigma, AllCosetReps, rep, numcosets, CosetsInHe, trivialse;
        
  out:=[];
  oldgens:=Generators(S);
  newgens:=[];
  for i in [1..Length(oldgens)] do newgens[i]:=[]; od;
  
  D:=JoinIrreducibleDClasses(S);

  for e in D do
                                
    ##### Calculate He as a small permutation group #####       
        H:=GroupHClass(e);
    trivialse:=HasTrivialSe(H);
    He:=InverseSemigroup(Elements(H), rec(small:=true));
        
        
    # If Se is trivial, we have a special simpler case    
    if trivialse then
      orbits:=[[1]];
      HeCosetReps:=[Representative(e)];
      Fei:=He;
    else 
        # Approximate the minimal degree of Se in He (by phi & rho)
                phi:=SameMinorantHomomorphismOfGroupHClass(H);
                HePhi:=Range(phi);     

                psi:=IsomorphismPermGroup(HePhi);
                HePhiPsi:=Range(psi);
                
                rho:=SmallerDegreePermutationRepresentation(HePhiPsi);
                HePhiPsiRho:=Range(rho);     

                phipsirhoinv:=InverseGeneralMapping(phi*psi*rho);

                orbits:=Orbits(HePhiPsiRho);

        fi;

    for i in orbits do
    
      if not trivialse then

        # Generate Fei
        Fei:=ImagesSet(phipsirhoinv, Stabiliser(HePhiPsiRho,i[1]));
                
        # Generate reps for the cosets of Fei in He
                sigma:=IsomorphismPermGroup(He);
                sigmainv:=InverseGeneralMapping(sigma);
                HeSigma:=Range(sigma);
        FeiSigma:=ImagesSet(sigma, Fei);
                # Turn FeiSigma into a group to avoid RightTransversal failures. This method can perhaps be improved
                FeiSigma:=Group(FeiSigma);
                
        HeCosetRepsSigma:=RightTransversal(HeSigma, FeiSigma);
        HeCosetReps:=[];
        for j in [1..Size(HeCosetRepsSigma)] do
          Add(HeCosetReps, HeCosetRepsSigma[j]^sigmainv);
        od;
      
      fi; 

      # Generate reps for the HClasses in the RClass of e
      h:=HClassReps( RClassNC(e, Representative(e)) );
      CosetsInHe:=Length(HeCosetReps);
      numcosets:=Size(h)*CosetsInHe;
      
      # Generate reps for ALL the cosets that the generator will act on      
      j:=0;
      AllCosetReps:=[];
      lookup:=EmptyPlist(Length(e!.o));
      for k in [1..Size(h)] do
        lookup[Position(e!.o, RanSetPP(h[k]))]:= k;
        for m in [1..Length(HeCosetReps)] do
          j:=j+1;
          AllCosetReps[j]:=HeCosetReps[m]*h[k];
        od;
      od;
                        
      # Loop over the old generators of S to find action on cosets
      for j in [1..Length(oldgens)] do

        gen:=oldgens[j];
        offset:=Length(newgens[j]);

        # Loop over cosets to calculate image of each under gen
        for k in [1..numcosets] do

          rep:=AllCosetReps[k]*gen;

          # Will the new generator will be defined at this point?
          if not rep*rep^(-1) in Fei then
            Add(newgens[j], 0);
          else
            box:=lookup[Position(e!.o, RanSetPP(rep))];
            if trivialse then
              subbox:=1;
            else
              subbox:=PositionCanonical(HeCosetRepsSigma, (rep*h[box]^(-1))^sigma);
            fi;
            Add(newgens[j], (box-1)*CosetsInHe+subbox+offset);  
          fi;

        od;                                                                             
      od; 
    od;        
  od;

  out:=InverseSemigroup(List(newgens, x->PartialPermNC(x)));

  # Check whether work has actually been done
  if NrMovedPoints(out) > NrMovedPoints(S) or (NrMovedPoints(out) = NrMovedPoints(S) and Degree(out) >= Degree(S)) then
    return S;
  else
    return out;
  fi;

end;
