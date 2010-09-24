
InstallMethod(IsomorphismTransformationSemigroup, "for a perm. group", 
[IsPermGroup],
function(g)
local dom;
dom:=[1..LargestMovedPoint(g)];
return Semigroup(List(GeneratorsOfGroup(g), x-> 
 TransformationNC(OnTuples(dom, x))));
# JDM could add more of the info known about g here!

end);

###########################################################################

InstallMethod(IsomorphismAutomorphismGroupOfRMS, "for the aut. group of a simple semigroup", true, [IsAutomorphismGroupOfSimpleSemigp], 0, 
function(G)
local S, iso, H, hom;

S:=Source(One(G));
iso:=IsomorphismReesMatrixSemigroup(S);
H:=AutomorphismGroup(Range(iso));

return GroupHomomorphismByImagesNC(G, H, GeneratorsOfGroup(G), SmallGeneratingSet(H));
end);

###########################################################################

InstallMethod(IsomorphismFpSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
function(S)

if IsMonoid(S) then 
  Error("<S> must NOT be a monoid");
else
  FroidurePinExtendedAlg(S);
  return IsomorphismFpSemigroup(S);
fi;
end);

###########################################################################

InstallOtherMethod(IsomorphismFpMonoid, "for a transformation monoid", 
[IsTransformationMonoid],
function(S)
FroidurePinExtendedAlg(S);
return IsomorphismFpMonoid(S);
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a zero group and zero group", 
[IsZeroGroup, IsZeroGroup], 
function(ZG1, ZG2)
local G1, G2, iso, imgs;

if ZG1=ZG2 then 
  return IdentityMapping(ZG1);
else

  G1:=UnderlyingGroupOfZG(ZG1);
  G2:=UnderlyingGroupOfZG(ZG2);
  iso:=IsomorphismGroups(G1, G2);

  if not iso=fail then #JDM could be some ordering problem here
    imgs:=OnTuples(List(GeneratorsOfMonoid(ZG1), 
           UnderlyingGroupEltOfZGElt){[1..Length(GeneratorsOfMonoid(ZG1))-1]}, iso);

  return SemigroupHomomorphismByImagesOfGensNC(ZG1, ZG2, Concatenation(
          List(imgs, ZeroGroupElt), [MultiplicativeZero(ZG2)]));
  fi;
fi;

return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RMS and RZMS", true, [IsReesMatrixSemigroup, IsReesZeroMatrixSemigroup], 0,
function(S1, S2)

return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RZMS and RMS", true, [IsReesZeroMatrixSemigroup, IsReesMatrixSemigroup], 0,
function(S1, S2)

return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RMS and ZS", true, [IsReesMatrixSemigroup, IsZeroSemigroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a ZS and RMS", true, [IsZeroSemigroup, IsReesMatrixSemigroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a ZS and ZS", true, [IsZeroSemigroup, IsZeroSemigroup], 0,
function(S1, S2)

if S1=S2 then 
  return IdentityMapping(S1); 
else 
  return fail;
fi;
# if they have the same number of elements, they are the same

end);

#############################################################################
#JDM new for 3.2! Install similar for RMS and change IsomorphismSemigroups
# accordingly!

InstallMethod(IdentityMapping, "for a RZMS", [IsReesZeroMatrixSemigroup], 
function(rms)
local g,m,n;

g:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms); 
m:=ColumnsOfReesZeroMatrixSemigroup(rms);
n:=RowsOfReesZeroMatrixSemigroup(rms);

return RZMSIsoByTriple(rms, rms, [(), IdentityMapping(g), List([1..m+n], 
 x-> One(g))]);
end);

#############################################################################

InstallMethod(IsomorphismSemigroups, "for a RMS and RMS", true, [IsReesMatrixSemigroup, IsReesMatrixSemigroup], 0,
function(R1, R2)
local g1, g2, iso, G1, G2, isograph, isogroup, f, l, g, tup, candidate, mat, m, n;

if not (Size(R1)=Size(R2) and ColumnsOfReesMatrixSemigroup(R1)=ColumnsOfReesMatrixSemigroup(R2) and RowsOfReesMatrixSemigroup(R1)=RowsOfReesMatrixSemigroup(R2)) then 
  return fail;
else

	mat:=SandwichMatrixOfReesMatrixSemigroup(R1);
	m:=Length(mat[1]); n:=Length(mat);

	if R1=R2 then 

		g:=UnderlyingSemigroupOfReesMatrixSemigroup(R1); 
		return RMSIsoByTriple(R1, R2, [(), IdentityMapping(g), List([1..m+n], 
                         x-> One(g))]);

	else
	
		g1:=UnderlyingSemigroupOfReesMatrixSemigroup(R1);
		g2:=UnderlyingSemigroupOfReesMatrixSemigroup(R2);
		iso:=IsomorphismGroups(g1, g2);

  #for RMS without 0 the graphs are always isomorphic, being complete bipartite.

		if iso=fail then 
			return fail;
  	else 
			isograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
			
			#all isomorphisms from g1 to g2
			isogroup:=List(Elements(AutomorphismGroup(g1)), x->x*iso); 
  
			#find an induced function, if there is one
			for l in isograph do
				for g in isogroup do
					for tup in Elements(g2) do 
						candidate:=RMSInducedFunction(R2, l, g, tup); 
						if not candidate[1]=false then 
							return RMSIsoByTriple(R1, R2, [l, g, candidate[2]]);
						fi;
					od;
				od;
			od;

			return fail;
		fi;
	fi;
fi;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups, "for a RZMS and RZMS",
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup],
function(R1, R2)

Print("#I It appears that the `grape' package is not fully installed.",  
 "As a\n#I consequence this function is not available.\n");
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a group and RZMS", true, [IsGroup, IsReesZeroMatrixSemigroup], ReturnFail);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RZMS and group", true, [IsReesZeroMatrixSemigroup, IsGroup], ReturnFail);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a group and zero semigroup", true, [IsGroup, IsZeroSemigroup], ReturnFail);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a zero semigroup and group", true, [IsZeroSemigroup, IsGroup], ReturnFail);

###########################################################################

InstallMethod(IsomorphismReesMatrixSemigroupOfDClass, 
"for a Green's D-class", [IsGreensDClass and IsGreensClassOfTransSemigp],
function(D)
local g, zg, rep, r, l, rreps, lreps, mat, func, rms, invlreps, invrreps, Q, R, 
hom, invfunc, RMS;

if  IsRegularDClass(D) then 
	g:= Range(IsomorphismPermGroup(GroupHClassOfGreensDClass(D)));
	zg:=ZeroGroup(g);
	rep:=Representative(GroupHClassOfGreensDClass(D));

	r:=GreensRClassOfElement(ParentAttr(D), rep);
	l:=GreensLClassOfElement(ParentAttr(D), rep);

	rreps:=List(GreensHClasses(l), Representative);
	lreps:=List(GreensHClasses(r), Representative);
	
	if Length(lreps)=1 and Length(rreps)=1 then #it is a group
		rms:=g;
		func:=AsPermOfRange;
		invfunc:=x-> IdempotentNC(KernelOfTransformation(rep), ImageSetOfTransformation(rep))*x;
		#hom:=SemigroupHomomorphismByFunctionNC(D, g, AsPermOfRange);
		#return SemigroupHomomorphismByFunctionNC(D, g, AsPermOfRange);
	else

		RMS:=true;
	
		mat:=List(lreps, x-> List(rreps, function(y)
			if x*y in D then 
				return AsPermOfRange(x*y);
			else
				RMS:=false;
				return MultiplicativeZero(zg);
			fi; end));

		if RMS then 
			rms:=ReesMatrixSemigroup(g, mat);
	
			func:=function(d)
				local col, row;
				col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)=ImageSetOfTransformation(x));
				row:=PositionProperty(rreps, x-> KernelOfTransformation(d)=KernelOfTransformation(x));    
				return ReesMatrixSemigroupElementNC(rms, row,
					AsPermOfRange(lreps[col]*rreps[row])^-1*
					AsPermOfRange(lreps[col]*d*rreps[row])*
					AsPermOfRange(lreps[col]*rreps[row])^-1, col);
			end;
		
			invfunc:=function(rmselt)
			local i,a,lambda;

			i:=RowIndexOfReesMatrixSemigroupElement(rmselt);
			a:=UnderlyingElementOfReesMatrixSemigroupElement(rmselt);
			lambda:=ColumnIndexOfReesMatrixSemigroupElement(rmselt);

			return rreps[i]*a*lreps[lambda];
			end;

		else

		#find inverses for rreps and lreps
	
		#JDM is there an easier way? 
		Q:=List([1..Length(rreps)], x-> PositionProperty( List(mat, y-> y[x]), z-> not z=MultiplicativeZero(zg)));
		invrreps:=List([1..Length(rreps)], x-> mat[Q[x]][x]^-1*lreps[Q[x]]);

		R:=List([1..Length(lreps)], x-> PositionProperty(mat[x], y-> not y=MultiplicativeZero(zg)));
		invlreps:=List([1..Length(lreps)], x-> rreps[R[x]]*mat[x][R[x]]^-1);

		mat:=List(mat, x-> List(x, function(y)
			if not y=MultiplicativeZero(zg) then 
				return ZeroGroupElt(y);
			fi;
			return y; end));

		rms:=ReesZeroMatrixSemigroup(zg, mat);

		func:=function(d)
		local col, row;
	
		col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)=ImageSetOfTransformation(x));
		if not col=fail then 
			row:=PositionProperty(rreps, x-> KernelOfTransformation(d)=KernelOfTransformation(x));    

			return ReesZeroMatrixSemigroupElementNC(rms, row, ZeroGroupElt(AsPermOfRange(invrreps[row]*d*invlreps[col])), col);
		fi;
	
		return MultiplicativeZero(rms);
		end;
		
		invfunc:=function(rmselt)
		local i,a,lambda;
		
		if rmselt=MultiplicativeZero(zg) then
			Error("the multiplicative zero has no preimage");
		fi;
		
		i:=RowIndexOfReesZeroMatrixSemigroupElement(rmselt);
		a:=UnderlyingGroupEltOfZGElt(UnderlyingElementOfReesZeroMatrixSemigroupElement(rmselt));
		lambda:=ColumnIndexOfReesZeroMatrixSemigroupElement(rmselt);

		return rreps[i]*a*lreps[lambda];
		end;
	fi;
fi;
else	#it's not a regular D-class
			#and so it's a zero semigroup
	 
	rms:=ZeroSemigroup(Size(D)+1);

	func:=function(x)
		if x in D then 
			return Elements(rms)[Position(Elements(D), x)+1];
		else
			return MultiplicativeZero(rms);
		fi;
	end;
	
	invfunc:=function(x)
		local str;
		
		if x=MultiplicativeZero(rms) then
			Error("the multiplicative zero has no preimage");
		fi;
		
		#str:=String(x);
		#return Elements(D)[Int(str{[2..Length(str)]})];
		return Elements(D)[x![1]];
	end;

fi;

hom:=SemigroupHomomorphismByFunctionNC(D, rms, func);
SetInverseGeneralMapping(hom, 
       SemigroupHomomorphismByFunctionNC(rms, D, invfunc));
SetIsInjective(hom, true);
SetIsSurjective(hom, true);
SetIsTotal(hom, true);
if not HasIsZeroSemigroup(rms) then 
	SetIsZeroSemigroup(rms, false);
fi;

return hom;

end);

#############################################################################
## JDM it might be possible to improve this after making improvements to the 
## JDM the way in which MONOID handles Green's relations of completely simple
## JDM semigroups.

InstallMethod(IsomorphismReesMatrixSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
function(S)
local g, rep, r, l, rreps, lreps, mat, func, rms, D, img, ker, hom,
	invfunc;

if IsCompletelySimpleSemigroup(S) #and not IsGroupAsTransSemigroup(S) 
	then 

  D:=GreensDClassOfElement(S, Representative(S));
  g:= Range(IsomorphismPermGroup(GroupHClassOfGreensDClass(D)));
    
  rep:=Representative(GroupHClassOfGreensDClass(D));

  r:=GreensRClassOfElement(S, rep);
  l:=GreensLClassOfElement(S, rep);
  img:=ImageSetOfTransformation(Representative(l));
  ker:=KernelOfTransformation(Representative(r));

  rreps:= List(List(GreensHClasses(l), x-> KernelOfTransformation
           (Representative(x))), y-> IdempotentNC(y, img)); 
  lreps:= List(List(GreensHClasses(r), x-> ImageSetOfTransformation
           (Representative(x))), y-> IdempotentNC(ker, y)); 

  mat:=List(lreps, x-> List(rreps, y-> AsPermOfRange(x*y)));

  rms:=ReesMatrixSemigroup(g, mat);
  
  #JDM this could be speeded up :)
  func:=function(d)
    local col, row;
    col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)
		=ImageSetOfTransformation(x));
    row:=PositionProperty(rreps, x-> KernelOfTransformation(d)
		=KernelOfTransformation(x));    

    return ReesMatrixSemigroupElementNC(rms, row, 
	    AsPermOfRange(lreps[col]*rreps[row])^-1*
            AsPermOfRange(lreps[col]*d*rreps[row])*
            AsPermOfRange(lreps[col]*rreps[row])^-1, col);
    end;
   
    invfunc:=function(rmselt)
      local i,a,lambda;

      i:=RowIndexOfReesMatrixSemigroupElement(rmselt);
      a:=UnderlyingElementOfReesMatrixSemigroupElement(rmselt);
      lambda:=ColumnIndexOfReesMatrixSemigroupElement(rmselt);

      return rreps[i]*a*lreps[lambda];
    end;

    hom:=SemigroupHomomorphismByFunctionNC(S, rms, func);
   
    SetInverseGeneralMapping(hom, 
       SemigroupHomomorphismByFunctionNC(rms, S, invfunc));
    SetIsInjective(hom, true);
    SetIsSurjective(hom, true);
    SetIsTotal(hom, true);
    
    return hom;
  #elif IsGroupAsTransSemigroup(S) then
	#	#JDM should there be split cases here for semigroups and monoids
	#	
  #	G:=Group(List(GeneratorsOfSemigroup(S), x-> AsPermOfRange(x)));
	#	hom:=SemigroupHomomorphismByFunctionNC(S, G, AsPermOfRange);
	#	SetInverseGeneralMapping(hom, SemigroupHomomorphismByFunctionNC(G, S, x->Idempotents(S)[1]*x));
	#	SetIsBijective(hom, true); SetIsTotal(hom, true);
	#	return hom;
		
  else
    Error("<S> must be a completely simple semigroup");
  fi;
end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for an auto. gp. of a simple semigp", true, [IsAutomorphismGroupOfSimpleSemigp], 0, 
function(A)
local iso;

iso:=IsomorphismAutomorphismGroupOfRMS(A);
return iso*IsomorphismPermGroup(Range(iso));

end);

###########################################################################
##JDM this could certainly be improved...

InstallOtherMethod(IsomorphismPermGroup, "for an automorphism group of a RMS", true, [IsAutomorphismGroupOfRMS], 0, 
function(A)
local RMS, permgp, a, perm, hom, i;

RMS:=Source(One(A));
permgp:=Group(());
SetAsSSortedList(permgp, [()]);
	
if Size(RMS)< Size(A) then 
	i:=0;
	repeat
		i:=i+1;
	#for a in [1..Size(A)/2+1] do
		a:= AsSSortedList(A)[i];
		perm:=PermListList([1..Size(RMS)], List([1..Size(RMS)], x-> Position(AsSSortedList(RMS), ImageElm(a, AsSSortedList(RMS)[x]))));
		if not perm in permgp then 
			permgp:=ClosureGroupDefault(permgp, perm); 
		fi;
	#od;
	until Size(permgp)=Size(A);

	#JDM better not to take SmallGeneratingSet of permgp here since
	#JDM the original generators of permgp will be easier to find in A
	
	hom:=GroupHomomorphismByFunction(A, permgp, x-> PermListList([1..Size(RMS)], List([1..Size(RMS)], y-> Position(AsSSortedList(RMS), ImageElm(x, AsSSortedList(RMS)[y])))));
	#JDM need the inverse of hom
	
	#hom:=GroupHomomorphismByImagesNC(A, permgp, GeneratorsOfGroup(A), List(GeneratorsOfGroup(A), x-> PermListList([1..Size(RMS)], List([1..Size(RMS)], y-> Position(AsSSortedList(RMS), ImageElm(x, AsSSortedList(RMS)[y])))))); 
	
	#SetInverseGeneralMapping(hom, GroupHomomorphismByImagesNC(permgp, A, GeneratorsOfGroup(permgp), List(GeneratorsOfGroup(permgp), x-> First(A, y-> ImageElm(hom, y)=x))));

	#JDM need the inverse of hom
	
	SetIsBijective(hom, true);

	return hom;

else 
	hom := ActionHomomorphism( A, A, OnRight, "surjective" );
	SetIsBijective( hom, true );
	return hom;
fi;

end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for an automorphism group of a RZMS", true, [IsAutomorphismGroupOfRZMS], 0, 
function(A)
local RZMS, permgp, a, perm, hom, i;

RZMS:=Source(One(A));
permgp:=Group(());
SetAsSSortedList(permgp, [()]);
	
if Size(RZMS)<Size(A) then 
	i:=0;
	repeat
		i:=i+1;
		a:= AsSSortedList(A)[i];
		perm:=PermListList([1..Size(RZMS)], List([1..Size(RZMS)], x-> Position(AsSSortedList(RZMS), ImageElm(a, AsSSortedList(RZMS)[x]))));
		if not perm in permgp then 
			permgp:=ClosureGroupDefault(permgp, perm); 
		fi;
	until Size(permgp)=Size(A);
	
	hom:=GroupHomomorphismByFunction(A, permgp, x-> PermListList([1..Size(RZMS)], List([1..Size(RZMS)], y-> Position(AsSSortedList(RZMS), ImageElm(x, AsSSortedList(RZMS)[y])))));
	
	#JDM need the inverse of hom, see comments in IsomorphismPermGroup of RMS
	
	SetIsBijective(hom, true);

	return hom;

else 
	hom := ActionHomomorphism( A, A, OnRight, "surjective" );
	SetIsBijective( hom, true );
	return hom;
fi;

end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for an H-class of a transformation semigroup", true, 
[IsGreensHClass and IsGreensClassOfTransSemigp], 0,
function(hc)
local elms, gp, mapfun;

if not IsGroupHClass( hc )  then
   Error( "can only create isomorphism of group H-classes" );
fi;

elms:=AsSSortedList( hc ); 
gp:=Group(List(elms, AsPermOfRange));
mapfun := x-> AsPermOfRange(x); 

return 	SemigroupHomomorphismByFunctionNC(hc, Group(SmallGeneratingSet(gp)), mapfun );

end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,
function(S)
local elms, gens, mapfun;

if not IsGroupAsSemigroup(S)  then
   Error( "can only create isomorphism groups for semigroups that are groups" );
fi;

gens:=List(GeneratorsOfSemigroup(S), AsPermOfRange);
mapfun := x-> AsPermOfRange(x); 

return 	SemigroupHomomorphismByFunctionNC(S, Group(gens), mapfun );

end);


###########################################################################
##
#M	IsomorphismSemigroups( <RZMS>, <ZS> );
#M	IsomorphismSemigroups( <ZS>, <RZMS> );
##	
##	JDM not yet implemented. also add method for RMS and group

#InstallOtherMethod(IsomorphismSemigroups,  "for a RZMS and ZS", true, 
#[IsReesZeroMatrixSemigroup, IsZeroSemigroup], 0,
#function(S1, S2)
#
#if ForAll(SandwichMatrixOfReesZeroMatrixSemigroup(S1), 
#          row -> ForAll(row, x-> x=MultiplicativeZero(
#              UnderlyingSemigroupOfReesMatrixSemigroup(S1)))) 
#   and Size(S1)=Size(S2) then
#
#   return SemigroupHomomorphismByImagesOfGensNC(S1, S2, List([1..Length
# (GeneratorsOfSemigroup(S1))], x-> AsList(S2)[x]);
#
#end);

#EOF

