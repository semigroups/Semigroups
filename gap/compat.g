##
## compat.g
## Version 3.1.1
## Mon Jun  9 09:26:11 BST 2008
##


##  This file contains methods that should have been in the GAP library at the 
##  last release but were not.

###########
#semirel.gd
###########

DeclareProperty("IsFpSemigpReducedElt", IsElementOfFpSemigroup);
DeclareProperty("IsFpMonoidReducedElt", IsElementOfFpMonoid);

###########
#semirel.gi
###########

InstallMethod(FroidurePinExtendedAlg, "for a semigroup", true, [IsSemigroup and HasIsFinite and IsFinite], 99,
function(mono)

local gens, k, free, freegens, actualelts, fpelts, rules, i, u, v, Last, currentlength, b, s, r, newelt, j, p, new, length, newword, first, final, prefix, suffix, next, postmult, reducedflags, premult, fpsemi, old, sortedelts, pos, semi, perm, free2, fam, t;

if not IsMonoid(mono) then 
	semi := MonoidByAdjoiningIdentity(mono);
else
	semi:=mono;
fi;

#JDM gens:=Set(GeneratorsOfMonoid(semi));
gens:=Set(Filtered(GeneratorsOfMonoid(semi), x-> not IsOne(x)));
k:=Length(gens);
free:=FreeMonoid(k);
freegens:=GeneratorsOfMonoid(free);
actualelts:=Concatenation([One(semi)], gens);
fpelts:=Concatenation([One(free)], freegens);
sortedelts:=List(Concatenation([One(semi)], gens));

#output

Sort(sortedelts);
rules:=[];
pos:=List([1..k+1], x-> Position(actualelts, sortedelts[x]));

# table containing all data
# for a word <u>

# position of first letter in <gens>
first:=Concatenation([fail], [1..k]); 	
# position of last letter in <gens> 
final:=Concatenation([fail], [1..k]); 	   
# position of prefix of length |u|-1 in <fpelts>
prefix:=Concatenation([fail], List([1..k], x->1));    
# position of suffix of length |u|-1 in <fpelts>
suffix:=Concatenation([fail], List([1..k], x->1)); 
# position of u*freegens[i] in <fpelts>
postmult:=Concatenation([[2..k+1]], List([1..k], x-> []));  
# true if u*freegens[i] is the same word as fpelts[i] 
reducedflags:=Concatenation([List([1..k], x->  true)], List([1..k], x-> [])); 
# position of freegens[i]*u in <fpelts>
premult:=Concatenation([[2..k+1]],  List([1..k], x-> [])); 	
# length of <u>
length:=Concatenation([0], List([1..k], x->1));

# initialize loop

u:=2;							# position of the first generator
v:=u;							# place holder
Last:=k+1;				# the current position of the last element in <fpelts>
currentlength:=1;	# current length of words under consideration

# loop

repeat
  
  while u<=Last and length[u]=currentlength do
  
    b:=first[u];
    s:=suffix[u];
    
    for i in [1..k] do  					#loop over generators
     
      newword:=fpelts[u]*freegens[i];  				# newword=u*a_i
    
      if not reducedflags[s][i] then   				# if s*a_i is not reduced
        r:=postmult[s][i];             				# r=s*a_i
        if fpelts[r]=One(free) then				# r=1    
          postmult[u][i]:=b+1; reducedflags[u][i]:=true;	# u*a_i=b and it is reduced 
        else 	
          postmult[u][i]:=postmult[premult[prefix[r]][b]][final[r]];    	
								#\rho(u*a_i)=\rho(\rho(b*r)*l(r)) 
          reducedflags[u][i]:=(newword=fpelts[postmult[u][i]]); # if \rho(u*a_i)=u*a_i then true
       fi;
      
      else
        
        newelt:=actualelts[u]*gens[i];		# newelt=nu(u*a_i)
	old:=PositionSorted(sortedelts, newelt);
	if old<=Last and newelt=sortedelts[old] then
	  old:=pos[old];
          Add(rules, [newword, fpelts[old]]); 
          postmult[u][i]:=old; reducedflags[u][i]:=false;  # u*a_i represents the same elt as 
							   # fpelts[j] and is (hence) not reduced
        else 
	  Add(fpelts, newword); Add(first, b); Add(final, i);	# add all its info to the table
	  Add(prefix,u); Add(suffix, postmult[suffix[u]][i]); 	# u=b*suffix(u)*a_i
	  Add(postmult, []); Add(reducedflags, []); Add(premult, []);
	  Add(length, length[u]+1); Add(actualelts, newelt); 
 
    	  Last:=Last+1;
          postmult[u][i]:=Last; reducedflags[u][i]:=true;   # the word u*a_i is a new elt 
							    # and is hence reduced
          	  
	  AddSet(sortedelts, newelt);
	  
          COPY_LIST_ENTRIES( pos, old, 1, pos, old+1, 1, Last-old );
	  pos[old] := Last; 

       fi;
     fi; 
   od; 
    
    u:=u+1; 
    
  od;
  u:=v;  # go back to the first elt with length=currentlength
  
  while u<=Last and length[u]=currentlength do
    p:=prefix[u];
    for i in [1..k] do 
      premult[u][i]:=postmult[premult[p][i]][final[u]];  		
		# \rho(a_i*u)=\rho(\rho(a_i*p)*final(u))
    od; 
    u:=u+1; 
  od;

  v:=u;

  currentlength:=currentlength+1;
   
until u=Last+1;

if IsMonoid(mono) then 

   fpsemi:=free/rules;
   fam:=FamilyObj(GeneratorsOfMonoid(fpsemi)[1]);
   fpelts:=List(fpelts, 
   	function(x)
   	local new;
   	new:=ElementOfFpMonoid(fam, x);
   	SetIsFpMonoidReducedElt(new, true);
   	return new; end );
   
   for u in GeneratorsOfSemigroup(fpsemi) do
   	SetIsFpMonoidReducedElt(u, true);
   od;

		#fpelts, function(x)
		#				local new;
		#					new:=MappedWord(x, FreeGeneratorsOfFpMonoid(fpsemi), GeneratorsOfMonoid(fpsemi));
		#					SetIsFpMonoidReducedElt(new, true);
		#					return new;
		#					end);

   SetAsSSortedList(fpsemi, fpelts);
   SetSize(fpsemi, Last);
   SetLeftCayleyGraphSemigroup(fpsemi, premult);
   SetRightCayleyGraphSemigroup(fpsemi, postmult);
   SetAssociatedConcreteSemigroup(fpsemi, semi);

#JDM if KnuthBendixRewritingSystem was an attribute and not an operation 
#JDM it would be possible to set that it is confluent at this point

   perm:=PermListList(pos, [1..Last]);
   premult:=Permuted(OnTuplesTuples(premult, perm), perm);
   postmult:=Permuted(OnTuplesTuples(postmult, perm), perm);

#JDM this step runs GreensRClasses somehow when MONOID is loaded!
   
   SetAsSSortedList(mono, sortedelts);  
   SetSize(mono, Last);
   SetLeftCayleyGraphSemigroup(mono, premult);
   SetRightCayleyGraphSemigroup(mono, postmult);
   SetAssociatedFpSemigroup(mono, fpsemi);

   u:=SemigroupHomomorphismByImagesNC(mono, fpsemi, 
        List(pos, x-> fpelts[x]));
   SetInverseGeneralMapping(u, SemigroupHomomorphismByImagesNC(fpsemi, mono, 
   actualelts));
   SetIsTotal(u, true); SetIsInjective(u, true); 
   SetIsSurjective(u, true); SetIsSingleValued(u, true);
   SetIsomorphismFpMonoid(mono, u);

else

#get rid of the identity! JDM better to do this online?

	free2:=FreeSemigroup(k);

	rules:=List(rules, x-> [MappedWord(x[1], GeneratorsOfMonoid(free), 
           GeneratorsOfSemigroup(free2)), 
           MappedWord(x[2], GeneratorsOfMonoid(free), 
           GeneratorsOfSemigroup(free2))]);

   fpsemi:=free2/rules;

	fpelts:=List(fpelts{[2..Last]}, function(x)
						local new;
							new:=MappedWord(x, GeneratorsOfMonoid(free), GeneratorsOfSemigroup(fpsemi));
							SetIsFpSemigpReducedElt(new, true);
							return new;
							end);
   #fpelts:=List(fpelts, x-> MappedWord(x, 
   #         FreeGeneratorsOfFpSemigroup(fpsemi),
   #         GeneratorsOfSemigroup(fpsemi)));

   SetAsSSortedList(fpsemi, fpelts);
   SetSize(fpsemi, Last-1);

   premult:=premult{[2..Length(premult)]}-1;
   postmult:=postmult{[2..Length(postmult)]}-1;

   SetLeftCayleyGraphSemigroup(fpsemi, premult);
   SetRightCayleyGraphSemigroup(fpsemi, postmult);
   SetAssociatedConcreteSemigroup(fpsemi, mono);

   sortedelts:=sortedelts{[2..Last]}; 
   actualelts:=actualelts{[2..Length(actualelts)]};
   pos:=pos{[2..Last]}-1;
   perm:=PermListList(pos, [1..Last-1]);
   sortedelts := List(sortedelts, 
   UnderlyingSemigroupElementOfMonoidByAdjoiningIdentityElt);
	actualelts:=List(actualelts,UnderlyingSemigroupElementOfMonoidByAdjoiningIdentityElt);
 
   
   premult:=Permuted(OnTuplesTuples(premult, perm), perm);
   postmult:=Permuted(OnTuplesTuples(postmult, perm), perm);
 
   SetAsSSortedList(mono, sortedelts);
   SetSize(mono, Last-1);
   SetLeftCayleyGraphSemigroup(mono, premult);
   SetRightCayleyGraphSemigroup(mono, postmult);
   SetAssociatedFpSemigroup(mono, fpsemi);

   u:=SemigroupHomomorphismByImagesNC(mono, fpsemi, 
       List(pos, x-> fpelts[x]));
       
   SetInverseGeneralMapping(u, SemigroupHomomorphismByImagesNC(fpsemi, mono, 
   actualelts));

   SetIsTotal(u, true); SetIsInjective(u, true); 
   SetIsSurjective(u, true); SetIsSingleValued(u, true);  
   SetIsomorphismFpSemigroup(mono, u);
fi;

end);

InstallMethod(\<, "for fp semigp elts produced by the Froidure-Pin algorithm", IsIdenticalObj, [IsFpSemigpReducedElt, IsFpSemigpReducedElt], function(x,y)

if not x=y then 
	return IsShortLexLessThanOrEqual(UnderlyingElement(x), UnderlyingElement(y));
else
	return false;
fi;
end);

InstallMethod(\=, "for fp semigp elts produced by the Froidure-Pin algorithm", IsIdenticalObj, [IsFpSemigpReducedElt, IsFpSemigpReducedElt], function(x,y)
			local S;

			return UnderlyingElement(x)=UnderlyingElement(y);
			
end);

InstallMethod(\<, "for fp monoid elts produced by the Froidure-Pin algorithm", IsIdenticalObj, [IsFpMonoidReducedElt, IsFpMonoidReducedElt], function(x,y)
if not x=y then 
	return IsShortLexLessThanOrEqual(UnderlyingElement(x), UnderlyingElement(y));
else
	return false;
fi;
end);

InstallMethod(\=, "for fp monoid elts produced by the Froidure-Pin algorithm", IsIdenticalObj, [IsFpMonoidReducedElt, IsFpMonoidReducedElt], function(x,y)
	return UnderlyingElement(x)=UnderlyingElement(y);
end);


#########
#trans.gi
#########

############################################################################
##
#M  <trans>^perm
##
##  Makes sense in that permutations have inverses and are transformations
##

InstallOtherMethod(\^, "for a transformation and a permutation",[IsTransformation, IsPerm],
function(t,p)
	return p^-1*t*p;
end); 

InstallMethod(\*, "trans * trans", IsIdenticalObj,
        [IsTransformation and IsTransformationRep, 
         IsTransformation and IsTransformationRep], 0, 
    function(x, y) 
        local a,b;

        a:= x![1]; b := y![1];
        #return TransformationNC(List([1 .. Length(a)], i -> b[a[i]]));
        return TransformationNC(b{a});
    end);
    
##########
#mgmadj.gi
##########

#############################################################################
##
#A  InjectionZeroMagma( <M> )
##  
##  The canonical homomorphism from the 
##  <M> into the magma formed from <M> with a single new element
##  which is a multiplicative zero for the resulting magma.
##  
##  In order to be able to define multiplication, the elements of the
##  new magma must be in  a new family.
##

InstallMethod(InjectionZeroMagma, 
    "method for a magma",
    true,
    [IsMagma], 10,
function(M)

	local
				Fam, Typ, 	# the new family and type
				z, 					# the new zero
				ZM,					# the new magma
				ZMgens,			# generators of the new magma
				filters,		# the new elements family's filters
				coerce;			# coerce an element of the base magma into the zero magma


  # Putting IsMultiplicativeElement in the NewFamily means that when you make,
  # say [a] it picks up the Category from the Family object and makes
  # sure that [a] has CollectionsCategory(IsMultiplicativeElement)

	# Preserve all sensible properties
	filters := IsMultiplicativeElementWithZero;

	if IsMultiplicativeElementWithOne(Representative(M)) then
		filters := filters and IsMultiplicativeElementWithOne;
	fi;

	if IsAssociativeElement(Representative(M)) then
		filters := filters and IsAssociativeElement;
	fi;

  Fam := NewFamily( "TypeOfElementOfMagmaWithZeroAdjoined", filters);
	Fam!.underlyingMagma := Immutable(M);

	# put n in the type data so that we can find the position in the database
	# without a search
	Typ := NewType(Fam, filters and 
			IsMagmaWithMultiplicativeZeroAdjoinedElementRep);

	
	coerce :=  g->Objectify(Typ, 
		rec( IsTheZero:= false, UnderlyingElement := g));

	# Now create the new magma and its zero element
	z := Objectify(Typ, rec( IsTheZero:= true ) );

	if Length(GeneratorsOfMagma(M))=0 then
		# ZM := Magma(CollectionsFamily(Fam),[]);
		Error("Can't adjoin a zero to a Magma without generators");
	fi;

	# make the list of generators into generators of ZM

	ZMgens := List(GeneratorsOfMagma(M), g->coerce(g));

	if IsSemigroup(M) then
		if IsMonoid(M) then
			# need to supply the identity as second argument
			ZM :=  MonoidByGenerators(Concatenation(ZMgens, [z]));
		else
			ZM :=  Semigroup(Concatenation(ZMgens, [z]));
		fi;
	else
		ZM :=  Magma(Concatenation(ZMgens, [z]));
	fi;

	#if IsGroup(M) then
	#	SetIsZeroGroup(ZM,true);
	#fi;

	SetMultiplicativeZero(ZM,z);

	Fam!.injection := Immutable(MagmaHomomorphismByFunctionNC(M, ZM, coerce));
	Fam!.zero := Immutable(z);
	return Fam!.injection;
end);