##
## properties.gi
## Version 3.1
## Fri May  2 17:42:56 BST 2008
##

##  The functions in this file are used to test whether a given 
##  transformation semigroup has a given property. The algorithms can be 
##  found in:
##  
##  R. Gray and J. D. Mitchell, Largest subsemigroups of the full
##  transformation monoid, submitted for publication.
##
##  JDM include Size things
##

###########################################################################
##
##	<#GAPDoc Label="IsCompletelyRegularSemigroup">
##	<ManSection>
##	<Prop Name="IsCompletelyRegularSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is completely 
##	regular and <C>false</C> otherwise.<P/>
##	A semigroup is <E>completely regular</E> 
##	if every element is contained in a subgroup.
##
##	<Example>	
##  gap&gt; gens:=[ Transformation( [ 1, 2, 4, 3, 6, 5, 4 ] ), 
##  &gt;  Transformation( [ 1, 2, 5, 6, 3, 4, 5 ] ), 
##  &gt;  Transformation( [ 2, 1, 2, 2, 2, 2, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsCompletelyRegularSemigroup(S);
##  true
##  gap&gt; S:=RandomSemigroup(5,5);;
##  gap&gt; IsSimpleSemigroup(S);
##  false
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod( IsCompletelyRegularSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,  
function(M)
local pnt, orbit, gens, s, new, g;

if HasIsRegularSemigroup(M) and not IsRegularSemigroup(M) then 
	return false;
fi;

gens:= GeneratorsOfSemigroup(M);

for g in gens do
  orbit:=[ImageSetOfTransformation(g)];
  if not Size(OnSets(orbit[1], g))=Size(orbit[1]) then                          
    return false;                                                               
  fi; 

  for pnt in orbit do
    for s in gens do
      new:= OnSets(pnt,s);
      if not new in orbit then
        Add(orbit, new); 
        if not Size(OnSets(new, g))=Size(new) then
          return false;
        fi;
      fi;
    od;
  od;
od;

return true;

end) ;

###########################################################################
##
##	<#GAPDoc Label="IsCompletelySimpleSemigroup">
##	<ManSection><Heading>IsSimpleSemigroup</Heading>
##	<Prop Name="IsSimpleSemigroup" Arg="S"/>
##	<Prop Name="IsCompletelySimpleSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is simple and 
##	<C>false</C> otherwise.<P/>
##
##	A semigroup is <E>simple</E> if it has no proper
##	2-sided ideals. A semigroup is <E>completely simple</E> if it is simple and 
##	possesses minimal left and right ideals. A finite semigroup is simple if and 
##	only if it is completely simple. 
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 2 ] ), 
##  &gt;  Transformation( [ 1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 3 ] ), 
##  &gt;  Transformation( [ 1, 7, 3, 9, 5, 11, 7, 1, 9, 3, 11, 5, 5 ] ), 
##  &gt;  Transformation( [ 7, 7, 9, 9, 11, 11, 1, 1, 3, 3, 5, 5, 7 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsSimpleSemigroup(S);
##  true
##  gap&gt; IsCompletelySimpleSemigroup(S);
##  true
##  gap&gt; S:=RandomSemigroup(5,5);;
##  gap&gt; IsSimpleSemigroup(S);
##  false
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

##  JDM could include if IsCompletelyRegular and HasGreensDClasses etc
##  JDM but this is so fast it might not be worthwhile...

InstallMethod( IsSimpleSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,  
function(M)
local pnt, orbit, gens, s, new, g, image;
   	
gens:= GeneratorsOfSemigroup(M);

for g in gens do
  image:=ImageSetOfTransformation(g);
  orbit:=[image];
  for pnt in orbit do
    for s in gens do
      new:= OnSets(pnt,s);
      if not new in orbit then
        Add(orbit, new);
        if not Size(OnSets(new, g))=Size(image) then
          return false;
        fi;
      fi;
    od;
  od;
od;

SetIsCompletelyRegularSemigroup(M,true);
SetIsRegularSemigroup(M, true);

return true;

end) ;

#############################################################################
#M  Size( <comp. simple trans. semigroup> );
## 
##  size for completely simple transformation semigroup.
## 

InstallOtherMethod(Size, "for a comp. simple trans. semigroup", true, [IsCompletelySimpleSemigroup and IsTransformationSemigroup], 0,
function(M)
local gens, ims, kers, H;

gens:=GeneratorsOfSemigroup(M);

ims:=Size(Set(List(gens, ImageSetOfTransformation)));
kers:=Size(Set(List(gens, KernelOfTransformation)));
H:=GreensHClassOfElement(M, gens[1]);

return Size(H)*ims*kers;

end);

###########################################################################
##
##	<#GAPDoc Label="IsGroupAsSemigroup">
##	<ManSection>
##	<Prop Name="IsGroupAsSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is a group and 
##	<C>false</C> otherwise.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 4, 5, 3, 7, 8, 6, 9, 1 ] ), 
##  &gt;  Transformation( [ 3, 5, 6, 7, 8, 1, 9, 2, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsGroupAsSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
 
InstallMethod(IsGroupAsSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
function(M)
local gens;

gens:=GeneratorsOfSemigroup(M);

return  ForAll(gens, y-> ImageSetOfTransformation(y)	
		=ImageSetOfTransformation(gens[1]))
 and 
	ForAll(gens, y->KernelOfTransformation(y)
		=KernelOfTransformation(gens[1]))
 and 
	ImageSetOfTransformation(gens[1]^2)=
        ImageSetOfTransformation(gens[1]); #it's a perm. of its image
end);

###########################################################################
##
##	<#GAPDoc Label="IsCliffordSemigroup">
##	<ManSection>
##	<Prop Name="IsCliffordSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is a Clifford 
##	semigroup and <C>false</C> otherwise.<P/>
##
##  A semigroup <M>S</M> is a <E>Clifford 
##	semigroup</E> if it is a regular semigroup whose idempotents are central, 
##	that is, for all <M>e,f</M> in <M>S</M> with <M>e^2=e</M> and <M>f^2=f</M> 
##	we have that <M>ef=fe</M>.
##	<Example>
##  gap&gt; gens:=[Transformation([1,2,4,5,6,3,7,8]),
##  &gt; Transformation([3,3,4,5,6,2,7,8]),
##  &gt;Transformation([1,2,5,3,6,8,4,4])];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsCliffordSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsCliffordSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 

function(M)
local gens, identities, gen, identity;

if HasIsInverseSemigroup(M) and not IsInverseSemigroup(M) then 
	return false;
elif HasIsRegularSemigroup(M) and not IsRegularSemigroup(M) then 
	return false;
elif HasIsCompletelyRegularSemigroup(M) and not IsCompletelyRegularSemigroup(M) then 
	return false;
elif not IsCompletelyRegularSemigroup(M) then 
  return false;
elif IsGroupAsSemigroup(M) then
  return true;
else

  gens:=GeneratorsOfSemigroup(M);

  #JDM this should be done online...
  identities:=List(gens, x->Idempotent(KernelOfTransformation(x), ImageSetOfTransformation(x)));

  for gen in gens do

    if not ImageSetOfTransformation(gen^2)
           =ImageSetOfTransformation(gen) then 
      return false; 
    fi;

    for identity in identities do
      if not identity*gen=gen*identity then 
        return false;
      fi;
    od;

  od;

  return true;

fi;

end);

###########################################################################
##
##	<#GAPDoc Label="IsRegularSemigroup">
##	<ManSection>
##	<Prop Name="IsRegularSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is a regular
##	semigroup and <C>false</C> otherwise. The algorithm used here is essentially 
##	the same algorithm as that used for 
##	<Ref Attr="GreensRClasses" BookName="ref"/> in <Package>Monoid</Package>. If 
##	<C>S</C> is regular, then <C>S</C> will have the attribute 
##	<C>GreensRClasses</C> after <C>IsRegularSemigroup</C> is invoked. <P/>
##
##	A semigroup <M>S</M> is <E>regular</E> if for all <M>x</M> in <M>S</M> there 
##	exists <M>y</M> in <M>S</M> such that <M>xyx=x</M>.
##
##	<Example>
##  gap&gt; IsRegularSemigroup(FullTransformationSemigroup(5));
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(IsRegularSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,

function ( M )
local n, one, gens, images, positions, classes, classespart, reps, 
      kernels, orb, x, img, k, j, i, r, pos, ker, new, s, class;

if IsCompletelyRegularSemigroup(M) then 
   return true;
elif HasGreensDClasses(M) then 
   return ForAll(GreensDClasses(M), IsRegularDClass);
elif HasGreensRClasses(M) then 
  return ForAll(GreensRClasses(M), x-> ForAny(GreensData(x)!.strongorb, 
    y->IsTransversal(KernelOfTransformation(Representative(x)), y)));

else
   
   n := DegreeOfTransformationSemigroup( M );
   one := TransformationNC( [ 1 .. n ] );

   if IsTransformationMonoid( M )  then
      gens := GeneratorsOfMonoid( M );
   else
      gens := GeneratorsOfSemigroup( M );
   fi;

   images := List( [ 1 .. n ], x-> [] );
   positions := List( [ 1 .. n ], x->[]) ;
   classes := [  ]; reps := [  ];
   kernels := [  ]; classespart:=[];
   orb := [ one ];
    
   for x in orb  do
      img := ImageSetOfTransformation( x );
      ker:=KernelOfTransformation( x );
      k := Length( img );
      j := Position( images[k], img );

      if j = fail  then
        if IsTransformationMonoid( M ) or not x = one  then
           class:=GreensRClassOfElement(M, x); 
           r := GreensRClassData(class);
           
           if not ForAny(r!.strongorb, x-> IsTransversal(ker, x)) then 
              return false;
           fi;            

	   Add(classes, class);  Add(classespart, [r]);
           Add( reps, [ x ] ); 
           Add( kernels, [ KernelOfTransformation( x ) ] );
           Append( images[k], r!.strongorb ); j := Length( classespart );
           Append( positions[k], List( r!.strongorb,  x ->j ));       
         fi;
         for s  in gens  do
            Add( orb, s * x );
         od;
      else
            if IsTransformationMonoid( M ) or not x = one  then
                pos := positions[k][j];
                r := classespart[pos][1];
                
		if not ForAny(r!.strongorb, x-> IsTransversal(ker, x)) then 
               	   return false;
           	fi;
                
		x := x * r!.perms[Position( r!.strongorb, img )];
                new := true;
                i:=0;

		repeat       
                  	i:=i+1;                    
  			if ker = kernels[pos][i] and 
                         x in classespart[pos][i] then
                        	new := false;
                    	fi;
                until i=Length( kernels[pos] ) or new=false;

                if new then
                    Add( reps[pos], x );
                    Add( kernels[pos], ker );
                    class:=GreensRClassOfElement(M, x);
                    r:=RClassData( rec( rep:=x, strongorb:=r!.strongorb, 
			  perms:=r!.perms, schutz:=r!.schutz ));
                    
                    Add( classespart[pos], r);                   
		   
                    Add(classes, class);

		    for s  in gens  do
                        Add( orb, s * x );
                    od;
                fi;
            fi;
        fi;
      od;

      SetPositionsRClasses( M, positions);
      SetGradedRClasses(M, classespart);
      SetGradedImagesTransformationMonoid( M, images);
      SetInternalKernels(M, kernels);
      SetGreensRClassReps(M, reps); #JDM can this be a flat list???

      return true;
fi;
end);

###########################################################################
##
##	<#GAPDoc Label="IsInverseSemigroup">
##	<ManSection>
##	<Prop Name="IsInverseSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is an inverse
##	semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is an <E>inverse semigroup</E> if every element 
##	<M>x</M> in <M>S</M> has a unique semigroup inverse, that is, a unique 
##	element <M>y</M> such that <M>xyx=x</M> and <M>yxy=y</M>.
##
##	<Example>
##  gap&gt; gens:=[Transformation([1,2,4,5,6,3,7,8]),
##  &gt; Transformation([3,3,4,5,6,2,7,8]),
##  &gt;Transformation([1,2,5,3,6,8,4,4])];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsInverseSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#JDM is the `other' required here? 

InstallOtherMethod(IsInverseSemigroup, "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,

function(M)
local imgs, kers, rclasses, class, ker, strongorb, numb, img, istransv;

if not IsRegularSemigroup(M) then 
   return false;
elif IsCompletelyRegularSemigroup(M) and not HasGreensRClasses(M) then
   return IsCliffordSemigroup(M);
else 
  imgs:=ImagesTransformationMonoid(M);
  kers:=KernelsTransformationMonoid(M);
  
  if not Length(imgs)=Length(kers) then 
     return false;
  else
     rclasses:=GreensRClasses(M);

     for class in rclasses do
        class:=GreensRClassData(class);
        ker:=KernelOfTransformation(class!.rep);
        strongorb:=class!.strongorb;
        numb:=0;
        for img in strongorb do
           istransv:=IsTransversal(ker,img);
           if istransv and numb<1 then
              numb:=numb+1;
           elif istransv then 
              return false;
           fi;
        od;
        if numb=0 then 
           return false;
        fi;
     od;

  fi;
  return true;

fi;
end);

###########################################################################
##
##	<#GAPDoc Label="IsBand">
##	<ManSection>
##	<Prop Name="IsBand" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is a band and 
##	<C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is a <E>band</E> if every element is an idempotent, 
##	that is, <M>x^2=x</M> for all <M>x</M> in <M>S</M>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 1 ] ), 
##  &gt; Transformation( [ 2, 2, 2, 5, 5, 5, 8, 8, 8, 2 ] ), 
##  &gt; Transformation( [ 3, 3, 3, 6, 6, 6, 9, 9, 9, 3 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 4 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 7 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsBand(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsBand, "for a transformation semigroup",
true, [IsTransformationSemigroup], 0,

function(M)

if not IsCompletelyRegularSemigroup(M) then 
   return false;
else
   return #JDM Size(M)=Size(Idempotents(M)); 
    ForAll(AsList(M), IsIdempotent);
fi; 
end);

###########################################################################
##
##	<#GAPDoc Label="IsRectangularBand">
##	<ManSection>
##	<Prop Name="IsRectangularBand" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is a 
##	rectangular band and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is a <E>rectangular band</E> if for all <M>x,y,z</M> in 
##	<M>S</M> we have that <M>x^2=x</M> and <M>xyz=xz</M>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 1 ] ), 
##  &gt; Transformation( [ 2, 2, 2, 5, 5, 5, 8, 8, 8, 2 ] ), 
##  &gt; Transformation( [ 3, 3, 3, 6, 6, 6, 9, 9, 9, 3 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 4 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 7 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRectangularBand(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

##  JDM is there a better way?

InstallMethod(IsRectangularBand, "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,

function(M)
local x,y,z, gens;

if not IsCompletelySimpleSemigroup(M) then 
   return false;
elif HasIsBand(M) then
   return IsBand(M) and IsCompletelySimpleSemigroup(M);
else
   #check the generators

   gens:=GeneratorsOfSemigroup(M);

   for x in gens do
      for y in gens do
         for z in gens do
            if not x*y*z=x*z then 
               return false;
            fi;
         od;
      od;
   od;
   #SetIsBand(M, true)
   return true;
fi; 
  
end);

###########################################################################
##
##	<#GAPDoc Label="IsSemiBand">
##	<ManSection>
##	<Prop Name="IsSemiBand" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is a 
##	semiband and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is a <E>semiband</E> if it is generated by its 
##	idempotent elements, that is, elements satisfying <M>x^2=x</M>.
##
##	<Example>
##  gap&gt; S:=FullTransformationSemigroup(4);;
##  gap&gt; x:=Transformation( [ 1, 2, 3, 1 ] );;
##  gap&gt; D:=GreensDClassOfElement(S, x);;
##  gap&gt; T:=Semigroup(Elements(D));;
##  gap&gt; IsSemiBand(T);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

##  JDM is there a better way?

InstallMethod(IsSemiBand, "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,

function(M)

if Idempotents(M)=[] then #JDM this can't happen!
   return false;
elif IsOrthodoxSemigroup(M) then #JDM advantage?
    if IsCompletelyRegularSemigroup(M) and IsBand(M) then 
       return true;
    else
       return false;
    fi;
else
   return Size(M)=Size(Semigroup(Idempotents(M)));
fi;  
end);

###########################################################################
##
##	<#GAPDoc Label="IsOrthodoxSemigroup">
##	<ManSection>
##	<Prop Name="IsOrthodoxSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is 
##	orthodox and <C>false</C> otherwise.<P/>
##	
##	A semigroup is an <E>orthodox semigroup</E> if its idempotent elements 
##	form a subsemigroup.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 1, 1, 4, 5, 4 ] ), 
##  &gt;  Transformation( [ 1, 2, 3, 1, 1, 2 ] ), 
##  &gt;  Transformation( [ 1, 2, 3, 1, 1, 3 ] ), 
##  &gt;  Transformation( [ 5, 5, 5, 5, 5, 5 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsOrthodoxSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

##  JDM is there a better way?


InstallMethod(IsOrthodoxSemigroup, "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,

function(M)
local idems, e, f;

idems:=Idempotents(M);

for e in idems do
   for f in idems do
      if not (e*f)^2=e*f then #JDM IsIdempotent(e*f)? 
         return false;
      fi;
   od;
od;

return true;
  
end);

###########################################################################
##
##	<#GAPDoc Label="IsRightZeroSemigroup">
##	<ManSection>
##	<Prop Name="IsRightZeroSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is 
##	a right zero semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is a <E>right zero semigroup</E> if <M>xy=y</M> for all 
##	<M>x,y</M> in <M>S</M>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
##  &gt;  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRightZeroSemigroup(S);
##  false
##  gap&gt; gens:=[Transformation( [ 1, 2, 3, 3, 1 ] ), 
##  &gt;  Transformation( [ 1, 2, 4, 4, 1 ] )];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRightZeroSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsRightZeroSemigroup, "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,

function(M)
local gens, kers;

gens:=GeneratorsOfSemigroup(M);
kers:=Set(List(gens, KernelOfTransformation));

if Length(kers)=1 and ForAll(gens, IsIdempotent) then
   return true;
else
   return false;
fi;

end);

###########################################################################
##
##	<#GAPDoc Label="IsLeftZeroSemigroup">
##	<ManSection>
##	<Prop Name="IsLeftZeroSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is 
##	a left zero semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is a <E>left zero semigroup</E> if <M>xy=x</M> for all 
##	<M>x,y</M> in <M>S</M>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
##  &gt;  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRightZeroSemigroup(S);
##  false
##  gap&gt; gens:=[Transformation( [ 1, 2, 3, 3, 1 ] ), 
##  &gt; Transformation( [ 1, 2, 3, 3, 3 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsLeftZeroSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsLeftZeroSemigroup, "for a transformation semigroup", 
true, [IsTransformationSemigroup], 0,

function(M)
local gens, imgs;

gens:=GeneratorsOfSemigroup(M);
imgs:=Set(List(gens, ImageSetOfTransformation));

if Size(imgs)=1 and ForAll(gens, IsIdempotent) then
   return true;
else
   return false;
fi;

end);

###########################################################################
##
##	<#GAPDoc Label="IsCommutativeSemigroup">
##	<ManSection>
##	<Prop Name="IsCommutativeSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <M>S</M> is commutative 
##	and <C>false</C> otherwise. The function 
##	<Ref Prop="IsCommutative" BookName="ref"/> can also be used to test if a 
##	semigroup is commutative.  <P/>
##
##	A semigroup <M>S</M> is <E>commutative</E> if 
##	<M>xy=yx</M> for all <M>x,y</M> in <M>S</M>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 4, 5, 3, 7, 8, 6, 9, 1 ] ), 
##  &gt;  Transformation( [ 3, 5, 6, 7, 8, 1, 9, 2, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsCommutativeSemigroup(S);
##  true
##  gap&gt; IsCommutative(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsCommutativeSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,
function(M)
local gens, n, i, j; 

gens:=GeneratorsOfSemigroup(M);
n:=Length(gens);

for i in [1..n] do
  for j in [i+1..n] do
    if not gens[i]*gens[j]=gens[j]*gens[i] then 
      return false;
    fi;
  od;
od;

return true;

end);

###########################################################################
##
##	<#GAPDoc Label="IsZeroSemigroup">
##	<ManSection>
##	<Prop Name="IsZeroSemigroup" Arg="S"/>
##	<Description> 
##	returns <C>true</C> if the transformation semigroup <M>S</M> is 
##	a zero semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> is a <E>zero semigroup</E> if there exists an element 
##	<M>0</M> in <M>S</M> such that <M>xy=0</M> for all <M>x,y</M> in <M>S</M>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 7, 6, 3, 1, 5, 3, 6, 5, 9 ] ), 
##  &gt; Transformation( [ 5, 3, 5, 1, 9, 3, 8, 7, 4, 3 ] ), 
##  &gt; Transformation( [ 5, 10, 10, 1, 7, 6, 6, 8, 7, 7 ] ), 
##  &gt; Transformation( [ 7, 4, 3, 3, 2, 2, 3, 2, 9, 3 ] ), 
##  &gt; Transformation( [ 8, 1, 3, 4, 9, 6, 3, 7, 1, 6 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsZeroSemigroup(S);
##  false
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(IsZeroSemigroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,
function(S)
local zero, x, y;

zero:=MultiplicativeZero(S);

if not zero=fail then
	for x in GeneratorsOfSemigroup(S) do
		for y in GeneratorsOfSemigroup(S) do 
			if not x*y=zero then 
				return false;
			fi;
		od;
	od;
else
	return false;
fi;

return true;
end);

###########################################################################
##
##	<#GAPDoc Label="IsZeroGroup">
##	<ManSection>
##	<Prop Name="IsZeroGroup" Arg="S"/>
##	<Description> 
##	returns <C>true</C> if the transformation semigroup <M>S</M> is 
##	a zero group and <C>false</C> otherwise.<P/>
##
##	A semigroup <M>S</M> <M>S</M> is a <E>zero group</E> if there exists an 
##	element <M>0</M> in <M>S</M> such that <M>S</M> without <M>0</M> is a group 
##	and for all <M>x</M> in <M>S</M> we have that <M>x0=0x=0</M>.
##	<Example>
##  gap&gt; S:=ZeroGroup(DihedralGroup(10));;
##  gap&gt; iso:=IsomorphismTransformationSemigroup(S);;
##  gap&gt; T:=Range(iso);;
##  gap&gt; IsZeroGroup(T);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(IsZeroGroup, "for a transformation semigroup", true, [IsSemigroup], 0, 
function(S)
local zero, one;

zero:=MultiplicativeZero(S);
one:=MultiplicativeNeutralElement(S);

if not (zero=fail or one=fail) and Length(GreensHClasses(S))=2 then 
	return IsGroupHClass(GreensHClassOfElement(S, one));
fi;

return false;
end);


###########################################################################
##
##	<#GAPDoc Label="MultiplicativeZero">
##	<ManSection>
##	<Prop Name="MultiplicativeZero" Arg="S"/>
##	<Description> 
##	returns the multiplicative zero of the transformation semigroup <M>S</M> if 
##	it has one and returns <C>fail</C> otherwise. 
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 4, 2, 6, 6, 5, 2 ] ), 
##  &gt; Transformation( [ 1, 6, 3, 6, 2, 1, 6 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; MultiplicativeZero(S);
##  Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] )
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(MultiplicativeZero, "for a transformation semigroup", true, [IsTransformationSemigroup], 0,
function(S)
local n, imgs, m, kers, idem;

n:=DegreeOfTransformationSemigroup(S);
imgs:=GradedImagesTransformationMonoid(S);
m:=PositionProperty([1..n], x-> not Length(imgs[x])=0);

if Length(imgs[m])=1 then
	kers:=GradedKernelsTransformationMonoid(S); 
	if Length(kers[m])=1 then 
		idem:=Idempotent(kers[m][1], imgs[m][1]);
		if not idem=fail and Size(GreensHClassOfElement(S, idem))=1 then 
			return idem;
		fi;
	fi;
fi;

return fail;
end);

#####################

#InstallOtherMethod(IsMultiplicativeZero, "for a transformation semigroup", true, [IsTransformationSemigroup, IsTransformation], 0,
#function(S, f)
#
#return f=MultiplicativeZero(S);
#end);