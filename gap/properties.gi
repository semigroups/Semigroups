#############################################################################
##
#W  properties.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

###########################################################################

InstallMethod( IsCompletelyRegularSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
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
## for simple transformation semigroups... 

InstallOtherMethod(Size, "for a simple transformation semigroup", true, [IsCompletelySimpleSemigroup and IsTransformationSemigroup], 0,
function(M)
local gens, ims, kers, H;

gens:=GeneratorsOfSemigroup(M);

ims:=Size(Set(List(gens, ImageSetOfTransformation)));
kers:=Size(Set(List(gens, KernelOfTransformation)));
H:=GreensHClassOfElement(M, gens[1]);

return Size(H)*ims*kers;

end);

###########################################################################
 
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

InstallMethod(IsCliffordSemigroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0, 

function(M)
local gens, identities, gen, identity;

if HasIsInverseSemigroup(M) and not IsInverseSemigroup(M) then 
	return false;
elif HasIsRegularSemigroup(M) and not IsRegularSemigroup(M) then 
	return false;
elif HasIsCompletelyRegularSemigroup(M) and 
 not IsCompletelyRegularSemigroup(M) then 
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

InstallOtherMethod(IsRegularSemigroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,

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
      SetGradedImagesOfTransSemigroup( M, images);
      SetInternalKernels(M, kernels);
      SetGreensRClassReps(M, reps); #JDM can this be a flat list???

      return true;
fi;
end);

###########################################################################
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
  imgs:=ImagesOfTransSemigroup(M);
  kers:=KernelsOfTransSemigroup(M);
  
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

InstallMethod(IsCommutativeSemigroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,
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
#JDM new for 3.1.4 used to accept IsSemigroup as filter, changed for semex

InstallOtherMethod(IsZeroGroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0, 
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

InstallOtherMethod(MultiplicativeZero, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,
function(S)
local n, imgs, m, kers, idem;

n:=DegreeOfTransformationSemigroup(S);
imgs:=GradedImagesOfTransSemigroup(S);
m:=PositionProperty([1..n], x-> not Length(imgs[x])=0);

if Length(imgs[m])=1 then
	kers:=GradedKernelsOfTransSemigroup(S); 
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
#JDM why's this commented out? 
#InstallOtherMethod(IsMultiplicativeZero, "for a transformation semigroup", 
#true, [IsTransformationSemigroup, IsTransformation], 0,
#function(S, f)
#
#return f=MultiplicativeZero(S);
#end);