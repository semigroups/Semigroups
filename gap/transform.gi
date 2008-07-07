##
## transform.gi
## Version 3.1.1
## Mon Jun  9 17:02:20 BST 2008
##


##  <#GAPDoc Label="transformtop">
##  The functions in this chapter extend the functionality of &GAP; relating 
##  to transformations. 
##	<#/GAPDoc>


##  JDM install methods for partial transformations, partial bijections

###########################################################################
##
##  <#GAPDoc Label="IsRegularTransformation">
##  <ManSection>
##  <Attr Name="IsRegularTransformation" Arg="S, x"/>
##	<Description>
##	if <C>x</C> is a regular element of the transformation semigroup <C>S</C>, 
##	then <C>true</C> is returned. Otherwise <C>false</C> is returned.<P/> 
##
##	A transformation <M>x</M> is regular inside a transformation semigroup 
##	<M>S</M> if it lies inside a regular D-class. This is equivalent to the cone 
##	of the image of <M>x</M> containing a transversal of the kernel of <M>x</M>; 
##	see ??.
## <Example>
##  gap&gt; g1:=Transformation([2,2,4,4,5,6]);;
##  gap&gt; g2:=Transformation([5,3,4,4,6,6]);;
##  gap&gt; m1:=Monoid(g1,g2);;
##  gap&gt; IsRegularTransformation(m1, g1);
##  true
##  gap&gt; IsRegularTransformation(m1, g2);
##  false
##  gap&gt; IsRegularTransformation(FullTransformationSemigroup(6), g2);
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

InstallMethod(IsRegularTransformation, "for a transformation", true, [IsTransformationSemigroup, IsTransformation], 0,
function(M, x)

local r, orb, gens, s, p, n;

if HasIsRegularSemigroup(M) and IsRegularSemigroup(M) then 
  return true;
fi;

n:= ImageSetOfTransformation(x);   
r:= Size(n);

if RankOfTransformation(x^2)=r then      
  return true;
fi;

# otherwise form the weak orbit of img x.
orb:= [n];   
gens:= GeneratorsOfSemigroup(M);
for p in orb do
  for s in gens do
    n:=Set(ImageListOfTransformation(s){p});
    if Size(n) = r and not n in orb then

            # did we find a cross section? JDM1
	    # JDM try IsDuplicateFreeList or IsTransversal here

            if Size(Set(ImageListOfTransformation(x){n})) = r then
               return true;
            fi;

            Add(orb, n);
         fi;
      od;
   od;

   # if we arrive here, no cross section has been found.
   return false;

end );

#############################################################################
##
##	<#GAPDoc Label="IsTransversal">
##	<ManSection>
##	<Func Name="IsTransversal" Arg="ker, img"/>
##	<Description>
##	returns <C>true</C> if <C>img</C> is a transversal of <C>ker</C>, that is, 
##	if every class in <C>ker</C> contains exactly one element in <C>img</C>. 
##	Otherwise <C>false</C> is returned.
##	<Example>
##  gap&gt; g1:=Transformation([2,2,4,4,5,6]);;
##  gap&gt; g2:=Transformation([5,3,4,4,6,6]);;
##  gap&gt; ker:=KernelOfTransformation(g2*g1);
##  [ [ 1 ], [ 2, 3, 4 ], [ 5, 6 ] ] 
##  gap&gt; im:=ImageListOfTransformation(g2);
##  [ 5, 3, 4, 4, 6, 6 ]
##  gap&gt; IsTransversal(ker, im);
##  false
##  gap&gt; IsTransversal([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  true
##  </Example> <!-- transform.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(IsTransversal, 
function(ker, img)

if not Length(ker)=Length(img) then 
   return fail; 
else 
   return ForAll(ker, k-> Number(k, i-> i in img) = 1);
   #return ForAll(ker, k-> ForAny(k, i-> i in img));
fi;
   
end);

#############################################################################
##
##	<#GAPDoc Label="Idempotent">
##	<ManSection><Heading>Idempotent</Heading>
##	<Func Name="IdempotentNC" Arg="ker, img"/>
##	<Func Name="Idempotent" Arg="ker, img"/>
##	<Description>
##	<C>IdempotentNC</C> returns an idempotent with kernel <C>ker</C> and image 
##	<C>img</C> without checking <Ref Func="IsTransversal"/><C>(ker, im)</C>. 
##	<P/>
##
##	<C>Idempotent</C> returns an idempotent with kernel <C>ker</C> and image 
##	<C>img</C> after checking that <Ref Func="IsTransversal"/><C>(ker, im)</C> 
##	returns true. <P/>
##	<Example>
##  gap&gt; g1:=Transformation([2,2,4,4,5,6]);;
##  gap&gt; g2:=Transformation([5,3,4,4,6,6]);;
##  gap&gt; ker:=KernelOfTransformation(g2*g1);;
##  gap&gt; im:=ImageListOfTransformation(g2);;
##  gap&gt; Idempotent(ker, im);
##  Error,  the image must be a transversal of the kernel
##  [ ... ]
##  gap&gt; Idempotent([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
##  gap&gt; IdempotentNC([[1,2,3],[4,5],[6,7]], [1,5,6]);
##  Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

InstallGlobalFunction(IdempotentNC, 
function(ker, img)
      local e, l;
      e:= [];
      for l in ker do  
         e{l}:= 0*l + Intersection(l, img)[1];  
      od;
      #return TransformationNC(e);
      return Transformation(e);
end);

InstallGlobalFunction(Idempotent, 
function(ker, img)
  if IsTransversal(ker, img) then 
     return IdempotentNC(ker, img);
  else 
     Error(" the image must be a transversal of the kernel");
  fi;
end);

#############################################################################
##
##	<#GAPDoc Label="PermRepTrans">
##	<ManSection>
##	<Func Name="PermRepTrans" Arg="x"/>
##	<Description>
##	converts a transformation <C>x</C> that is a permutation of its image into 
##	that permutation.
##	<Example>
##  gap&gt; t:=Transformation([1,2,9,9,9,8,8,8,4]);
##  Transformation( [ 1, 2, 9, 9, 9, 8, 8, 8, 4 ] )
##  gap&gt; PermRepTrans(t);
##  (4,9)
##  gap&gt; t*last;
##  Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8, 9 ] )
##  gap&gt; PermRepTrans(last);
##  ()
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

InstallMethod(PermRepTrans, "for a transformation", true, [IsTransformation], 0,
function(trans)
local perm;

if not ImageSetOfTransformation(trans^2)=ImageSetOfTransformation(trans) then 
	Error("transformation must be a permutation of its image");
else
	return PermList(List([1..DegreeOfTransformation(trans)], 
                function(x)
		if x in ImageSetOfTransformation(trans) then 
			return x^trans;
		else 
			return x;
		fi;
         end));
fi;

end);

#############################################################################
##
#M  AsTransformationNC( <binary relation>)  
##   
##  returns the binary relation as a transformation. If <rel> is not a 
##  transformation the results are unpredictable.
##

InstallOtherMethod(AsTransformationNC, true, [IsBinaryRelation], 
0, function(rel)
return TransformationNC( Flat( Successors( rel ) ) );
end);

#############################################################################
##
##	<#GAPDoc Label="RandomTransformation">
##	<ManSection>
##	<Func Name="RandomTransformation" Arg="ker, img"/>
##	<Description>
##	This is a new method for the existing library function 
##	<Ref Func="RandomTransformation" BookName="ref"/>.  A random transformation 
##	is returned that has the given image <C>img</C> and kernel <C>ker</C>. Note 
##	that <C>ker</C> must have the same number of classes as <C>img</C> has 
##	elements.
##	<Example>
##	gap&gt; x:=RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
##	gap&gt; KernelOfTransformation(x)=[[1,2,3], [4,5], [6,7,8]];
##	true
##	gap&gt; ImageSetOfTransformation(x)=[1,2,3];
##	true
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(RandomTransformation,  "random transformation with given kernel and image", true, 
[IsCyclotomicCollColl, IsCyclotomicCollection], 0,     

function(ker, img)
local e, l, x;

if not Size(ker)=Size(img) then 
   Error ("there must be equal numbers of kernel classes and image elements");
else  
   e:= [];
   for l in ker do  
      x:=RandomList(img);
      e{l}:= 0*l + x;
      img:=DifferenceLists(img, [x]);
   od;
   return TransformationNC(e);     
fi;

end);

#############################################################################
##
##	<#GAPDoc Label="IndexPeriodOfTransformation">
##	<ManSection>
##	<Oper Name="IndexPeriodOfTransformation" Arg="x"/>
##	<Description>
##	returns the minimum numbers <M>m, r</M> such that <M>x^{m+r}=x^m</M>; known 
##	as the index and period of the transformation. 
##	<Example>
##  gap&gt; x:=Transformation( [ 3, 4, 4, 6, 1, 3, 3, 7, 1 ] );;
##  gap&gt; IndexPeriodOfTransformation(x);
##  [ 2, 3 ]
##  gap&gt; x^2=x^5;
##  true
##	</Example> <!-- transform.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##	</Description>

InstallMethod(IndexPeriodOfTransformation, "for a transformation", true, [IsTransformation], 0, 
function(x)
local i, y;

i:=1;
y:=x;

while not RankOfTransformation(y^2)=RankOfTransformation(y) do 
	i:=i+1; 
	y:=y*x;
od;

return [i, Order(PermRepTrans(y))];
end);

######################################

InstallMethod( PrintObj, "for a transformation semigroup", [IsTransformationSemigroup], 
function(S)

Print( "<trans. semigroup" );
if HasSize( S )  then
	Print( " of size ", Size( S ) );
fi;

Print( " with ", Length( GeneratorsOfSemigroup( S ) ), " generators>" );

return;

end);

######################################
######################################
#JDM this should not be necessary. Better if '\in' for a 
#IsFullTransformationSemigroup took priority 
#over '\in' for a TransformationSemigroup

InstallOtherMethod(One, "for a transformation semigroup", true, [IsFullTransformationSemigroup], 0, 
x-> TransformationNC([1..DegreeOfTransformationSemigroup(x)]));