






InstallMethod(\*, "for a partial perm and partial perm", 
[IsPartialPerm, IsPartialPerm],
function(f,g)
#  local dom, perm, img;

#  dom:=OnTuples(Intersection(f!.img, g!.dom), f!.perm^-1);
#  perm:=f!.perm*g!.perm;
#  img:=OnTuples(dom, perm);
#  return Objectify(TypeObj(f), rec( dom:=dom,
#   perm:=perm, img:=img));
    local  a, b, c;
    a := f!.img;
    b := g!.img;
    c := b{a};
    MakeImmutable( c );
    return Objectify(PartialPermType, rec(max:=Maximum(f!.max, g!.max), img:=c));
end);

InstallMethod(\^, "for a partial perm",
[IsPartialPerm, IsNegInt],
function(f, n)
  f:=f^-n;
  return );
end);

InstallGlobalFunction(AsPermOfDomRan, 
f-> MappingPermListList(Dom(f), Ran(f)));

InstallMethod(Dom, "for a partial perm",
[IsPartialPerm], 999, f-> PositionsProperty(f!.img, x-> not x=f!.max));

InstallMethod(Ran, "for a partial perm", 
[IsPartialPerm], f-> f!.img{Dom(f)});

InstallOtherMethod(Rank, "for a partial perm",
[IsPartialPerm], f-> Length(Dom(f)));

#PPP

## Partial transformations
#############################################################################

InstallGlobalFunction(PartialTransformation,
function(img)

if ForAny(img, x-> not x in [1..Length(img)+1]) or not IsDenseList(img) then
   Error ("the argument does not describe a partial transformation");
fi;

return PartialTransformationNC(img);
end);

#############################################################################

InstallGlobalFunction(PartialTransformationNC,
function(img)
return Objectify(PartialTransformationType, rec(img:=img));
end);

#############################################################################

InstallMethod(PrintObj, "for a partial perm",
[IsPartialPerm], 
function(f)
Print("PartialPerm( ", Dom(f), " -> ", Ran(f), " )");
#Print("PartialPerm( ", f!.img, " )");
end);

#############################################################################

InstallMethod(PrintObj, "for a partial transformation", 
[IsPartialTransformation],
function(f) 
Print("PartialTransformation( ", ReplacedString(String(f!.img), 
 String(Length(f!.img)+1), "-"), ")");
end);

#############################################################################

InstallOtherMethod(ImageListOfTransformation, "for a partial transformation",  
[IsPartialTransformation],
function(f)
return f!.img;
end);

#############################################################################

InstallOtherMethod(ImageSetOfTransformation, "for a partial transformation", 
[IsPartialTransformation],
function(f)
local img;
img:=Set(f!.img);
return img{[1..Length(img)-1]};
end);

#############################################################################

InstallOtherMethod(RankOfTransformation,  "for a partial transformation", 
[IsPartialTransformation],
function(f) 
return Length(Set(f!.img))-1;
end);

#############################################################################

InstallOtherMethod(DegreeOfTransformation, "for a partial transformation",
[IsPartialTransformation], f-> Length(f!.img));

#############################################################################

InstallOtherMethod(KernelOfTransformation, "for a partial transformation", 
[IsPartialTransformation],
function(f)
local ker, imgs, i;

ker:=[]; imgs:=f!.img;

for i in [1..Length(imgs)] do
	if not imgs[i]=Length(f!.img)+1 then 
		if not IsBound(ker[imgs[i]]) then 
			ker[imgs[i]]:=[];
		fi;
		Add(ker[imgs[i]], i);
	fi;
od;

return Set(ker);
end);

#############################################################################

InstallMethod(DomainOfPartialTransformation, "for a partial transformation", 
[IsPartialTransformation], 
function(f)
return Filtered([1..DegreeOfTransformation(f)], x-> not x^f=Length(f!.img)+1);
end);

#############################################################################

#RRR

# new for 0.4! - RandomPartialPerm - "for a pos. int."

InstallGlobalFunction(RandomPartialPerm, 
function(n) 
  local out, avail, i, j;
  
  out:=EmptyPlist(n); avail:=[1..2*n];

  for i in [1..n] do 
      j:=Random(avail);
      if not j>n then 
        out[i]:=j; RemoveSet(avail, out[i]);
      fi;
  od;
  
  return PartialPermNC(out);
end);


#InstallGlobalFunction(RandomPartialTransformation,
#function(n)
#local out, i;

#out:=[];
#for i in [1..n] do #
#	Add(out, Random([1..n+1]));
#od;

#return PartialTransformationNC(out);
#end);

#############################################################################

#InstallTrueMethod(IsPartialTransformationMonoid, IsMonoid and 
#	IsPartialTransformationCollection);

#############################################################################
	
#InstallMethod(IsPartialTransformationMonoid, "for a transformation semigroup",
#    true, [IsPartialTransformationSemigroup and HasGeneratorsOfSemigroup], 0,
#function( S )
#    if ForAny(GeneratorsOfSemigroup(S),
#           x->RankOfTransformation(x)=DegreeOfPartialTransformationSemigroup(S)) then 
#
#        SetGeneratorsOfMonoid(S, GeneratorsOfSemigroup(S));
#        SetIsMonoid(S, true);
#        return true;
#    else
#        return false;
#    fi;
#end);	

#############################################################################

InstallTrueMethod(IsFinite, IsPartialTransformationSemigroup);

#############################################################################

InstallMethod(DegreeOfPartialTransformationSemigroup, 
"for a partial transformation semigroup", [IsPartialTransformationSemigroup],
function(s)
return Length(Representative(s)!.img);
end);

#############################################################################

InstallOtherMethod(AsTransformation, "for a partial transformation", [IsPartialTransformation],
function(f)
return TransformationNC(Concatenation(f!.img, [Length(f!.img)+1]));
end);

#############################################################################

#InstallMethod(IsomorphismTransformationSemigroup, 
#"for a partial transformation semigroup", [IsPartialTransformationSemigroup],
#function(s)
#local t, hom;#

#t:=Semigroup(List(GeneratorsOfSemigroup(s), AsTransformation));

#hom:=SemigroupHomomorphismByFunctionNC(s, t, AsTransformation);

#SetInverseGeneralMapping(hom, SemigroupHomomorphismByFunctionNC(t, s, AsPartialTransformation));
#SetIsInjective(hom, true);
#SetIsSurjective(hom, true);
#SetIsTotal(hom, true);
#SetIsSingleValued(hom, true);

#return hom;
#end);

#############################################################################

InstallMethod(AsPartialTransformationNC, "for a transformation", 
[IsTransformation],
function(f)
return PartialTransformation(f![1]{[1..Length(f![1])-1]});
end);

#############################################################################

InstallMethod(AsPartialTransformation, "for a transformation",  
[IsTransformation],
function(f)

if not f![1][Length(f![1])]=Length(f![1]) then 
    Error("transformation does not fix ", DegreeOfTransformation(f));
fi;

return AsPartialTransformationNC(f);
end);

#############################################################################

InstallMethod(\*, "for a partial transformation and a partial transformation",
[IsPartialTransformation, IsPartialTransformation], 
function(f, g)
local img_f, img_g,img;

if Length(f!.img)=Length(g!.img) then 
	img_f:=Concatenation(f!.img, [Length(f!.img)+1]);
	img_g:=Concatenation(g!.img, [Length(g!.img)+1]);
	img:=img_g{img_f};
	
	return PartialTransformationNC(img{[1..Length(f!.img)]});
fi;
Info(InfoWarning, 1, "degrees of partial transformations are not equal");
return fail;
end);

#############################################################################

InstallMethod(\<, "for a partial transformation and a partial transformation", 
[IsPartialTransformation, IsPartialTransformation],
function(f, g)

return f!.img<g!.img;
end);

#############################################################################

InstallMethod(\=, "for a partial transformation and a partial transformation", 
[IsPartialTransformation, IsPartialTransformation],
function(f, g)
return f!.img=g!.img;
end);

#############################################################################

InstallMethod(\^, "for a partial transformation", 
[IsPosInt, IsPartialTransformation],
function(i, f)
return f!.img[i];
end);

#############################################################################

InstallMethod(One, "for a partial transformation", [IsPartialTransformation],
function(x)
return PartialTransformationNC([1..DegreeOfTransformation(x)]);
end);

#############################################################################

InstallOtherMethod(ZeroOp, "for a partial transformation", 
[IsPartialTransformation],
function(f)
return PartialTransformationNC(List([1..Length(f!.img)], x->Length(f!.img)+1));
end);

#############################################################################

InstallOtherMethod(MultiplicativeZero, "for a partial transformation", 
[IsPartialTransformation],
function(f)
return Zero(f);
end);

#############################################################################

InstallMethod(Size, "for a partial trans semigp", true, 
[IsPartialTransformationSemigroup], 0, 
function(S)
return Size(Range(IsomorphismTransformationSemigroup(S)));
end);

#############################################################################

InstallMethod(AsSSortedList, "for a partial trans semigp", true, 
[IsPartialTransformationSemigroup], 0, 
function(S)
return List(AsSSortedList(Range(IsomorphismTransformationSemigroup(S))), 
AsPartialTransformation);
end);

#############################################################################

InstallOtherMethod(AsPartialTransformation, "for a permutation",
[IS_PERM], x-> AsPartialTransformation(AsTransformation(x)));
