#
###########################################################################
##
#W  setup.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

###############################################################################
# Setup - install the basic things required for specific acting semigroups    #
###############################################################################

# IsGeneratorsOfActingSemigroup

InstallMethod(IsGeneratorsOfActingSemigroup, 
"for an associative element collection",
[IsAssociativeElementCollection], ReturnFalse);

# In the below can't do ReturnTrue, since GAP insists that we use
# InstallTrueMethod.
#
# InstallTrueMethod(IsGeneratorsOfActingSemigroup, IsTransformationCollection);
# 
# can't do InstallTrueMethod for the above since this is not picked up 
# if Semigroups is loaded after any transformation semigroup has been created.
# It seems that since IsTransformationCollection has had its implied filters
# installed, if we add an additional implied filter
# IsGeneratorsOfActingSemigroup, then this is ignored. I think this is a bug.

InstallMethod(IsGeneratorsOfActingSemigroup, "for a transformation collection", 
[IsTransformationCollection], x-> true);

InstallMethod(IsGeneratorsOfActingSemigroup, "for a partial perm collection", 
[IsPartialPermCollection], x-> true);

InstallMethod(IsGeneratorsOfActingSemigroup, "for a bipartition collection", 
[IsBipartitionCollection], x-> true);

InstallMethod(IsGeneratorsOfActingSemigroup, 
"for a Rees 0-matrix semigroup element collection", 
[IsReesZeroMatrixSemigroupElementCollection],
function(coll)
  local R;
  R:=ReesMatrixSemigroupOfFamily(FamilyObj(Representative(coll)));
  return IsGroup(UnderlyingSemigroup(R)) and IsRegularSemigroup(R);
end);

# IsActingSemigroupWithInverseOp

InstallTrueMethod(IsInverseSemigroup, IsActingSemigroupWithInverseOp);
InstallTrueMethod(IsActingSemigroupWithInverseOp, IsInverseSemigroup and IsPartialPermSemigroup);
InstallTrueMethod(IsActingSemigroupWithInverseOp, IsInverseSemigroup and IsBlockBijectionSemigroup);
InstallTrueMethod(IsActingSemigroupWithInverseOp, IsInverseSemigroup and
IsPartialPermBipartitionSemigroup);

#InstallTrueMethod(IsActingSemigroup, IsReesZeroMatrixSemigroup);

# the largest point involved in the action

InstallMethod(ActionDegree, "for a transformation",
[IsTransformation], DegreeOfTransformation);

InstallMethod(ActionDegree, "for a partial perm",
[IsPartialPerm], x-> Maximum(DegreeOfPartialPerm(x), 
CodegreeOfPartialPerm(x)));

InstallMethod(ActionDegree, "for a bipartition",
[IsBipartition], DegreeOfBipartition);

InstallMethod(ActionDegree, "for a Rees 0-matrix semigroup element", 
[IsReesZeroMatrixSemigroupElement], 
function(x)
  if x![1]=0 then 
    return 0; 
  else 
    return NrMovedPoints(x![2])+1; 
  fi; 
end);

#

InstallMethod(ActionDegree, "for a transformation collection",
[IsTransformationCollection], DegreeOfTransformationCollection);

InstallMethod(ActionDegree, "for a partial perm collection",
[IsPartialPermCollection], x-> Maximum(DegreeOfPartialPermCollection(x), 
CodegreeOfPartialPermCollection(x)));

InstallMethod(ActionDegree, "for a bipartition collection",
[IsBipartitionCollection], DegreeOfBipartitionCollection);

InstallMethod(ActionDegree, "for a Rees 0-matrix semigroup element collection",
[IsReesZeroMatrixSemigroupElementCollection],           
function(coll)
  local R;
  if ForAny(coll, x-> x![1]<>0) then 
    R:=ReesMatrixSemigroupOfFamily(FamilyObj(Representative(coll)));
    return NrMovedPoints(UnderlyingSemigroup(R))+1;
  else
    return 0;
  fi;
end);

#

InstallMethod(ActionDegree, "for a transformation semigroup",
[IsTransformationSemigroup], DegreeOfTransformationSemigroup);

InstallMethod(ActionDegree, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> Maximum(DegreeOfPartialPermSemigroup(x), 
CodegreeOfPartialPermSemigroup(x)));

InstallMethod(ActionDegree, "for a partial perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], DegreeOfPartialPermSemigroup);

InstallMethod(ActionDegree, "for a bipartition semigroup",
[IsBipartitionSemigroup], DegreeOfBipartitionSemigroup);

InstallMethod(ActionDegree, "for a Rees 0-matrix subsemigroup with generators",
[IsReesZeroMatrixSubsemigroup and HasGeneratorsOfSemigroup],
function(R) 
  if ForAny(GeneratorsOfSemigroup(R), x-> x![1]<>0) then 
    return NrMovedPoints(UnderlyingSemigroup(
      ReesMatrixSemigroupOfFamily(ElementsFamily(FamilyObj(R)))))+1;
  else
    return 0;
  fi;
end);


# the number of points in the range of the action

InstallMethod(ActionRank, "for a transformation and positive integer",
[IsTransformation, IsInt], RANK_TRANS_INT);

InstallMethod(ActionRank, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
  local deg;
  deg:=DegreeOfTransformationSemigroup(s);
  return function(f)
    return RANK_TRANS_INT(f, deg);
  end;
end);

InstallMethod(ActionRank, "for a partial perm and positive integer",
[IsPartialPerm, IsInt], 
function(f, n)
  return RankOfPartialPerm(f);
end);

InstallMethod(ActionRank, "for a partial perm semigroup",
[IsPartialPermSemigroup], 
function(s)
  return RankOfPartialPerm;
end);

InstallMethod(ActionRank, "for a bipartition",
[IsBipartition, IsInt], 
function(f, n)
  return RankOfBipartition(f);
end);

InstallMethod(ActionRank, "for a bipartition semigroup",
[IsBipartitionSemigroup], 
function(s)
  return RankOfBipartition;
end);

InstallMethod(ActionRank, "for a Rees 0-matrix semigroup element", 
[IsReesZeroMatrixSemigroupElement, IsInt],
function(f, n)
  if f![1]=0 then 
    return 0;
  else
    return NrMovedPoints(UnderlyingSemigroup(
      ReesMatrixSemigroupOfFamily(FamilyObj(f))))+1; 
  fi;
end);

InstallMethod(ActionRank, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], 
function(R)
  local U;
  return function(x)
    if x![1]=0 then 
      return 0;
    else
      return NrMovedPoints(UnderlyingSemigroup(
       ReesMatrixSemigroupOfFamily(ElementsFamily(FamilyObj(R)))))+1; 
    fi;
  end;
end);

# the minimum possible rank of an element

InstallMethod(MinActionRank, "for a transformation semigroup",
[IsTransformationSemigroup], x-> 1);

InstallMethod(MinActionRank, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> 0);

InstallMethod(MinActionRank, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> 0);

InstallMethod(MinActionRank, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], x-> 0);

# options passed to LambdaOrb(s) when it is created

InstallMethod(LambdaOrbOpts, "for a transformation semigroup",
[IsTransformationSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> rec());

InstallMethod(LambdaOrbOpts, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], s-> rec());

InstallMethod(RhoOrbOpts, "for a transformation semigroup",
[IsTransformationSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(RhoOrbOpts, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(RhoOrbOpts, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> rec());

InstallMethod(RhoOrbOpts, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], s-> rec());

# the lambda and rho acts

InstallMethod(LambdaAct, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(S)
  local deg;
  deg:=DegreeOfTransformationSemigroup(S);
  return 
    function(set, f) 
      return OnPosIntSetsTrans(set, f, deg);
    end;
end);

InstallMethod(LambdaAct, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> OnPosIntSetsPartialPerm);

InstallMethod(LambdaAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnRightBlocks);

InstallMethod(LambdaAct, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], x-> function(pt, x)
  if x![1]=0 or pt=0 then 
    return 0;
  elif pt=-1 or x![4][pt][x![1]]<>0 then 
    return x![3];
  else
    return 0;
  fi;
end);

#

InstallMethod(RhoAct, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(S)
  local deg;
  deg:=DegreeOfTransformationSemigroup(S);
  return 
    function(set, f) 
      return ON_KERNEL_ANTI_ACTION(set, f, deg);
    end;
end);

# JDM new c method for this!
InstallMethod(RhoAct, "for a partial perm semigroup",
[IsPartialPermSemigroup], s->       
  function(set, f) 
    return OnPosIntSetsPartialPerm(set, f^-1);
  end);

InstallMethod(RhoAct, "for a partial perm semigroup",
[IsBipartitionSemigroup], x-> OnLeftBlocks);

InstallMethod(RhoAct, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], x-> function(pt, x)
  if x![1]=0 or pt=0 then 
    return 0;
  elif pt=-1 or x![4][x![3]][pt]<>0 then 
    return x![1];
  else
    return 0;
  fi;
end);

# the seed or dummy start point for LambdaOrb

InstallMethod(LambdaOrbSeed, "for a transformation semigroup",
[IsTransformationSemigroup], s-> [0]);

InstallMethod(LambdaOrbSeed, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> [0]);

InstallMethod(LambdaOrbSeed, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> EmptyBlocks);

InstallMethod(LambdaOrbSeed, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], s-> -1);

# the seed or dummy start point for RhoOrb

InstallMethod(RhoOrbSeed, "for a transformation semigroup",
[IsTransformationSemigroup], s-> [0]);

InstallMethod(RhoOrbSeed, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> [0]);

InstallMethod(RhoOrbSeed, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> EmptyBlocks);

InstallMethod(RhoOrbSeed, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], s-> -1);

# the function calculating the lambda or rho value of an element

InstallMethod(LambdaFunc, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(S)
  local deg;
  deg:=DegreeOfTransformationSemigroup(S);
  return 
    function(f)
      return IMAGE_SET_TRANS_INT(f, deg);
    end;
end);

InstallMethod(LambdaFunc, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> IMAGE_SET_PPERM);

InstallMethod(LambdaFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> RightBlocks);

InstallMethod(LambdaFunc, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], R-> function(x)
  if x![1]<>0 then 
    return x![3];
  else 
    return 0;
  fi;
end);

#

InstallMethod(RhoFunc, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(S)
  local deg;
  deg:=DegreeOfTransformationSemigroup(S);
  return 
    function(f)
      return FLAT_KERNEL_TRANS_INT(f, deg);
    end;
end);

InstallMethod(RhoFunc, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> DOMAIN_PPERM);

InstallMethod(RhoFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> LeftBlocks);

InstallMethod(RhoFunc, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], R->(x-> x![1]));

# the function used to calculate the rank of lambda or rho value

InstallMethod(LambdaRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a partial perm semigroup", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a bipartition semigroup", 
[IsBipartitionSemigroup], x-> RankOfBlocks);

InstallMethod(LambdaRank, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], R-> 
function(x)
  if x=0 then 
    return 0;
  else 
    return
     NrMovedPoints(UnderlyingSemigroup(
      ReesMatrixSemigroupOfFamily(ElementsFamily(FamilyObj(R)))))+1; 
  fi;
end);

#

InstallMethod(RhoRank, "for a transformation semigroup", 
[IsTransformationSemigroup], S-> function(x)
  if IsEmpty(x) then 
    return 0;
  else 
    return MaximumList(x);
  fi;
end);

InstallMethod(RhoRank, "for a partial perm semigroup", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(RhoRank, "for a bipartition semigroup", 
[IsBipartitionSemigroup], x-> RankOfBlocks);

InstallMethod(RhoRank, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], R->  
function(x) 
  if x=0 then 
    return 0;
  else 
    return 
     NrMovedPoints(UnderlyingSemigroup(
      ReesMatrixSemigroupOfFamily(ElementsFamily(FamilyObj(R)))))+1; 
  fi;
end);

# if g=LambdaInverse(X, f) and X^f=Y, then Y^g=X and g acts on the right 
# like the inverse of f on Y.

InstallMethod(LambdaInverse, "for a transformation semigroup",
[IsTransformationSemigroup], s-> INV_LIST_TRANS);

InstallMethod(LambdaInverse, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> function(x, f) return f^-1; end); 

InstallMethod(LambdaInverse, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> InverseRightBlocks); 

InstallMethod(LambdaInverse, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], s-> 
function(k, x)
  local i;
  if x![1]=0 or k=0 then 
    return x;
  fi;
  i:=First([1..Length(x![4][x![3]])], i-> x![4][x![3]][i]<>0);
  return Objectify(FamilyObj(x)!.type, 
   [i, (x![4][k][x![1]]*x![2]*x![4][x![3]][i])^-1, k, x![4]]);
end);

# if g=RhoInverse(X, f) and f^X=Y (this is a left action), then g^Y=X and g
# acts on the left like the inverse of g on Y. 

InstallMethod(RhoInverse, "for a transformation semigroup",
[IsTransformationSemigroup], s-> INV_KER_TRANS);

InstallMethod(RhoInverse, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> 
  function(dom, f)
    return f^-1;
  end);

#JDM better method for this!!

InstallMethod(RhoInverse, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], s-> 
function(k, x)
  local i;
  if x![1]=0 or k=0 then 
    return x;
  fi;
  i:=First([1..Length(x![4])], i-> x![4][i][x![1]]<>0);
  return Objectify(FamilyObj(x)!.type, 
    [k, (x![4][i][x![1]]*x![2]*x![4][x![3]][k])^-1, i, x![4]]);
end);

InstallMethod(RhoInverse, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> InverseLeftBlocks);

# LambdaPerm(s) returns a permutation from two acting semigroup elements with
# equal LambdaFunc and RhoFunc. This is required to check if one of the two
# elements belongs to the schutz gp of a lambda orb.

InstallMethod(LambdaPerm, "for a transformation semigroup",
[IsTransformationSemigroup], s-> PERM_LEFT_QUO_TRANS_NC);

InstallMethod(LambdaPerm, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> PERM_LEFT_QUO_PPERM_NC);

InstallMethod(LambdaPerm, "for a partial perm semigroup",
[IsBipartitionSemigroup], s-> PermLeftQuoBipartitionNC);

InstallMethod(LambdaPerm, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], s-> 
function(x, y)
  if x![1]=0 or y![1]=0 then 
    return ();
  fi;
  return x![2]^-1*y![2];
end);

# returns a permutation mapping LambdaFunc(s)(f) to LambdaFunc(s)(g) so that 
# gf^-1(i)=p(i) when RhoFunc(s)(f)=RhoFunc(s)(g)!!

InstallMethod(LambdaConjugator, "for a transformation semigroup",
[IsTransformationSemigroup], s-> TRANS_IMG_CONJ);

# c method
InstallMethod(LambdaConjugator, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> 
function(f, g)
  return MappingPermListList(IMAGE_PPERM(f), IMAGE_PPERM(g));
end);

InstallMethod(LambdaConjugator, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> BipartRightBlocksConj);

InstallMethod(LambdaConjugator, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], s-> 
function(f, g) 
  return ();
end);

# the function used to test if there is an idempotent with the specified 
# lambda and rho values.

InstallMethod(IdempotentTester, "for a transformation semigroup", 
[IsTransformationSemigroup], s-> 
function(img, ker)
  if IsEmpty(img) then 
    return IsEmpty(ker);
  fi;
  return IS_INJECTIVE_LIST_TRANS(img, ker) and Length(img)=MaximumList(ker);
end);

InstallMethod(IdempotentTester, "for a partial perm semigroup", 
[IsPartialPermSemigroup], s-> EQ);

InstallMethod(IdempotentTester, "for a bipartition semigroup", 
[IsBipartitionSemigroup], s-> BlocksIdempotentTester);

InstallMethod(IdempotentTester, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], R-> 
function(j,i)
  if i=0 and j=0 then 
    return true;
  fi;
  return Matrix(ReesMatrixSemigroupOfFamily(
   ElementsFamily(FamilyObj(R))))[j][i]<>0;
end);

# the function used to create an idempotent with the specified lambda and rho
# values. 

InstallMethod(IdempotentCreator, "for a transformation semigroup",
[IsTransformationSemigroup], s-> IDEM_IMG_KER_NC);

InstallMethod(IdempotentCreator, "for a partial perm semigp",
[IsPartialPermSemigroup], s-> PartialPermNC);

InstallMethod(IdempotentCreator, "for a bipartition semigroup", 
[IsBipartitionSemigroup], s-> BlocksIdempotentCreator);

InstallMethod(IdempotentCreator, "for a Rees 0-matrix subsemigroup", 
[IsReesZeroMatrixSubsemigroup], R-> 
function(j,i)
  local mat;
  if i=0 and j=0 then 
    return Objectify(TypeReesMatrixSemigroupElements(R), [0]);
  fi;
  mat:=Matrix(ReesMatrixSemigroupOfFamily(ElementsFamily(FamilyObj(R))));
  return Objectify(TypeReesMatrixSemigroupElements(R), 
     [i, mat[j][i]^-1, j, mat]);
end);

# the action of elements of the stabiliser of a lambda-value on any element of
# the semigroup with that lambda-value 

# StabilizerAction will be \* for transformation and partial perm semigroups 
# and something else for semigroups of bipartitions.

InstallMethod(StabilizerAction, "for a transformation semigroup",
[IsTransformationSemigroup], s-> PROD);

InstallMethod(StabilizerAction, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> PROD);

InstallMethod(StabilizerAction, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> OnRightBlocksBipartitionByPerm);

InstallMethod(StabilizerAction, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], s-> 
function(x, p)

  if x![1]=0 then 
    return x;
  fi;
  return Objectify(TypeObj(x), [x![1], x![2]*p, x![3], x![4]]);
end);

# IsActingSemigroupWithFixedDegreeMultiplication should be <true> if and only
# if it is only possible to multiply elements of the type in the semigroup with
# equal degrees.

InstallMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
"for a transformation semigroup", [IsTransformationSemigroup], ReturnFalse);

InstallTrueMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
IsBipartitionSemigroup);

InstallMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
"for a partial perm semigroup", [IsPartialPermSemigroup], ReturnFalse);

#this is not really relevant here.
InstallMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
"for a Rees 0-matrix subsemigroup", [IsReesZeroMatrixSubsemigroup], ReturnFalse);

# One or a fake one for those types of object without one.

InstallMethod(FakeOne, "for a transformation collection", 
[IsTransformationCollection], One);

InstallMethod(FakeOne, "for a partial perm collection", 
[IsPartialPermCollection], One);

InstallMethod(FakeOne, "for a bipartition collection", 
[IsBipartitionCollection], One);

InstallMethod(FakeOne, "for a Rees 0-matrix semigroup element collection",
[IsReesZeroMatrixSemigroupElementCollection],  R-> UniversalFakeOne);

# missing hash functions

InstallMethod(ChooseHashFunction, "for a Rees 0-matrix semigroup element",
[IsReesZeroMatrixSemigroupElement, IsInt],
  function(x, hashlen)
  return rec( func := ORB_HashFunctionReesZeroMatrixSemigroupElements, 
              data := hashlen );
end);

InstallGlobalFunction(ORB_HashFunctionReesZeroMatrixSemigroupElements, 
function(x, data)
  local p, l;
  
  if x![1]=0 then 
    return 1;
  fi;
  
  p:=x![2];
  l:=LARGEST_MOVED_POINT_PERM(p);
  
  if IsPerm4Rep(p) then
    # is it a proper 4byte perm?
    if l>65536 then
      return (x![1]+x![3]+HashKeyBag(p,255,0,4*l)) mod data + 1;
    else
      # the permutation does not require 4 bytes. Trim in two
      # byte representation (we need to do this to get consistent
      # hash keys, regardless of representation.)
      TRIM_PERM(p,l);
    fi;
  fi;
  # now we have a Perm2Rep:
  return (x![1]+x![3]+HashKeyBag(p,255,0,2*l)) mod data + 1; 
end);

#EOF
