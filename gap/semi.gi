############################################################################# 
## 
#W  semi.gi 
#Y  Copyright (C) 2011                                   James D. Mitchell 
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# new for 0.5! - MonoidByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "for a trans. collection",
[IsTransformationCollection],
function(gens)
  local opt, S;
   
  opt:=rec(schreier:=true, smallgenset:=false); 
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - MonoidByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "for a trans. coll. and record",
[IsTransformationCollection, IsRecord],
function(gens, opt)
  local S;
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - SemigroupByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "for a trans. collection",
[IsTransformationCollection],
function(gens)
  local opt, S;
   
  opt:=rec(schreier:=true, smallgenset:=false); 
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - SemigroupByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "for a trans. collection",
[IsTransformationCollection, IsRecord],
function(gens, opt)
  local S;
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

#EOF
