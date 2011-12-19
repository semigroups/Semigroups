############################################################################# 
## 
#W  semi.gi 
#Y  Copyright (C) 2011                                   James D. Mitchell 
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##


# new for 0.5! - Monoid 
##############################################################################

MakeReadWriteGlobal("Monoid");
UnbindGlobal("Monoid");

BindGlobal("Monoid", 
function ( arg )

  if Length( arg ) = 1 and IsMatrix( arg[1] )  then
    return MonoidByGenerators( [ arg[1] ] );
  elif Length( arg ) = 2 and IsMatrix( arg[1] )  then
    return MonoidByGenerators( arg );
  elif Length( arg ) = 1 and IsList( arg[1] ) and 0 < Length( arg[1] )  then
    return MonoidByGenerators( arg[1] );
  elif Length( arg ) = 2 and IsList( arg[1] )  then
    return MonoidByGenerators( arg[1], arg[2] );
  elif Length(arg) = 2 and IsTransformationCollection(arg[1]) and
   IsRecord(arg[2]) then 
    return MonoidByGenerators( arg[1], arg[2] );
  elif Length(arg) = 2 and IsTransformationCollection(arg[1]) and 
    IsTransformation(arg[2]) then 
    if IsTransformationMonoid(arg[1]) then 
      return MonoidByGenerators(Concatenation(Generators(arg[1]), [arg[2]]));
    fi;
    return MonoidByGenerators(Concatenation(arg[1], [arg[2]]));
  elif IsPermGroup(arg[1]) and IsTransformationCollection(arg[2]) then
    return MonoidByGenerators(Concatenation(List(GeneratorsOfGroup(arg[1]),
     x-> AsTransformation(x, Degree(arg[2][1]))), arg[2]));
  elif IsPermGroup(arg[1]) and IsTransformation(arg[2]) then 
    return MonoidByGenerators(Concatenation(List(GeneratorsOfGroup(arg[1]),  
         x-> AsTransformation(x, Degree(arg[2][1]))), [arg[2]]));
  elif 0 < Length( arg )  then
    return MonoidByGenerators( arg );
  else
    Error( "usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)" );
  fi;
  return;
end);

# new for 0.5! - MonoidByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "for a trans. collection",
[IsTransformationCollection],
function(gens)
  local opt, S;
   
  opt:=rec(schreier:=true); 
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - Monoid -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "for a trans. coll. and record",
[IsTransformationCollection, IsRecord],
function(gens, opt)
  local S;
  
  if not "schreier" in RecNames(opt) then 
    opt!.schreier:=true;
  fi;

  S:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - Semigroup
##############################################################################

MakeReadWriteGlobal("Semigroup");
UnbindGlobal("Semigroup");

BindGlobal("Semigroup", 
function ( arg )
  if Length( arg ) = 1 and IsMatrix( arg[1] )  then
    return SemigroupByGenerators( [ arg[1] ] );
  elif Length( arg ) = 1 and IsList( arg[1] ) and 0 < Length( arg[1] )  then
    return SemigroupByGenerators( arg[1] );
  elif Length(arg) = 2 and IsTransformationCollection(arg[1]) and
   IsRecord(arg[2]) then 
    return SemigroupByGenerators( arg[1], arg[2] );
  elif Length(arg) = 2 and IsTransformationCollection(arg[1]) and 
    IsTransformation(arg[2]) then 
    if IsTransformationSemigroup(arg[1]) then 
      return SemigroupByGenerators(Concatenation(Generators(arg[1]), [arg[2]]));
    fi;
    return SemigroupByGenerators(Concatenation(arg[1], [arg[2]]));
  elif IsPermGroup(arg[1]) and IsTransformationCollection(arg[2]) then
    return SemigroupByGenerators(Concatenation(List(GeneratorsOfGroup(arg[1]),
     x-> AsTransformation(x, Degree(arg[2][1]))), arg[2]));
  elif IsPermGroup(arg[1]) and IsTransformation(arg[2]) then 
    return SemigroupByGenerators(Concatenation(List(GeneratorsOfGroup(arg[1]),  
         x-> AsTransformation(x, Degree(arg[2][1]))), [arg[2]]));
  elif 0 < Length( arg )  then
    return SemigroupByGenerators( arg );
  else
    Error( "usage: Semigroup(<gen>,...),Semigroup(<gens>),Semigroup(<D>)");
  fi;
  return;
end);

# new for 0.5! - SemigroupByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "for a trans. collection",
[IsTransformationCollection],
function(gens)
  local opt, S;
   
  opt:=rec(schreier:=true); 
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - SemigroupByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "for a trans. collection and record",
[IsTransformationCollection, IsRecord],
function(gens, opt)
  local S;

  if not "schreier" in RecNames(opt) then 
    opt!.schreier:=true;
  fi;

  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opt:=opt));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

#EOF
