############################################################################# 
## 
#W  semi.gi 
#Y  Copyright (C) 2011                                   James D. Mitchell 
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# this file contains methods for lib functions that do not naturally fit in 
# any of the other files...

# new for 0.5! - Monoid 
##############################################################################

MakeReadWriteGlobal("Monoid");
UnbindGlobal("Monoid");

BindGlobal("Monoid", 
function ( arg )
  local out, i;

  if Length( arg ) = 1 and IsMatrix( arg[1] )  then
    return MonoidByGenerators( [ arg[1] ] );
  elif Length( arg ) = 2 and IsMatrix( arg[1] )  then
    return MonoidByGenerators( arg );
  elif Length( arg ) = 1 and IsList( arg[1] ) and 0 < Length( arg[1] )  then
    return MonoidByGenerators( arg[1] );
  elif Length( arg ) = 2 and IsList( arg[1] )  then
    return MonoidByGenerators( arg[1], arg[2] );
  elif IsTransformation(arg[1]) or IsTransformationCollection(arg[1]) then 
    out:=[];
    for i in [1..Length(arg)] do 
      if IsTransformation(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsTransformationCollection(arg[i]) then 
        if IsTransformationSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return MonoidByGenerators(Concatenation(out), arg[i]);
      else
        Error( "usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)" );
        return;
      fi;
    od;
    return MonoidByGenerators(Concatenation(out));
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
  local opts, S;
   
  opts:=rec(schreier:=true); 
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - MonoidByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "for a trans. coll. and record",
[IsTransformationCollection, IsRecord],
function(gens, opts)
  local S;
  
  if not "schreier" in RecNames(opts) then 
    opts!.schreier:=true;
  fi;

  S:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - Semigroup
##############################################################################

MakeReadWriteGlobal("Semigroup");
UnbindGlobal("Semigroup");

BindGlobal("Semigroup", 
function ( arg )
  local out, i;
  if Length( arg ) = 1 and IsMatrix( arg[1] )  then
    return SemigroupByGenerators( [ arg[1] ] );
  elif Length( arg ) = 1 and IsList( arg[1] ) and 0 < Length( arg[1] )  then
    return SemigroupByGenerators( arg[1] );
  elif IsTransformation(arg[1]) or IsTransformationCollection(arg[1]) then 
    out:=[];
    for i in [1..Length(arg)] do 
      if IsTransformation(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsTransformationCollection(arg[i]) then 
        if IsTransformationSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return SemigroupByGenerators(Concatenation(out), arg[i]);
      else
        Error( "usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)" );
        return;
      fi;
    od;
    return SemigroupByGenerators(Concatenation(out));
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
  local opts, S;
   
  opts:=rec(schreier:=true); 
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

# new for 0.5! - SemigroupByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "for a trans. collection and record",
[IsTransformationCollection, IsRecord],
function(gens, opts)
  local S;

  if not "schreier" in RecNames(opts) then 
    opts!.schreier:=true;
  fi;

  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

#EOF
