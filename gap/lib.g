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
   
  opts:=rec(schreier:=true, small:=false, hashlen:=rec(S:=1009, M:=25013, 
   L:=100003)); 
   
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
  local names, n, i, closure_opts, s, f;
  
  names:=RecNames(opts);

  if not "schreier" in names then 
    opts!.schreier:=true;
  fi;

  if not "small" in names then 
    opts!.small:=false;
  fi;
  
  if not "hashlen" in names then
    opts!.hashlen:=rec(S:=1009, M:=25013, L:=100003);
  elif IsPosInt(opts!.hashlen) then  
    n:=opts!.hashlen; 
    opts!.hashlen:=rec(S:=NextPrimeInt(n/100), M:=NextPrimeInt(n/4), 
     L:=NextPrimeInt(n));
  fi;

  if opts!.small then #small gen. set
    
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return Rank(x)>Rank(y); end);;

    n:=Length(gens[1]![1]);

    if gens[1]![1]=[1..n] and Rank(gens[2])=n then #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(schreier:=opts!.schreier, small:=false, 
     hashlen:=opts!.hashlen);
    s:=Monoid(gens[1], closure_opts);

    if InfoLevel(InfoCitrus)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then 
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
        if not opts!.schreier then 
          Print(", for \t", Size(OrbitsOfImages(s)), " elements\r");
        fi;
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then 
          s:=ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;    

  s:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagmaWithOne( s, AsList( gens ) );
  return s;
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
   
  opts:=rec(schreier:=true, small:=false, hashlen:=rec(S:=1009, M:=25013, 
   L:=100003)); 
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
  local names, n, i, closure_opts, s, f;

  names:=RecNames(opts);

  if not "schreier" in RecNames(opts) then 
    opts!.schreier:=true;
  fi;

  if not "small" in names then 
    opts!.small:=false;
  fi;

  if not "hashlen" in names then
    opts!.hashlen:=rec(S:=1009, M:=25013, L:=100003);
  elif IsPosInt(opts!.hashlen) then 
    n:=opts!.hashlen;
    opts!.hashlen:=rec(S:=NextPrimeInt(n/100), M:=NextPrimeInt(n/4),
     L:=NextPrimeInt(n));
  fi;

  if opts!.small then 
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return Rank(x)>Rank(y); end);;

    n:=Length(gens[1]![1]);

    if gens[1]![1]=[1..n] and Rank(gens[2])=n then #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(schreier:=opts!.schreier, small:=false,
     hashlen:=opts!.hashlen);
    s:=Semigroup(gens[1], closure_opts);

    if InfoLevel(InfoCitrus)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
        if not opts!.schreier then
          Print(", for \t", Size(OrbitsOfImages(s)), " elements\r");
        fi;
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then
          s:=ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;

  s:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagma( s, AsList( gens ) );
  return s;
end);

#EOF
