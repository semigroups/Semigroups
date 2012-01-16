############################################################################# 
## 
#W  semigroups.gi 
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

#CCC

# mod for 0.6! - ClosureSemigroup - "for a trans. semi. and trans. coll."
#############################################################################

InstallGlobalFunction(ClosureSemigroup,
function(arg)

  if not IsTransformationSemigroup(arg[1]) or not 
   (IsTransformationCollection(arg[2]) or IsTransformation(arg[2])) then 
    Error("Usage: arg. must be a trans. semigroup and transformation or ", 
    "collection of transformations,");
    return;
  fi;

  if Length(arg)=3 then 
    if not IsRecord(arg[3]) then 
      Error("Usage: the third argument must be a record,");
      return;
    fi;
    if IsBound(arg[3].schreier) then  
      arg[3].schreier:=false;
    fi;
  else
    arg[3]:=arg[1]!.opts;
  fi;

  arg[3].small:=false;

  if IsTransformationSemigroup(arg[2]) then 
    arg[2]:=Generators(arg[2]);
  elif IsTransformation(arg[2]) then 
    arg[2]:=[arg[2]];
  fi;

  if not Degree(arg[1])=Degree(arg[2][1]) then 
    Error("Usage: degrees of transformations must equal degree of semigroup,");
    return;
  fi;

  return ClosureSemigroupNC(arg[1], Filtered(arg[2], x-> not x in arg[1]), 
   arg[3]);
end);

# new for 0.5! - ClosureSemigroupNC - "for a trans. semi. and trans. coll."
#############################################################################
# Usage: s = a transformation semigroup; coll = a list of transformations not
# belonging to s but with degree equal to that of s.  

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll, opts)
  local t, old_data, n, max_rank, orbits, lens, data_ht, data, data_len, images, old_lens, old_orbits, gens, ht, o, r, j, scc, reps, out, old_reps, old_data_list, old_reps_len, old_o, new_data, d, g, m, z, i, k, val, y;
 
  if coll=[] then 
    Info(InfoCitrus, 2, "All the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;
  
  if IsTransformationMonoid(s) then 
    t:=Monoid(s, coll, opts);
  else
    t:=Semigroup(s, coll, opts);
  fi;

  if not HasOrbitsOfImages(s) or opts.schreier then 
    return t;
  fi;

  #no schreier###############################################################
  
  old_data:=OrbitsOfImages(s);
  n:=Degree(t);
  
  # set up orbits of images of t

  max_rank:=Maximum(List(coll, Rank)); 
  gens:=List(Generators(t), x-> x![1]);
  orbits:=EmptyPlist(n); 
  lens:=[1..n]*0;
  data_ht:=HTCreate([1,1,1,1,1,1], rec(forflatplainlists:=true,
   hashlen:=old_data!.data_ht!.len));
  data:=EmptyPlist(Length(old_data!.data)); 
  data_len:=0;
  images:=HTCreate(SSortedList(gens[1]), rec(forflatplainlists:=true,
   hashlen:=old_data!.images!.len));
  old_lens:=old_data!.lens; old_orbits:=old_data!.orbits;

  # initialize R-class reps orbit
  
  ht:=StructuralCopy(old_data!.ht); o:=ht!.o; 
  r:=Length(o);
  
  for i in [1..Length(coll)] do 
    j:=HTAdd(ht, coll[i]![1], r+i);
    o[r+i]:=ht!.els[j];
  od;
  
  # process orbits of large images
 
  for j in [n, n-1..max_rank+1] do
    if old_lens[j]>0 then
      lens[j]:=old_lens[j];
      orbits[j]:=EmptyPlist(lens[j]);
      for k in [1..lens[j]] do
        o:=StructuralCopy(old_orbits[j][k]);
        o!.onlygradesdata:=images;
        AddGeneratorsToOrbit(o, coll);
        scc:=o!.scc; reps:=o!.reps;

        for m in [1..Length(scc)] do
          for val in [1..Length(reps[m])] do
            for n in [1..Length(reps[m][val])] do
              data_len:=data_len+1;
              out:=[j, k, scc[m][1], m, val, n];
              HTAdd(data_ht, out, data_len);
              data[data_len]:=out;
            od;
          od;
        od;  
        for i in o do 
          HTAdd(images, i, k);
        od;  
        orbits[j][k]:=o;
      od;
    fi;
  od;

  # process orbits of small images

  old_reps:=EmptyPlist(Length(old_data!.data));
  old_data_list:=EmptyPlist(Length(old_data!.data));
  old_reps_len:=0;

  for j in [max_rank, max_rank-1..1] do 
    if old_lens[j]>0 then 
      orbits[j]:=[];
      for k in [1..old_lens[j]] do
        old_o:=old_orbits[j][k];
        if HTValue(images, old_o[1])=fail then 
          lens[j]:=lens[j]+1;
          o:=StructuralCopy(old_o);
          o!.onlygradesdata:=images;
          AddGeneratorsToOrbit(o, coll);
          Unbind(o!.scc); Unbind(o!.rev);

          r:=Length(OrbSCC(o));

          o!.trees:=EmptyPlist(r);
          o!.reverse:=EmptyPlist(r);
          o!.reps:=List([1..r], x-> []);
          o!.kernels_ht:=[];
          o!.perms:=EmptyPlist(Length(o));
          o!.schutz:=EmptyPlist(r);
          o!.nr_idempotents:=List([1..r], m-> []);  
          for i in o do 
            HTAdd(images, i, lens[j]);
          od;
          orbits[j][lens[j]]:=o;
        fi;
        Append(old_reps, Concatenation(Concatenation(old_o!.reps)));
      od;
    fi;
  od;

  # set orbits of images of t

  new_data:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
   rec(finished:=false, orbits:=orbits, lens:=lens, images:=images,
    at:=old_data!.at, gens:=gens, ht:=ht, data_ht:=data_ht, data:=data));

  SetOrbitsOfImages(t, new_data); 
 
  # process old R-reps 

  for i in [1..Length(old_reps)] do 
    j:=InOrbitsOfImages(old_reps[i], false, 
     [fail, fail, fail, fail, fail, 0, fail], orbits, images);
    if not j[1] then
      AddToOrbitsOfImages(t, old_reps[i], j[2], new_data, false);
    fi;
  od;
  
  # install new pts in the orbit
  
  coll:=List(coll, x-> x![1]); n:=Length(Generators(s)); 

  for i in [1..Length(data)] do 
    d:=data[i];
    g:=orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
    m:=Length(coll); j:=Length(ht!.o);
    for y in [1..m] do 
      z:=g{coll[y]};
      if HTValue(ht, z)=fail then
        j:=j+1; z:=HTAdd(ht, z, j); ht!.o[j]:=ht!.els[z];
      fi;
    od;
  od;
  
  # process kernel orbits here too!
  # if IsBound(OrbitsOfKernels(s)) then 
  # fi;

  return t;
end);

#MMM

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
        Error( "Usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)," );
        return;
      fi;
    od;
    return MonoidByGenerators(Concatenation(out));
  elif 0 < Length( arg )  then
    return MonoidByGenerators( arg );
  else
    Error( "Usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)," );
    return;
  fi;
end);

# new for 0.6! - MagmaByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(MagmaByGenerators, "(Citrus) for a trans. collection",
[IsTransformationCollection],
function(gens)
  local M;
   
  M:=Objectify( NewType( FamilyObj( gens ), 
   IsMagma and IsAttributeStoringRep ), rec(opts:=CitrusOptionsRec));

  SetGeneratorsOfMagma( M, AsList( gens ) );
  return M;
end);


# mod for 0.6! - MonoidByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "(Citrus) for a trans. collection",
[IsTransformationCollection],
function(gens)
  local S;
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opts:=CitrusOptionsRec));

  SetGeneratorsOfMagmaWithOne( S, AsList( gens ) );
  return S;
end);

# mod for 0.6! - MonoidByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(MonoidByGenerators, "(Citrus) for a trans. coll. and record",
[IsTransformationCollection, IsRecord],
function(gens, opts)
  local n, i, closure_opts, s, f;
  
  if not IsBound(opts.schreier) then 
    opts.schreier:=CitrusOptionsRec.schreier;
  fi;

  if not IsBound(opts.small) then 
    opts.small:=CitrusOptionsRec.small;
  fi;
  
  if not IsBound(opts.hashlen) then
    opts.hashlen:=CitrusOptionsRec.hashlen;
  elif IsPosInt(opts.hashlen) then  
    n:=opts.hashlen; 
    opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)), 
     L:=NextPrimeInt(n));
  elif not IsRecord(opts.hashlen) then 
    Error("the component hashlen should be a positive integer or a record,");
    return;
  fi;

  if opts.small then #small gen. set
    
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return Rank(x)>Rank(y); end);;

    n:=Length(gens[1]![1]);

    if gens[1]![1]=[1..n] and Rank(gens[2])=n then #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(schreier:=opts.schreier, small:=false, 
     hashlen:=opts.hashlen);
    s:=Monoid(gens[1], closure_opts);

    if InfoLevel(InfoCitrus)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then 
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
        if not opts.schreier then 
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

#SSS

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
        Error( "Usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)," );
        return;
      fi;
    od;
    return SemigroupByGenerators(Concatenation(out));
  elif 0 < Length( arg )  then
    return SemigroupByGenerators( arg );
  else
    Error( "Usage: Semigroup(<gen>,...),Semigroup(<gens>),Semigroup(<D>),");
    return;
  fi;
end);

# mod for 0.6! - SemigroupByGenerators -  "for a trans. collection"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "(Citrus) for a trans. collection",
[IsTransformationCollection],
function(gens)
  local S;
   
  S:=Objectify( NewType( FamilyObj( gens ), 
   IsSemigroup and IsAttributeStoringRep ), rec(opts:=CitrusOptionsRec));

  SetGeneratorsOfMagma( S, AsList( gens ) );
  return S;
end);

# mod for 0.5! - SemigroupByGenerators -  "for a trans. coll. and record"
##############################################################################

InstallOtherMethod(SemigroupByGenerators, "(Citrus) for trans coll and record",
[IsTransformationCollection, IsRecord],
function(gens, opts)
  local n, i, closure_opts, s, f;

  if not IsBound(opts.schreier) then 
    opts.schreier:=CitrusOptionsRec.schreier;
  fi;

  if not IsBound(opts.small) then 
    opts.small:=CitrusOptionsRec.schreier;
  fi;

  if not IsBound(opts.hashlen) then
    opts.hashlen:=CitrusOptionsRec.hashlen;
  elif IsPosInt(opts.hashlen) then 
    n:=opts.hashlen;
    opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)),
     L:=NextPrimeInt(n));
  elif not IsRecord(opts.hashlen) then 
    Error("the component hashlen should be a positive integer or a record,");
    return;
  fi;

  if opts.small then 
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return Rank(x)>Rank(y); end);;

    n:=Length(gens[1]![1]);

    if gens[1]![1]=[1..n] and Rank(gens[2])=n then #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(schreier:=opts.schreier, small:=false,
     hashlen:=opts.hashlen);
    s:=Semigroup(gens[1], closure_opts);

    if InfoLevel(InfoCitrus)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
        if not opts.schreier then
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
