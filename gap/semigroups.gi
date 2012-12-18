############################################################################# 
## 
#W  semigroups.gi 
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

InstallMethod(\=, "for an acting semigp and acting semigp",
[IsActingSemigroup and HasGeneratorsOfSemigroup,
IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  return ForAll(Generators(s), x-> x in t) and
   ForAll(Generators(t), x-> x in s);
end);

#

InstallOtherMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection],
function(s, coll) 
  return ClosureInverseSemigroup(s, coll, s!.opts);
end);

#

InstallMethod(ClosureInverseSemigroup, 
"for an acting semigroup with inverse op, ass. elt. coll, and record",
[IsActingSemigroupWithInverseOp, IsAssociativeElementCollection, IsRecord],
function(s, coll, record)
  local n;
  
  if not IsActingElementWithInverseOpCollection(coll) then 
    Error("usage: the second argument should be a collection of acting",
    " elements with an inverse op,");
    return;
  fi;

  if IsSemigroup(coll) then 
    coll:=GeneratorsOfSemigroup(coll);
  fi;

  return ClosureInverseSemigroupNC(s, Filtered(coll, x-> not x in s),
   SemigroupOptions(record));
end);

#

InstallGlobalFunction(ClosureInverseSemigroupNC,
function(s, coll, opts)
  local t, coll_copy, o, f;
 
  if coll=[] then
    Info(InfoSemigroups, 2, "All the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;

  o:=LongOrb(s);
  
  if not IsSubset(o[1], DomainOfPartialPermCollection(coll)) then 
    #the original LongOrb seed is wrong
    return InverseSemigroup(Concatenation(Generators(s), coll), opts);
  fi;

  coll_copy:=ShallowCopy(coll);
  for f in coll do
    if not DomPP(f)=RanSetPP(f) then 
      Add(coll_copy, f^-1);
    fi;
  od;  
  
  o:=StructuralCopy(o);
  AddGeneratorsToOrbit(o, coll_copy);

  t:=InverseSemigroupByGeneratorsNC(o!.gens, 
   Concatenation(Generators(s), coll), opts);

  if IsBound(o!.scc) then 
    Unbind(o!.scc); Unbind(o!.truth); Unbind(o!.trees); Unbind(o!.scc_lookup);
  fi;
  
  if IsBound(o!.mults) then 
    Unbind(o!.mults); 
  fi;
  
  if IsBound(o!.schutz) then 
    Unbind(o!.schutz);
  fi;
  
  o!.finished:=false;
  SetLongOrb(t, o);
  return t;
end);

#

InstallGlobalFunction(ClosureSemigroup,
function(arg)

  if not ( IsActingSemigroup(arg[1]) 
      or IsActingElementCollection(arg[2]) 
      or IsAssociativeElement(arg[2])) then 
    Error("usage: the first argument should be an acting semigroup and the\n",
    "second argument an element or collection of elements,");
    return;
  fi;

  if Length(arg)=3 then 
    if not IsRecord(arg[3]) then 
      Error("usage: the third argument must be a record,");
      return;
    fi;
  else
    arg[3]:=arg[1]!.opts;
  fi;

  arg[3].small:=false;

  if IsActingSemigroup(arg[2]) then 
    arg[2]:=Generators(arg[2]);
  elif IsAssociativeElement(arg[2]) then 
    arg[2]:=[arg[2]];
  fi;

  if not ActionDegree(arg[1])=ActionDegree(arg[2][1]) then 
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
    Info(InfoSemigroups, 2, "All the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;
  
  if IsTransformationMonoid(s) then 
    t:=Monoid(s, coll, opts);
  else
    t:=Semigroup(s, coll, opts);
  fi;

  #no schreier###############################################################
  
  #old_data:=OrbitsOfImages(s);
  #n:=LambdaDegree(t);
  
  # set up orbits of images of t

  max_rank:=Maximum(List(coll, ActionRank)); 
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

  #new_data:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
  # rec(finished:=false, orbits:=orbits, lens:=lens, images:=images,
  #  at:=old_data!.at, gens:=gens, ht:=ht, data_ht:=data_ht, data:=data));

  #SetOrbitsOfImages(t, new_data); 
 
  # process old R-reps 

  for i in [1..Length(old_reps)] do 
    #j:=InOrbitsOfImages(old_reps[i], false, 
    # [fail, fail, fail, fail, fail, 0, fail], orbits, images);
    #if not j[1] then
    #  AddToOrbitsOfImages(t, old_reps[i], j[2], new_data, false);
    #fi;
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

  return t;
end);

#

InstallOtherMethod(Generators, "for a semigroup or monoid",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if IsInverseSemigroup(s) then 
    if IsMonoid(s) then 
      return GeneratorsOfInverseMonoid(s);
    fi;
    return GeneratorsOfInverseSemigroup(s);
  fi;

  if IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

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
  elif IsAssociativeElement(arg[1]) or IsAssociativeElementCollection(arg[1])
   then 
    out:=[];
    for i in [1..Length(arg)] do 
      if IsAssociativeElement(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsAssociativeElementCollection(arg[i]) then 
        if IsSemigroup(arg[i]) then
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

#

InstallMethod(MagmaByGenerators, "for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  local M;
  
  M:=Objectify( NewType( FamilyObj( gens ), 
   IsMagma and IsAttributeStoringRep ), rec(opts:=SemigroupsOptionsRec));

  SetGeneratorsOfMagma( M, AsList( gens ) );
  return M;
end);

#

InstallOtherMethod(MonoidByGenerators, "for an associative element collection",
[IsAssociativeElementCollection],
function(gens)
  return MonoidByGenerators(gens, SemigroupsOptionsRec);
end);

# mod for 0.6! - MonoidByGenerators -  for an acting elt collection and rec
##############################################################################

InstallOtherMethod(MonoidByGenerators, "for an acting elt collection and rec",
[IsAssociativeElementCollection, IsRecord],
function(gens, opts)
  local n, i, closure_opts, s, f;

  if not IsActingElementCollection(gens) then 
    TryNextMethod();
  fi;

  if not IsBound(opts.regular) then 
    opts.regular:=SemigroupsOptionsRec.regular;
  fi;
  
  if not IsBound(opts.acting) and IsActingElementCollection(gens) then 
    opts.acting:=SemigroupsOptionsRec.acting;
  fi;
 
  if not IsBound(opts.small) then 
    opts.small:=SemigroupsOptionsRec.small;
  fi;
  
  if not IsBound(opts.hashlen) then
    opts.hashlen:=SemigroupsOptionsRec.hashlen;
  elif IsPosInt(opts.hashlen) then  
    n:=opts.hashlen; 
    opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)), 
     L:=NextPrimeInt(n));
  elif not IsRecord(opts.hashlen) then 
    Error("the component hashlen should be a positive integer or a record,");
    return;
  fi;

  if opts.small and Length(gens)>1 then #small gen. set
    
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;

    n:=Length(gens[1]![1]);

    if gens[1]![1]=[1..n] and ActionRank(gens[2])=n then #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=Monoid(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then 
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
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

  if opts.regular then 
    SetIsRegularSemigroup(s, true);
  fi;

  if IsBound(opts.acting) and opts.acting then 
    SetIsActingSemigroup(s, true);
  fi;

  s:=Objectify( NewType( FamilyObj( gens ), 
   IsMonoid and IsAttributeStoringRep ), rec(opts:=opts));

  SetGeneratorsOfMagmaWithOne( s, AsList( gens ) );
  SetIsActingSemigroup(s, true);
  return s;
end);

#III

# new for 0.7! - InverseMonoid
##############################################################################

if Semigroups_C then 
  InstallGlobalFunction(InverseMonoid,
  function( arg )
    local out, i;

    if IsPartialPerm(arg[1]) or IsPartialPermCollection(arg[1]) then 
      out:=[]; 
      for i in [1..Length(arg)] do 
        if IsPartialPerm(arg[i]) then 
          out[i]:=[arg[i]];
        elif IsPartialPermCollection(arg[i]) then 
          if IsPartialPermSemigroup(arg[i]) then
            out[i]:=Generators(arg[i]);
          else
            out[i]:=arg[i];
          fi;
        elif i=Length(arg) and IsRecord(arg[i]) then 
          return InverseMonoidByGenerators(Concatenation(out), arg[i]);
        else
          Error( "usage: InverseMonoid(<gen>,...), InverseMonoid(<gens>),"
          ,  "InverseMonoid(<D>)," );
          return;
        fi;
      od;
      return InverseMonoidByGenerators(Concatenation(out));
    fi;
    Error( "usage: InverseMonoid(<gen>,...),InverseMonoid(<gens>),",
     "InverseMonoid(<D>),");
    return;
  end);
else 
  InstallGlobalFunction(InverseMonoid, 
  function(arg) return SemigroupsIsNotCompiled(); end);
fi;

# new for 0.7! - InverseSemigroup
##############################################################################

if Semigroups_C then 
  InstallGlobalFunction(InverseSemigroup,
  function( arg )
    local out, i;

    if IsPartialPerm(arg[1]) or IsPartialPermCollection(arg[1]) then 
      out:=[]; 
      for i in [1..Length(arg)] do 
        if IsPartialPerm(arg[i]) then 
          out[i]:=[arg[i]];
        elif IsPartialPermCollection(arg[i]) then 
          if IsPartialPermSemigroup(arg[i]) then
            out[i]:=Generators(arg[i]);
          else
            out[i]:=arg[i];
          fi;
        elif i=Length(arg) and IsRecord(arg[i]) then 
          return InverseSemigroupByGenerators(Concatenation(out), arg[i]);
        else
          Error( "usage: InverseSemigroup(<gen>,...), InverseSemigroup(<gens>),"
          ,  "InverseSemigroup(<D>)," );
          return;
        fi;
      od;
      return InverseSemigroupByGenerators(Concatenation(out));
    fi;
    Error( "usage: InverseSemigroup(<gen>,...),InverseSemigroup(<gens>),",
     "InverseSemigroup(<D>),");
    return;
  end);
else 
  InstallGlobalFunction(InverseSemigroup, 
  function(arg) return SemigroupsIsNotCompiled(); end);
fi;

# new for 0.7! - InverseMonoidByGenerators
################################################################################

if IsBound(DomPP) and IsBound(RanSetPP) then 
  InstallMethod(InverseMonoidByGenerators, "for partial perm coll", 
  [IsPartialPermCollection],
  function(coll)
    local one, gens, f;
  
    gens:=ShallowCopy(coll);

    for f in coll do
      if not DomPP(f)=RanSetPP(f) then 
        Add(gens, f^-1);
      fi;
    od;
  
    return InverseMonoidByGeneratorsNC(gens, coll, SemigroupsOptionsRec);
  end);
fi;

# new for 0.7! - InverseSemigroupByGenerators
################################################################################

if IsBound(DomPP) and IsBound(RanSetPP) then 
  InstallMethod(InverseSemigroupByGenerators, "for partial perm coll", 
  [IsPartialPermCollection],
  function(coll)
    local gens, f;

    gens:=ShallowCopy(coll);
    
    for f in coll do
      if not DomPP(f)=RanSetPP(f) then 
        Add(gens, f^-1);
      fi;
    od;

    return InverseSemigroupByGeneratorsNC(gens, coll, SemigroupsOptionsRec);
  end);
fi;

# new for 0.7! - InverseMonoidByGenerators - "for partial perm coll and
# record"
################################################################################

if IsBound(DomPP) and IsBound(RanSetPP) then 
  InstallOtherMethod(InverseMonoidByGenerators, 
  "for partial perm coll and record",
  [IsPartialPermCollection, IsRecord],
  function(coll, opts)
    local n, one, f, gens;
    
    if not IsBound(opts.small) then
      opts.small:=SemigroupsOptionsRec.small;
    fi;

    if not IsBound(opts.hashlen) then
      opts.hashlen:=SemigroupsOptionsRec.hashlen;
    elif IsPosInt(opts.hashlen) then
      n:=opts.hashlen;
      opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)),
       L:=NextPrimeInt(n));
    elif not IsRecord(opts.hashlen) then
      Error("the component hashlen should be a positive integer or a record,");
      return;
    fi;

    if not opts.small then
      gens:=ShallowCopy(coll);
        
      for f in coll do
        if not DomPP(f)=RanSetPP(f) then
          Add(gens, f^-1); 
        fi;
      od;
    else
      gens:=coll;
    fi;

    return InverseMonoidByGeneratorsNC(gens, coll, opts);
  end);
fi;

# new for 0.7! - InverseSemigroupByGenerators - "for partial perm coll and
# record"
################################################################################

if IsBound(DomPP) and IsBound(RanSetPP) then 
  InstallOtherMethod(InverseSemigroupByGenerators, 
  "for partial perm coll and record",
  [IsPartialPermCollection, IsRecord],
  function(coll, opts)
    local n, f, gens;

    if not IsBound(opts.small) then
      opts.small:=SemigroupsOptionsRec.small;
    fi;

    if not IsBound(opts.hashlen) then
      opts.hashlen:=SemigroupsOptionsRec.hashlen;
    elif IsPosInt(opts.hashlen) then
      n:=opts.hashlen;
      opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)),
       L:=NextPrimeInt(n));
    elif not IsRecord(opts.hashlen) then
      Error("the component hashlen should be a positive integer or a record,");
      return;
    fi;

    if not opts.small then
      gens:=ShallowCopy(coll);
        
      for f in coll do
        if not DomPP(f)=RanSetPP(f) then
          Add(gens, f^-1);
        fi;
      od;
    else
      gens:=coll;
    fi;

    return InverseSemigroupByGeneratorsNC(gens, coll, opts);
  end);
fi;

# new for 0.7! - InverseMonoidByGeneratorsNC
################################################################################

if IsBound(DomPP) then 
  InstallMethod(InverseMonoidByGeneratorsNC, 
  "for partial perm coll, partial perm coll, and record",
  [IsPartialPermCollection, IsPartialPermCollection, IsRecord],
  function(gens, coll, opts)
    local i, closure_opts, s, f;

    if opts.small and Length(gens)>1 then 
      coll:=SSortedList(ShallowCopy(coll));
      coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));;
      Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);;
      
      closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
      s:=InverseMonoid(coll[1], closure_opts);
      
      for f in coll do
        if not f in s then 
          s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
        fi;
      od;
      return s;
    fi;

    s:=Objectify( NewType (FamilyObj( gens ), IsMagmaWithOne and
     IsInverseSemigroup and IsAttributeStoringRep), rec(opts:=opts));
    SetDomainOfPartialPermCollection(s, Union(List(gens, DomPP)));
    SetRangeOfPartialPermCollection(s, DomainOfPartialPermCollection(s));
    SetGeneratorsOfMagmaWithOne(s, gens);
    SetGeneratorsOfInverseSemigroup(s, Concatenation([One(s)], coll));
    SetGeneratorsOfInverseMonoid(s, coll);
    SetIsActingSemigroup(s, true);
    return s;
  end);
fi;

# new for 0.7! - InverseSemigroupByGeneratorsNC
################################################################################

InstallMethod(InverseSemigroupByGeneratorsNC, 
"for partial perm coll, partial perm coll, and record",
[IsPartialPermCollection, IsPartialPermCollection, IsRecord],
function(gens, coll, opts)
  local i, closure_opts, s, f;

  if opts.small and Length(gens)>1 then 
    coll:=SSortedList(ShallowCopy(coll));
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));;
    Sort(coll, function(x, y) return x[2]>y[2]; end);;
    
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=InverseSemigroup(coll[1], closure_opts);
    
    for f in coll do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), IsMagma and IsInverseSemigroup
  and IsAttributeStoringRep), rec(opts:=opts));
  SetGeneratorsOfMagma(s, gens);
  SetGeneratorsOfInverseSemigroup(s, coll);
  SetIsActingSemigroup(s, true);
  return s;
end);

# move this to the library too!

# mod for 1.0! - IsSubsemigroup - "for an acting semigroup and acting semigroup"
################################################################################

InstallOtherMethod(IsSubsemigroup, "for acting semigroup and acting semigroup",
[IsActingSemigroup, IsActingSemigroup],
function(s, t)
  return ForAll(GeneratorsOfSemigroup(t), x-> x in s);
end);

# new for 0.7! - IsSubsemigroup - "for partial perm semi and inv semigroup"
################################################################################
 
InstallOtherMethod(IsSubsemigroup, 
"for a partial perm semi and inverse  semigroup of partial perms",
[IsPartialPermSemigroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(s, t)
  return ForAll(Generators(t), x-> x in s);
end);

## new for 0.2! - IsSubset - "for acting semi and acting elt coll"
###########################################################################

InstallOtherMethod(IsSubset, "for trans. semi. and trans. coll",
[IsActingSemigroup, IsActingElementCollection],
function(s, coll)
  return ForAll(coll, x-> x in s);
end);

#PPP 

# new for 0.7! - PrintObj - "for an inverse monoid"
################################################################################

InstallMethod(PrintObj, "for an inverse monoid",
[IsInverseMonoid], 
function(s)
  Print("<inverse monoid with ", Length(Generators(s)), " generators");
  if HasSize(s) then 
    Print(", ", Size(s), " elements");
  fi;
  Print(">");
  return;
end);

# new for 0.7! - PrintObj - "for an inverse semigroup"
################################################################################

InstallMethod(PrintObj, "for an inverse semigroup",
[IsInverseSemigroup], 
function(s)
  Print("<inverse semigroup with ", Length(Generators(s)), " generators");
  if HasSize(s) then 
    Print(", ", Size(s), " elements");
  fi;
  Print(">");
  return;
end);

#RRR

InstallMethod(RandomBipartitionSemigroup, "for a pos int and pos int",
[IsPosInt, IsPosInt],
function(m, n)
  return Semigroup(List([1..m], x-> RandomBipartition(n)));
end);

InstallMethod(RandomBipartitionMonoid, "for a pos int and pos int",
[IsPosInt, IsPosInt],
function(m, n)
  return Monoid(List([1..m], x-> RandomBipartition(n)));
end);

InstallMethod(RandomMatrixSemigroup, "for a ring and pos int",
[IsRing, IsPosInt, IsPosInt], 
function(R, m, n)
  return Semigroup(List([1..m], x-> RandomMat(n, n, R)));
end);

# new for 1.0! - RandomBinaryRelationMonoid - "for pos int and pos int"
#############################################################################

InstallMethod(RandomBinaryRelationMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  local s;

  s:=Monoid(List([1..m], x-> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true); 
  return s;
end);

# new for 1.0! - RandomBinaryRelationSemigroup - "for pos int and pos int"
#############################################################################

InstallMethod(RandomBinaryRelationSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  local s;

  s:=Semigroup(List([1..m], x-> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true);
  return s;
end);

# new for 1.0! - RandomBlockGroup - for a pos int and pos int
#############################################################################

if Semigroups_C then 
  InstallMethod(RandomBlockGroup, "for pos int and pos int",
  [IsPosInt, IsPosInt],
  function(m,n)
    return Semigroup(Set(List([1..m], x-> RandomPartialPerm(n))));
  end);
else
  InstallMethod(RandomBlockGroup, "for pos int and pos int",
  [IsPosInt, IsPosInt], SemigroupsIsNotCompiled);
fi;

# new for 0.7! - RandomInverseMonoid - for a pos int and pos int
#############################################################################

if Semigroups_C then 
  InstallMethod(RandomInverseMonoid, "for pos int and pos int",
  [IsPosInt, IsPosInt],
  function(m,n)
    return InverseMonoid(Set(List([1..m], x-> RandomPartialPerm(n))));
  end);
else
  InstallMethod(RandomInverseMonoid, "for pos int and pos int",
  [IsPosInt, IsPosInt], SemigroupsIsNotCompiled);
fi;

# new for 0.7! - RandomInverseSemigp - for a pos int and pos int
#############################################################################

if Semigroups_C then 
  InstallMethod(RandomInverseSemigroup, "for pos int and pos int",
  [IsPosInt, IsPosInt],
  function(m,n)
    return InverseSemigroup(Set(List([1..m], x-> RandomPartialPerm(n))));
  end);
else
  InstallMethod(RandomInverseSemigroup, "for pos int and pos int",
  [IsPosInt, IsPosInt], SemigroupsIsNotCompiled);
fi;

# mod for 0.8! - RandomTransformationSemigroup 
#############################################################################

InstallMethod(RandomTransformationSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt], 
function(m,n)
  return Semigroup(Set(List([1..m], x-> RandomTransformation(n))));
end);

# mod for 0.8! - RandomTransformationSemigroup 
###########################################################################

InstallMethod(RandomTransformationMonoid, "for a pos int and pos int",
[IsPosInt, IsPosInt], 
function(m,n)
  return Monoid(Set(List([1..m], x-> RandomTransformation(n))));
end);

InstallGlobalFunction(RegularSemigroup, 
function(arg)
  Add(arg, rec(regular:=true));
  return CallFuncList(Semigroup, arg);
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
  elif IsAssociativeElement(arg[1]) or IsAssociativeElementCollection(arg[1]) then 
    out:=[];
    for i in [1..Length(arg)] do 
      if IsAssociativeElement(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsAssociativeElementCollection(arg[i]) then 
        if IsSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return SemigroupByGenerators(Concatenation(out), arg[i]);
      else
        Error( "Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>)," );
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

#

InstallOtherMethod(SemigroupByGenerators, "for an acting elt collection",
[IsActingElementCollection],
function(gens)
   return SemigroupByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallOtherMethod(SemigroupByGenerators, 
"for an associative element collection and record (Semigroups package)",
[IsActingElementCollection, IsRecord],
function(gens, opts)
  local n, i, closure_opts, s, f;

  if not IsBound(opts.regular) then 
    opts.regular:=SemigroupsOptionsRec.regular;
  fi;

  if not IsBound(opts.acting) and IsActingElementCollection(gens) then 
    opts.acting:=SemigroupsOptionsRec.acting;
  fi;
  
  if not IsBound(opts.small) then 
    opts.small:=SemigroupsOptionsRec.small;
  fi;

  if not IsBound(opts.hashlen) then
    opts.hashlen:=SemigroupsOptionsRec.hashlen;
  elif IsPosInt(opts.hashlen) then 
    n:=opts.hashlen;
    opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)),
     L:=NextPrimeInt(n));
  elif not IsRecord(opts.hashlen) then 
    Error("the component hashlen should be a positive integer or a record,");
    return;
  fi;

  if opts.small and Length(gens)>1 then 
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;

    n:=ActionDegree(gens);

    #remove the identity
    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2])=n then
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=Semigroup(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
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
  
  if opts.regular then 
    SetIsRegularSemigroup(s, true);
  fi;
 
  if IsBound(opts.acting) and opts.acting then 
    SetIsActingSemigroup(s, true);
  fi;

  SetGeneratorsOfMagma( s, AsList( gens ) );
  return s;
end);

# new for 0.8! - SubsemigroupByProperty - "for a trans. semi. and func"
################################################################################

InstallMethod(SubsemigroupByProperty, "for a trans. semi. and func",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction], 
function(S, func)
  local limit, n;

  if HasSize(S) then 
    limit:=Size(S);
  else
    n:=ActionDegree(S);
    limit:=n^n;
  fi;

  return SubsemigroupByProperty(S, func, limit);
end);


# new for 0.8! - SubsemigroupByProperty - "for a trans. semi., func, rec"
################################################################################

InstallOtherMethod(SubsemigroupByProperty, "for a trans. semi., func, rec",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
 
  iter:=Iterator(S);
  T:=Semigroup(NextIterator(iter));
  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureSemigroup(T, f);
    fi;
  od;

  return T;
end);

# new for 0.8! - SubsemigroupByProperty - "for a part perm semi. and func"
################################################################################

InstallMethod(SubsemigroupByProperty, "for a part perm semi. and func",
[IsPartialPermSemigroup and IsInverseSemigroup, IsFunction], 
function(S, func)
  local limit, n;

  if HasSize(S) then 
    limit:=Size(S);
  else
    n:=NrMovedPoints(S);
    limit:=Sum(List([0..n], r-> Binomial(n,r)^2*Factorial(r)));
  fi;

  return SubsemigroupByProperty(S, func, limit);
end);

# new for 0.8! - SubsemigroupByProperty - "for a part perm semi., func, rec"
################################################################################

InstallOtherMethod(SubsemigroupByProperty, "for a part perm semi, func, rec",
[IsPartialPermSemigroup and IsInverseSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
  
  iter:=Iterator(S);
  T:=InverseSemigroup(NextIterator(iter));
  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureInverseSemigroup(T, f);
    fi;
  od;
  return T;
end);

# new for 1.0! - ViewObj - "for an acting semigroup"
################################################################################

InstallMethod(ViewObj, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 10,
function(s)
  local n;
 
  if not (IsPartialPermSemigroup(s) or IsTransformationSemigroup(s) or 
    IsBipartitionSemigroup(s) or IsMatrixSemigroup(s) or
    IsBinaryRelationSemigroup(s)) then 
    TryNextMethod();
  fi;

  if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    Print("<inverse ");
  elif HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
    Print("<regular ");
  else
    Print("<");
  fi;
  
  if IsPartialPermSemigroup(s) then 
    Print("partial perm ");
  elif IsTransformationSemigroup(s) then 
    Print("transformation ");
  elif IsBipartitionSemigroup(s) then 
    Print("bipartition ");
  elif IsMatrixSemigroup(s) then 
    Print("matrix ");
  elif IsBinaryRelationSemigroup(s) then 
    Print("binary relation ");
  fi;
  
  if IsMonoid(s) then 
    Print("monoid ");
  else 
    Print("semigroup ");
  fi;

  if IsTransformationSemigroup(s) then 
    Print("of degree ", DegreeOfTransformationSemigroup(s), " ");
  elif IsPartialPermSemigroup(s) then 
    Print("of degree ", DegreeOfPartialPermSemigroup(s), " ");
  elif IsMatrixSemigroup(s) then
    n:=Length(GeneratorsOfSemigroup(s)[1][1]);
    Print(n, "x", n, " over ", BaseDomain(GeneratorsOfSemigroup(s)[1][1]), " ");
  elif IsBipartitionSemigroup(s) then 
    Print("of degree ", DegreeOfBipartitionSemigroup(s)/2, " "); 
  elif IsBinaryRelationSemigroup(s) then 
    Print("of degree ", Length(Successors(Generators(s)[1])), " ");
  fi;

  Print("with ", Length(Generators(s)));
  Print(" generator");

  if Length(Generators(s))>1 then 
    Print("s");
  fi;
  Print(">");
end);

#EOF
