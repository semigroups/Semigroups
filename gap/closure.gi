############################################################################# 
## 
#W  closure.gi 
#Y  Copyright (C) 2011                                   James D. Mitchell 
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# new for 0.5! - ClosureSemigroup - "for a trans. semi. and trans. coll."
#############################################################################

InstallGlobalFunction(ClosureSemigroup,
function(s, coll)

  if not IsTransformationSemigroup(s) or not (IsTransformationCollection(coll)
   or IsTransformation(coll)) then 
    Error("Usage: arg. must be a trans. semigroup and transformation or ", 
    "collection of transformations.");
    return fail;
  fi;

  if IsTransformationSemigroup(coll) then 
    coll:=Generators(coll);
  elif IsTransformation(coll) then 
    coll:=[coll];
  fi;

  if not Degree(s)=Degree(coll[1]) then 
    Error("Usage: degrees of transformations must equal degree of semigroup");
    return fail;
  fi;

  return ClosureSemigroupNC(s, Filtered(coll, x-> not x in s));
end);

# new for 0.5! - ClosureSemigroupNC - "for a trans. semi. and trans. coll."
#############################################################################
# Usage: s = a transformation semigroup; coll = a list of transformations not
# belonging to s but with degree equal to that of s.  

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll)
  local t, old_data, ht, o, r, img_lists, j, max_rank, n, orbits, lens, data_ht, data, data_len, images, scc, reps, out, old_reps, old_o, new_data, d, g, m, z, i, k, val, y;
  
  if coll=[] then 
    return s;
  fi;

  if IsTransformationMonoid(s) then 
    t:=Monoid(Concatenation(Generators(s), coll));
  else
    t:=Semigroup(Concatenation(Generators(s), coll));
  fi;

  if not HasOrbitsOfImages(s) then 
    return t;
  fi;

  # initialize R-class reps orbit

  old_data:=OrbitsOfImages(s);
  
  ht:=StructuralCopy(old_data!.ht); o:=ht!.o; r:=Length(o);
  img_lists:=List(Generators(t), x-> x![1]);
  
  for i in [1..Length(coll)] do 
    j:=HTAdd(ht, coll[i]![1], r+i);
    o[r+i]:=ht!.els[j];
  od;

  # set up orbits of images of t

  max_rank:=Maximum(List(coll, Rank));
  
  n:=Degree(t);
  orbits:=EmptyPlist(n); 
  lens:=[1..n]*0;
  data_ht:=HTCreate([1,1,1,1,1,1], rec(forflatplainlists:=true,
   hashlen:=old_data!.data_ht!.len));
  data:=EmptyPlist(Length(old_data!.data)); 
  data_len:=0;
  images:=HTCreate(SSortedList(img_lists[1]), rec(forflatplainlists:=true,
   hashlen:=old_data!.images!.len));
 
  # process orbits of large images
 
  for j in [n, n-1..max_rank+1] do
    if old_data!.lens[j]>0 then
      lens[j]:=old_data!.lens[j];
      orbits[j]:=EmptyPlist(lens[j]);
      for k in [1..lens[j]] do
        o:=StructuralCopy(old_data!.orbits[j][k]);
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

  for j in [max_rank, max_rank-1..1] do 
    if old_data!.lens[j]>0 then 
      orbits[j]:=[];
      for k in [1..old_data!.lens[j]] do
        old_o:=old_data!.orbits[j][k];
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
    at:=old_data!.at, gens:=img_lists,
    ht:=ht, data_ht:=data_ht, data:=data,
    gen1:=[], pos1:=[], gen2:=[], pos2:=[]));

  SetOrbitsOfImages(t, new_data); 
 
  # process old R-reps 

  for i in old_reps do 
    d:=InOrbitsOfImages(i, false, [fail, fail, fail, fail, fail, 0, fail],
           orbits, images);
    if not d[1] then 
      AddToOrbitsOfImages(t, i, d[2], new_data);
    fi;
  od;
  
  # install new pts in the orbit
  
  coll:=List(coll, x-> x![1]); 

  for i in new_data!.data do 
    g:=orbits[i[1]][i[2]]!.reps[i[4]][i[5]][i[6]];
    m:=Length(coll); j:=Length(ht!.o);
    for y in [1..m] do 
      z:=g{coll[y]};
      if HTValue(ht, z)=fail then
        j:=j+1; z:=HTAdd(ht, z, j); ht!.o[j]:=ht!.els[z];
      fi;
    od;
  od;

  return t;
end);

#EOF
