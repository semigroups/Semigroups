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
  fi;

  if IsTransformationSemigroup(coll) then 
    coll:=Generators(coll);
  elif IsTransformation(coll) then 
    coll:=[coll];
  fi;

return ClosureSemigroupNC(s, Filtered(coll, x-> not x in s));
end);

# new for 0.5! - ClosureSemigroupNC - "for a trans. semi. and trans. coll."
#############################################################################

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll)
  local t, old_data, ht, o, r, j, n, max_rank, orbits, lens, images, data_ht, 
  data, data_len, old_reps, old_o, m, val, out, new_data, gen1, pos1, gen2, 
  pos2, d, i, k, old_m;

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

  for i in [1..Length(coll)] do 
    j:=HTAdd(ht, coll[i]![1], r+i);
    o[r+i]:=ht!.els[j];
  od;

  # process existing orbits

  n:=Degree(t);
  max_rank:=Maximum(List(coll, Rank));
  orbits:=EmptyPlist(n); lens:=[1..n]*0;
  
  for j in [max_rank+1..n] do
    if old_data!.lens[j]>0 then 
      lens[j]:=old_data!.lens[j];
      orbits[j]:=old_data!.orbits[j];
    fi;
  od;

  images:=StructuralCopy(old_data!.images);
  data_ht:=HTCreate([1,1,1,1,1,1], rec(forflatplainlists:=true,
   hashlen:=old_data!.data_ht!.len));
  data:=EmptyPlist(Length(old_data!.data)); data_len:=0; 
  old_reps:=EmptyPlist(Length(old_data!.data));

  for j in [1..max_rank] do 
    if IsBound(old_data!.orbits[j]) then 
      orbits[j]:=[];
      for k in [1..old_data!.lens[j]] do 
        old_o:=old_data!.orbits[j][k];
        o:=StructuralCopy(old_o); 
        o!.onlygradesdata:=images;
        AddGeneratorsToOrbit(o, coll);
        lens[j]:=lens[j]+1;
        
        Unbind(o!.scc); 
        r:=Length(OrbSCC(o));
        
        o!.trees:=EmptyPlist(r); 
        o!.reverse:=EmptyPlist(r);
        o!.reps:=List([1..r], x-> []);
        o!.kernels_ht:=[];
        o!.perms:=EmptyPlist(Length(o));
        o!.schutz:=EmptyPlist(r);
        o!.nr_idempotents:=List([1..r], m-> []);
          
        for old_m in [1..Length(old_o!.scc)] do 
          m:=Position(o!.scc, old_o!.scc[old_m]);
          if not old_m=fail then 
            o!.reps[m]:=old_o!.reps[old_m];
            for val in [1..Length(o!.reps[m])] do
              for n in [1..Length(o!.reps[m][val])] do 
                data_len:=data_len+1; 
                out:=[j, k, o!.scc[m][1], m, val, n]; 
                HTAdd(data_ht, out, data_len);
                data[data_len]:=out;
              od;
            od;

            o!.kernels_ht:=old_o!.kernels_ht[old_m];
            for i in o!.scc[m] do 
              o!.perms[i]:=old_o!.perms[i];
            od;
            o!.schutz[m]:=old_o!.schutz[old_m];
            o!.trees[m]:=old_o!.trees[old_m];
            o!.reverse[m]:=old_o!.reverse[old_m];
          else
            Append(old_reps, Flat(old_o!.reps[old_m]));
          fi;
        od;

        for i in o do 
          if HTValue(images, i)=fail then 
            HTAdd(images, i, lens[j]);
          fi;
        od;
        orbits[j][lens[j]]:=o;
      od;
    fi;
  od;

  # set orbits of images of t
  new_data:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
   rec(finished:=false, orbits:=orbits, lens:=lens, images:=images,
    at:=old_data!.at, gens:=List(Generators(t), x-> x![1]),
    ht:=ht, data_ht:=data_ht, data:=data,
    gen1:=[], pos1:=[], gen2:=[], pos2:=[]));

  SetOrbitsOfImages(t, new_data); 
 
  # process existing R-reps 

  for i in old_reps do 
    d:=InOrbitsOfImages(i, false, [fail, fail, fail, fail, fail, 0, fail],
           orbits, images);
    if not d[1] then 
      #Add(O!.pos2, i); Add(O!.gen2, d[2]{[1..4]});
      AddToOrbitsOfImages(t, i, d[2], new_data);
    fi;
  od;

  return t;
end);

#EOF=
