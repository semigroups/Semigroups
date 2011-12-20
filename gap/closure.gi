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
  local info, t, old_data, ht, o, r, img_lists, j, max_rank, n, orbits, lens, data_ht, data, data_len, images, old_gen1, old_gen2, old_pos1, old_pos2, gen1, pos1, gen2, pos2, old_lens, old_orbits, scc, reps, out, old_rep_nr, l, old_reps, old_data_list, old_reps_len, old_o, new_data, d, g, m, z, i, k, val, y;
 
  if InfoLevel(InfoCitrus)>1 then 
    info:=true;
  else
    info:=false;
  fi;

  if coll=[] then 
    Info(InfoCitrus, 2, "All the elements in the collection belong to the ",
    " semigroup.");
    return s;
  fi;

  if IsTransformationMonoid(s) then 
    t:=Monoid(Concatenation(Generators(s), coll));
  else
    t:=Semigroup(Concatenation(Generators(s), coll));
  fi;

  if not HasOrbitsOfImages(s) then 
    Info(InfoCitrus, 2, "No data known about old semigroup.");
    return t;
  fi;

  if s!.opts!.schreier then 
    Error("not yet implemented");
  fi;

  # initialize R-class reps orbit

  old_data:=OrbitsOfImages(s);
  
  ht:=StructuralCopy(old_data!.ht); o:=ht!.o; 
  r:=Length(o);
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
  
  old_gen1:=old_data!.gen1; old_gen2:=old_data!.gen2;
  old_pos1:=old_data!.pos1; old_pos2:=old_data!.pos2;
  gen1:=EmptyPlist(Length(old_gen1));
  l:=Positions(old_gen1, fail);
  gen1{l}:=ListWithIdenticalEntries(Length(l), fail);
  pos1:=EmptyPlist(Length(old_pos1));
  pos1{l}:=ListWithIdenticalEntries(Length(l), fail);
  gen2:=EmptyPlist(Length(old_gen2));
  pos2:=EmptyPlist(Length(old_pos2));

  old_lens:=old_data!.lens; old_orbits:=old_data!.orbits;

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
              
              old_rep_nr:=HTValue(old_data!.data_ht, out);
              l:=Positions(old_pos1, old_rep_nr);#JDM suboptimal
              pos1{l}:=ListWithIdenticalEntries(Length(l), data_len);
              gen1{l}:=old_gen1{l};
              pos2[data_len]:=old_pos2[old_rep_nr];
              gen2[data_len]:=old_gen2[old_rep_nr];
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
        #Append(old_reps, Concatenation(Concatenation(old_o!.reps)));
        reps:=old_o!.reps; scc:=old_o!.scc;
        for m in [1..Length(scc)] do 
          for val in [1..Length(reps[m])] do 
            for n in [1..Length(reps[m][val])] do 
              old_reps_len:=old_reps_len+1;
              old_data_list[old_reps_len]:=[j,k,scc[m][1],m,val,n];
              old_reps[old_reps_len]:=reps[m][val][n];
            od;
          od;
        od;
      od;
    fi;
  od;

  # set orbits of images of t

  new_data:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
   rec(finished:=false, orbits:=orbits, lens:=lens, images:=images,
    at:=old_data!.at, gens:=img_lists,
    ht:=ht, data_ht:=data_ht, data:=data,
    gen1:=gen1, pos1:=pos1, gen2:=gen2, pos2:=pos2));

  SetOrbitsOfImages(t, new_data); 
 
  # process old R-reps 

  for i in [1..Length(old_reps)] do 
    j:=InOrbitsOfImages(old_reps[i], false, 
     [fail, fail, fail, fail, fail, 0, fail], orbits, images);
    if not j[1] then
      data_len:=data_len+1;
      old_rep_nr:=HTValue(old_data!.data_ht, old_data_list[i]);
      l:=Positions(old_pos1, old_rep_nr);#JDM suboptimal
      pos1{l}:=ListWithIdenticalEntries(Length(l), data_len); #JDM this might
      #not work
      gen1{l}:=old_gen1{l}; 
      pos2[data_len]:=old_pos2[old_rep_nr];
      gen2[data_len]:=j[2]{[1..4]};
      
      AddToOrbitsOfImages(t, old_reps[i], j[2], new_data);
    fi;
  od;
  
  # install new pts in the orbit
  
  coll:=List(coll, x-> x![1]); n:=Length(Generators(s)); 

  Error("");
  for i in [1..Length(data)] do 
    d:=data[i];
    g:=orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
    m:=Length(coll); j:=Length(ht!.o);
    for y in [1..m] do 
      z:=g{coll[y]};
      if HTValue(ht, z)=fail then
        j:=j+1; z:=HTAdd(ht, z, j); ht!.o[j]:=ht!.els[z];
        pos1[j]:=i; gen1[j]:=r+y;
      fi;
    od;
  od;

  # process kernel orbits here too!

  return t;
end);

#EOF
