############################################################################# 
## 
#W  closure.gi 
#Y  Copyright (C) 2011                                   James D. Mitchell 
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# new for 0.5 - AddGensToOrbsOfImgs - not a user function!
#############################################################################
# Usage: s = old semigroup; t = new semigroup; new = new generators;
# j, k from O[j][k]

# JDM clean the following up! It's an extra mess due to the fact you can't make
# structural copies of hash tables!

InstallGlobalFunction(AddGensToOrbsOfImgs, 
function(s, t, new, j, k)
  local old_o, new_o, gens, filt, scc, r, ht, data, old_scc, old_reps, max, kernels_ht, h, val, o_t, images, f, d, n, reps, oo, z, m, i, g, y;

  old_o:=ImageOrbitFromData(s, [j,k]);
  new_o:=StructuralCopy(old_o);
  new_o!.ht:=StructuralCopy(old_o!.ht);

  AddGeneratorsToOrbit(new_o, new); gens:=new_o!.gens;
  
  Unbind(new_o!.truth); Unbind(new_o!.trees); Unbind(new_o!.reps); 
  new_o!.kernels_ht:=EmptyPlist(Length(old_o!.scc));
  new_o!.perms:=EmptyPlist(Length(new_o));

  filt:=function(o, scc) return not ForAny(OrbitsOfImages(t)!.orbits[j],
    x-> o[scc[1]] in x); end;

  if Length(old_o!.scc)>1 or Length(new_o)>Length(old_o) then 
    scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(
     OrbitGraphAsSets(new_o)), Set));;
    scc:=Filtered(scc, x-> filt(o,x));
  else
    scc:=StructuralCopy(old_o!.scc);
  fi;

  r:=Length(scc); new_o!.schutz:=List([1..r], x-> []);
  new_o!.reps:=List([1..r], x-> []); new_o!.scc:=scc;

  new_o!.truth:=List([1..r], i-> BlistList([1..Length(new_o)], scc[i]));
  new_o!.trees:=List([1..r], x-> CreateSchreierTreeOfSCC(new_o, x));

  OrbitsOfImages(t)!.orbits[j][k]:=new_o;
  ht:=OrbitsOfImages(t)!.ht; data:=OrbitsOfImages(t)!.data;

  for m in [1..r] do 
    old_scc:=Filtered([1..Length(old_o!.scc)], i-> old_o!.scc[i][1] in scc[m]);

    if not old_scc=[] then #scc contains an old scc
      old_reps:=StructuralCopy(old_o!.reps{old_scc});
      max:=List(old_scc, m-> Length(old_o!.reps[m]));
      max:=PositionProperty(old_scc, m-> Length(O!.reps[m])=Maximum(max));

      #kernels_ht:=O!.kernels_ht[old_scc[max]];
      kernels_ht:=HashTableForKernels(KernelOfTransformation(
       old_reps[max][1][1]));

      for i in [1..Length(old_reps[max])] do 
        HTAdd(kernels_ht, KernelOfTransformation(old_reps[max][i][1]), i);
      od;

      new_o!.kernels_ht[m]:=kernels_ht;
      data:=Concatenation(data, List([1..Length(old_reps[max])], val-> 
       [j, k, scc[m][1], m, val, 1]));

      new_o!.perms:=new_o!.perms+CreateImageOrbitSCCPerms(gens, o, m);
      #JDM use old perms as far as possible!
      for i in [1..Length(old_scc)] do 
        old_reps[i]:=List(old_reps[i], x-> 
         x*new_o!.perms[old_o!.scc[old_scc[i]][1]]);
      od;

      #old_reps[max]:=List(old_reps[max], x-> 
      # x*o!.perms[O!.scc[old_scc[max]][1]]);
      new_o!.schutz[m]:=CreateImageOrbitSchutzGp(gens, new_o,
       old_reps[max][1][1], m);
      #reuse old schutz gp! JDM prev.line was o!.reps[m][1][1]

      if Size(new_o!.schutz[m][2])=Size(old_o!.schutz[old_scc[max]][2]) then 
        new_o!.reps[m]:=old_reps[max];
        old_reps:=old_reps{Concatenation([1..max-1], 
         [max+1..Length(old_reps)])};
      else
        for i in [1..Length(old_reps[max])] do 
          new_o!.reps[m][Length(new_o!.reps[m])+1]:=[old_reps[max][i][1]];
          old_reps[1][i]:=old_reps[max][i]{[2..Length(old_reps[max][i])]};
        od;
      fi;

      for f in Concatenation(new_o!.reps[m]) do 
        for g in new do  
          h:=g*f;
          val:=HTValue(ht, h);
          if val=fail then 
            HTAdd(ht, h, true); ht!.new_o[Length(ht!.new_o)+1]:=h;
          fi;
        od;
      od;

#JDM here!

      if not old_reps=[] and not o!.schutz[m]=true then 

        o_t:=OrbitsOfImages(t)!.orbits; images:=OrbitsOfImages(t)!.images;
        old_reps:=List(old_reps, Concatenation); 

        for i in [1..Length(old_reps)] do
          for n in [1..Length(old_reps[i])] do 
            f:=old_reps[i][n];
            d:=InOrbitsOfImages(t, f, o_t, 
             [j, k, scc[m][1], m, fail, 0], images); 
            if not d[1] then #AddToOrbitsOfImages
              val:=d[2][5]; n:=d[2][6]; reps:=o!.reps[m];
              if not val=fail then #old kernel
                reps[val][n+1]:=f;
                data[Length(data)+1]:=[j, k, scc[m][1], m, val, n+1];
              else #new kernel
                val:=Length(reps)+1;
                reps[val]:=[f];
                data[Length(data)+1]:=[j, k, scc[m][1], m, val, 1];
                HTAdd(kernels_ht, KernelOfTransformation( f ), val);
              fi;

              for g in new do  
                h:=g*f;
                val:=HTValue(ht, h);
                if val=fail then 
                  HTAdd(ht, h, true);
                  ht!.o[Length(ht!.o)+1]:=h;
                fi;
              od;
            fi;
          od;
        od;
      fi;
    else #new points in the orbit!
      f:=o!.reps[1][1][1]*EvaluateWord(gens, 
      TraceSchreierTreeForward(o, scc[m][1]));
      o!.reps[m]:=[[f]];
      o!.kernels_ht[m]:=HashTableForKernels(
       KernelOfTransformation(f));
      o!.perms:=o!.perms+CreateImageOrbitSCCPerms(gens, o, m);
      o!.schutz[m]:=CreateImageOrbitSchutzGp(gens, o, f, m);

      #install descendants of f in OrbitsOfImages(t)!.ht
      ht:=OrbitsOfImages(t)!.ht; oo:=ht!.o;
      for y in [1..Length(gens)] do
        z:=gens[y]*f;
        if HTValue(ht, z)=fail then 
          HTAdd(ht, z, true);
          oo[Length(oo)+1]:=z;
          #schreier words here JDM
        fi;
      od;

      data[Length(data)+1]:=[j,k,scc[m][1],m,1,1];
    fi;
  od;
return true;
end);

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
  local t, o_s, o_t, j, n, orbits, gens, i, k, type, data, ht, val, g, h, f, l,
   o, d;

  if coll=[] then 
    return s;
  fi;

  gens:=Concatenation(Generators(s), coll);

  if IsTransformationMonoid(s) then 
    t:=Monoid(gens);
  else
    t:=Semigroup(gens);
  fi;

  if not HasOrbitsOfImages(s) then 
    return t;
  fi;

  # initialize the R-class reps orbit!

  coll:=List(coll, x-> x![1]);
  old_data:=OrbitsOfImages(s);
  
  n:=Degree(t);
  ht_len:=Maximum(CitrusHashLen!.rclassreps_orb, old_data!.ht!.len);
  
  ht:= HTCreate(new[1]);;
  HTAdd(ht, o_s!.one, true);

  for f in new do 
    HTAdd(ht, f, true);
  od;

  ht!.o:= Concatenation([o_s!.one], new); 

  for i in [o_s!.at+1..Length(o_s!.ht!.o)] do 
    g:=o_s!.ht!.o[i];
    val:=HTValue(ht, g);
    if val=fail then 
      HTAdd(ht, g, true);
      ht!.o[Length(ht!.o)+1]:=g;
    fi;
  od;

  n:=o_s!.deg;

  o_t:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
  rec( finished:=false,
    orbits:=EmptyPlist(n),
    lens:=List([1..n], x-> 0), 
    images:=HTCreate(ImageSetOfTransformation(new[1])),
    at:=0, 
    gens:=Generators(t),
    s:=t,
    deg := n, 
    one := o_s!.one,
    ht:=ht,
    data:=EmptyPlist(Length(o_s!.data)), 
    data_ht:=HTCreate([1,1,1,1,1,1])));

  SetOrbitsOfImages(t, o_t); 

  j:=Maximum(List(new, Rank));
  orbits:=o_t!.orbits;
  data:=o_t!.data;

  for i in [n,n-1..1] do 
    if IsBound(o_s!.orbits[i]) then 
      if i>j then 
        orbits[i]:=StructuralCopy(o_s!.orbits[i]);
        Append(data, RClassRepsDataFromOrbits(orbits[i], i));
#JDM could avoid using RClassRepsDataFromOrbits
# if the data was stored in the orbits when created. 
      else
        orbits[i]:=[];
        for k in [1..Length(o_s!.orbits[i])] do 
          AddGensToOrbsOfImgs(s, t, new, i, k);
        od;
      fi;
    fi;
  od;

return t;
end);

#EOF
