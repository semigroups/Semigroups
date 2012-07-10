#############################################################################
##
#W  r.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#FFF

# mod for 0.5! - Factorization - "for a trans. semigp. and trans."
#############################################################################

InstallOtherMethod(Factorization, "for a trans. semigroup and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
function(s, f)
  local data, l, o, rep, p, w, g, q;
 
  if not f in s then 
    Error("transformation is not an element of the semigroup,");
    return;
  fi;
 
  if not s!.opts!.schreier then 
    Error("it is not possible to factorize elements of this semigroup,");
    return;
  fi;

  data:=PreInOrbitsOfImages(s, f, false)[2];

  l:=data[3]; o:=ImageOrbitFromData(s, data);
  data[3]:=ImageOrbitSCCFromData(s, data)[1]; #JDM hack rectify!
  rep:=RClassRepFromData(s, data); p:=data[8];

  if p=fail then 
    p:=PermLeftQuoTransformationNC(rep![1], data[7]);
  fi;

  if l=data[3] and p=() then # f is an R-class rep!
    Info(InfoCitrus, 2, "transformation is an R-class representative.");
    return TraceRClassRepsTree(s, RClassIndexFromData(s, data));
  fi;
  
  if not l=data[3] then 
    w:=TraceSchreierTreeOfSCCForward(o, data[4], l);
    g:=EvaluateWord(Generators(s), w);
    q:=PermLeftQuoTransformationNC(rep*g*ImageOrbitPermsFromData(s, data)[l],
     rep); # would be good to remove this step!
  else
    w:=[]; q:=();
  fi;

  if p*q=() then 
    return Concatenation(TraceRClassRepsTree(s, RClassIndexFromData(s,
     data)), w);
  fi;
  
  # f= rep*p*q*g. 
  
  return Concatenation(TraceRClassRepsTree(s, RClassIndexFromData(s, data)),
   Factorization(s, data, p*q), w);
end);

# new for 0.4! - Factorization - "for a trans. semi., img data, and perm" 
#############################################################################
# Usage: s = trans. semigroup, data = image data, f = permutation

# Returns: a word in the generators of s that acts on the image of
# the representative of the R-class with data <data> in the same way that f
# acts on this image.

# Notes: this is rather slow! Require some MN assistance with this one. 

InstallOtherMethod(Factorization, "for a trans. semi., img data, and perm",
[IsTransformationSemigroup, IsList, IsPerm],
function(s, data, f)
  local g, w, out, orders, power, gen, o, word, graph, m, u, i;
  
  g:=ImageOrbitSchutzGpFromData(s, data);
  w:=String(Factorization(g, f));
  
  if w="<identity ...>" then
    return [];
  fi;
 
  w:=List(SplitString(w, "*"), x-> SplitString(x, "^"));
  out:=[]; orders:=List(GeneratorsOfGroup(g), Order);
  
  for u in w do 
    if IsBound(u[2]) then 
      power:=Int(u[2]); gen:=Int(u[1]{[2..Length(u[1])]});
      if IsNegInt(power) then 
        power:=power+orders[gen];
      fi;
      for i in [1..power] do 
        Add(out, gen);
      od;
    else
      Add(out, Int(u[1]{[2..Length(u[1])]}));
    fi;
  od;
  o:=ImageOrbitFromData(s, data);
  word:=o!.schutz[data[4]][3];
  graph:=OrbitGraph(o); m:=data[4];

  return Concatenation(List(out, x->
  Concatenation([TraceSchreierTreeOfSCCForward(o, m,
  word[x][1]), [word[x][2]], TraceSchreierTreeOfSCCBack(o, m,
  graph[word[x][1]][word[x][2]])])));
end);

#TTT

# new for 0.4! - TraceRClassRepsTree - not a user function!
#############################################################################
# Usage: s = trans. semigroup; i = index of R-class rep 

# Returns: a word in the generators of s equal to GreensRClassReps(s)[i]. 

InstallGlobalFunction(TraceRClassRepsTree, 
function(s, i)
  local o, gen1, pos1, gen2, pos2, word_1, word_2, j, orb, m, l;

  Info(InfoCitrus, 4, "TraceRClassRepsTree");

  if not s!.opts!.schreier then 
    Error("it is not possible to factorize elements of this semigroup,");
    return;
  fi;

  o:=OrbitsOfImages(s);
  gen1:=o!.gen1; pos1:=o!.pos1; gen2:=o!.gen2; pos2:=o!.pos2; o:=o!.orbits;

  word_1:=[]; word_2:=[]; j:=i;

  while not gen1[pos2[j]]=fail do
    Add(word_1, gen1[pos2[j]]);
    if not ForAny(gen2[j], x-> x=fail) then 
      orb:=o[gen2[j][1]][gen2[j][2]]; m:=gen2[j][4]; l:=gen2[j][3]; 
      word_2:= Concatenation(word_2, 
       Reversed(TraceSchreierTreeOfSCCBack(orb, m, l)));
    fi;
    j:=pos1[pos2[j]];
  od;
  
  if not pos2[j]=1 then  
    Add(word_1, pos2[j]-1);
  fi;
  
  if not ForAny(gen2[j], x-> x=fail) then 
    orb:=o[gen2[j][1]][gen2[j][2]]; m:=gen2[j][4]; l:=gen2[j][3];
    word_2:=Concatenation(word_2, 
     Reversed(TraceSchreierTreeOfSCCBack(orb, m, l)));
  fi;
  
  return Concatenation(word_1, Reversed(word_2));
end);

#EOF
