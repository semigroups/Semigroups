############################################################################
##
#W  reesmat-iso.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#

InstallMethod(IdentityMapping, "for a Rees matrix semigroup", 
[IsReesMatrixSemigroup], 
function(R)
  local G;
  G:=UnderlyingSemigroup(R); 
  return RMSIsoByTriple(R, R, [(), IdentityMapping(G), 
   List([1..Length(Columns(R))+Length(Rows(R))], x-> One(G))]);
end);

#

InstallMethod(IdentityMapping, "for a Rees 0-matrix semigroup", 
[IsReesZeroMatrixSemigroup], 
function(R)
  local G;
  G:=UnderlyingSemigroup(R); 
  return RZMSIsoByTriple(R, R, [(), IdentityMapping(G), 
   List([1..Length(Columns(R))+Length(Rows(R))], x-> One(G))]);
end);

#

InstallGlobalFunction(RMSInducedFunction,
function(R, l, g, x)
  local mat, m, n, out, j, i;

  mat:=Matrix(R); m:=Length(mat[1]); n:=Length(mat);
  out:=EmptyPlist(m+n); out[1]:=x;

  out{[m+1..n+m]}:=List([m+1..n+m], v-> mat[v^l-m][1^l]*x*
   (mat[v-m][1]^g)^-1);
  out{[2..m]}:=List([2..m], v-> (mat[(m+1)^l-m][v^l])^-1*out[m+1]*
   (mat[1][v]^g));

  for j in [m+2..n+m] do 
    for i in [2..m] do 
      if mat[j^l-m][i^l]<>out[j]*mat[j-m][i]^g*out[i]^-1 then 
        return [false, out];
      fi;
    od;
  od;

  return [true, out];
end);

#

InstallGlobalFunction(RZMSInducedFunction, 
function(R, l, g, x, component)
  local mat, m, n, graph, rep, out, edges, bicomps, sub, perm, defined, orb, j, Last, involved, verts, v, new, k;
  
  mat:=Matrix(R); m:=Length(mat[1]); n:=Length(mat); graph:=RZMSGraph(R);

  rep:=Minimum(component);
  out:=EmptyPlist([1..m+n]);
  out[rep]:=x;

  if Length(component)=Length(Vertices(graph)) then 
    edges:=DirectedEdges(graph);
    bicomps:=Bicomponents(graph);
  else
    sub:=InducedSubgraph(graph, component);
    perm:=MappingPermListList(VertexNames(sub), Vertices(sub));
    edges:=OnTuplesTuples(DirectedEdges(sub), perm^-1);
    bicomps:=OnTuplesTuples(Bicomponents(sub), perm^-1);
  fi;

  defined:=[];
  orb:=[rep];  
  j:=0;

  repeat 
    j:=j+1;
    Last:=orb[j];
 
    involved:=Filtered(edges, x-> x[1]=Last and not x in defined);
  
    if not involved=[] then
      verts:=List(involved, x-> x[2]);
      Append(orb, Filtered(verts, x-> not x in orb));

      for k in [1..Length(verts)] do
        v:=verts[k];
      
        if Last in bicomps[1] then
          new:=mat[v^l-m][Last^l]*out[Last]*(mat[v-m][Last]^g)^-1;
        else
          new:=(mat[Last^l-m][v^l])^-1*out[Last]*(mat[Last-m][v]^g);
        fi; 
      
        if not IsBound(out[v]) then 
          out[v]:=new;
        elif not out[v]=new then
          return fail;
        fi;
        defined:=Union(defined, [involved[k], Reversed(involved[k])]);   
      od;
    fi;
  until defined=edges;

  return out;
end);

#

InstallGlobalFunction(RZMStoRZMSInducedFunction, 
function(rms1, rms2, l, g, groupelts)
  local mat1, mat2, m, n, rmsgraph, components, reps, imagelist, edges,
  bicomps, sub, perm, defined, orb, j, Last, involved, verts, v, new, i, k;

  mat1:=Matrix(rms1);
  mat2:=Matrix(rms2);
  m:=Length(mat1[1]); n:=Length(mat1);
  rmsgraph:=RZMSGraph(rms1);
  components:=ConnectedComponents(rmsgraph);

  if not Length(groupelts)=Length(components) then 
    Error("Must be as many elements as components");
  fi;

  reps:=List(components, Minimum);
  imagelist:=[];
  imagelist{reps}:=groupelts;

  for i in [1..Length(components)] do 
    if Length(components)=1 then 
      edges:=DirectedEdges(rmsgraph);
      bicomps:=Bicomponents(rmsgraph);
    else
      sub:=InducedSubgraph(rmsgraph, components[i]);
      perm:=MappingPermListList(VertexNames(sub), Vertices(sub));
      edges:=OnTuplesTuples(DirectedEdges(sub), perm^-1);
      bicomps:=OnTuplesTuples(Bicomponents(sub), perm^-1);
    fi;

    defined:=[];
    orb:=[reps[i]];  
    j:=0;

    repeat 
      j:=j+1;
      Last:=orb[j];
      involved:=Filtered(edges, x-> x[1]=Last and not x in defined);
      if not involved=[] then

        verts:=List(involved, x-> x[2]);
        Append(orb, Filtered(verts, x-> not x in orb));

        for k in [1..Length(verts)] do
          v:=verts[k];
        
          if Last in bicomps[1] then
            new:=mat2[v^l-m][Last^l]*imagelist[Last]*(mat1[v-m][Last]^g)^-1;
          else
            new:=(mat2[Last^l-m][v^l])^-1*imagelist[Last]*(mat1[Last-m][v]^g);
          fi; 
      
          if not IsBound(imagelist[v]) then 
            imagelist[v]:=new;
          elif not imagelist[v]=new then
            return fail;
          fi;
          defined:=Union(defined, [involved[k], Reversed(involved[k])]);   
        od;
      fi;
    until defined=edges;
  od;
  return imagelist;
end);

#

InstallMethod(IsomorphismSemigroups, "for Rees matrix semigroups", 
[IsReesMatrixSemigroup, IsReesMatrixSemigroup],
function(R1, R2)
  local mat, m, n, g, g1, g2, iso, isograph, isogroup, candidate, l, tup;

  if (Size(R1)=Size(R2) and
   ColumnsOfReesMatrixSemigroup(R1)=ColumnsOfReesMatrixSemigroup(R2) and
   RowsOfReesMatrixSemigroup(R1)=RowsOfReesMatrixSemigroup(R2)) then 
    
    mat:=Matrix(R1);
    m:=Length(mat[1]); n:=Length(mat);

    if R1=R2 then 
      g:=UnderlyingSemigroupOfReesMatrixSemigroup(R1); 
      return RMSIsoByTriple(R1, R2, [(), IdentityMapping(g), List([1..m+n], 
       x-> One(g))]); #JDm IdentityMapping
    else
      g1:=UnderlyingSemigroupOfReesMatrixSemigroup(R1);
      g2:=UnderlyingSemigroupOfReesMatrixSemigroup(R2);
      iso:=IsomorphismGroups(g1, g2);

      # for RMS without 0 the graphs are always isomorphic, 
      # being complete bipartite.

      if not iso=fail then 
        isograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
        #all isomorphisms from g1 to g2
        isogroup:=List(Elements(AutomorphismGroup(g1)), x->x*iso); 
  
        #find an induced function, if there is one
        for l in isograph do
          for g in isogroup do
            for tup in Elements(g2) do 
              candidate:=RMSInducedFunction(R2, l, g, tup); 
              if not candidate[1]=false then 
                return RMSIsoByTriple(R1, R2, [l, g, candidate[2]]);
              fi;
            od;
          od;
        od;
      fi;
    fi;
  fi;
  return fail;
end);

#

InstallMethod(IsomorphismSemigroups, "for Rees 0-matrix semigroups", 
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup],
function(R1, R2)
  local G1, G2, mat, m, n, f, groupiso, graph1, graph2, g, graphiso, candidate, l, tup;

  G1:=UnderlyingSemigroup(R1); 
  G2:=UnderlyingSemigroup(R2); 

  if not (IsRegularSemigroup(R1) and IsGroup(G1) and IsRegularSemigroup(R2) and
    IsGroup(G2)) then 
    Error("usage: the arguments must regular Rees 0-matrix semigroups over groups,");
  fi;

  if not (Size(R1)=Size(R2) and Columns(R1)=Columns(R2) and Rows(R1)=Rows(R2))
    then 
      return fail;
  fi;

  mat:=Matrix(R1);
  m:=Length(mat[1]); 
  n:=Length(mat);

  if R1=R2 then 
    return RZMSIsoByTriple(R1, R2, [(), IdentityMapping(G1), List([1..m+n], 
      x-> One(G2))]);
  fi;

  # every isomorphism of the groups
  f:=IsomorphismGroups(G1, G2);
  if f=fail then 
    return fail;
  fi;
  groupiso:=List(AutomorphismGroup(G1), x->x*f); 
  
  # every isomorphism of the graphs
  graph1:=RZMSGraph(R1); graph2:=RZMSGraph(R2);
  if UndirectedEdges(graph1)<>UndirectedEdges(graph2) then 
    g:=GraphIsomorphism(graph1, graph2);
    if f=fail then 
      return fail;
    fi;
    graphiso:=List(AutGroupGraph(graph1, [[1..m],[m+1..n+m]]), x-> x*g);
  fi;

  #find an induced function, if there is one
  for l in graphiso do
    for g in groupiso do
      for tup in Elements(G2) do 
        candidate:=RZMStoRZMSInducedFunction(R1, R2, l, g, [tup]);
        if not candidate=false then 
          return RZMSIsoByTriple(R1, R2, [l, g, candidate]);
        fi;
      od;
    od;
  od;
  return fail;
end);

#

InstallGlobalFunction(RMSIsoByTriple, 
function(R1, R2, triple)
  local fam, out;

  fam:=GeneralMappingsFamily(ElementsFamily(FamilyObj(R1)),
   ElementsFamily(FamilyObj(R2)));	
  out:=Objectify(NewType(fam, IsRMSIsoByTriple), rec(triple:=triple));
  SetSource(out, R1);
  SetRange(out, R2);
  #IsOne(out);
  
  return out;
end);

#

InstallGlobalFunction(RZMSIsoByTriple, 
function(R1, R2, triple)
  local fam, out;

  fam:=GeneralMappingsFamily(ElementsFamily(FamilyObj(R1)),
   ElementsFamily(FamilyObj(R2)));	
  out:=Objectify(NewType(fam, IsRZMSIsoByTriple), rec(triple:=triple));
  SetSource(out, R1);
  SetRange(out, R2);
  return out;
end);

#

InstallMethod(ELM_LIST, "for objects in `IsRMSIsoByTriple'", 
[IsRMSIsoByTriple, IsPosInt], 
function(x, i)

  if 1<=i and i<=3 then 
    return x!.triple[i];
  fi;
  Error("usage: the index must be at most 3,");
  return;
end);

#

InstallMethod(\=, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj, 
[IsRMSIsoByTriple, IsRMSIsoByTriple],
function(triple1, triple2)

  if triple1[1]=triple2[1] and  triple1[2]=triple2[2] and
    triple1[3]=triple2[3] then 
    return true;
  fi;
  return OnTuples(GeneratorsOfSemigroup(Source(triple1)), 
   triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
end);

#

InstallMethod(\=, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj, 
[IsRZMSIsoByTriple, IsRZMSIsoByTriple], 
function(triple1, triple2)

  if triple1[1]=triple2[1] and  triple1[2]=triple2[2] and
    triple1[3]=triple2[3] then 
    return true;
  fi; 
  return OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple1)
   =OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
end);

#

InstallMethod(\<, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj,
[IsRMSIsoByTriple, IsRMSIsoByTriple],  
function(triple1, triple2)

  return (triple1[1]<triple2[1]) or
   (triple1[1]=triple2[1] and
    triple1[2]<triple2[2]) or 
   (triple1[1]=triple2[1] and
    triple1[2]=triple2[2] and 
    triple1[3]<triple2[3]);
end);

#

InstallMethod(\<, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj,
[IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(triple1, triple2)

  return (triple1[1]<triple2[1]) or
   (triple1[1]=triple2[1] and 
    triple1[2]<triple2[2]) or 
   (triple1[1]=triple2[1] and 
    triple1[2]=triple2[2] and 
    triple1[3]<triple2[3]);
end);

#

InstallMethod(CompositionMapping2, "for objects in `IsRMSIsoByTriple'", 
IsIdenticalObj, [IsRMSIsoByTriple, IsRMSIsoByTriple],
function(a1, a2)
  local n, l1, l2, g1, g2, f1, f2;

  n:=Length(Rows(Source(a1)))+Length(Columns(Source(a1)));

  l1:=a1[1]; l2:=a2[1];
  g1:=a1[2]; g2:=a2[2];
  f1:=a1[3]; f2:=a2[3];

  return RMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, List([1..n], 
   x->f2[x^l1]*f1[x]^g2)]);
end);

#

InstallMethod(CompositionMapping2, "for objects in `IsRZMSIsoByTriple'",
IsIdenticalObj, [IsRZMSIsoByTriple, IsRZMSIsoByTriple],
function(a1, a2)
  local n, l1, l2, g1, g2, f1, f2;

  n:=Length(Rows(Source(a1)))+Length(Columns(Source(a1)));

  l1:=a1[1]; l2:=a2[1];
  g1:=a1[2]; g2:=a2[2];
  f1:=a1[3]; f2:=a2[3];

  return RZMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, 
   List([1..n], x->f2[x^l1]*f1[x]^g2)]);
end);

#

InstallMethod(ImagesElm, "for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function(triple, x)
  return [ImagesRepresentative(triple, x)];
end);

#

InstallMethod(ImagesElm, "for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(triple, x)
  return [ImagesRepresentative(triple, x)];
end);

#

InstallMethod(ImagesRepresentative,
"for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function( triple, x)
  local g, i, j, lambda, gamma, f, m;

  m:=Length(Rows(Source(triple)));

  i:=x[1]; g:=x[2]; j:=x[3]+m;
  lambda:=triple[1];
  gamma:=triple[2]; 
  f:=triple[3];
  #JDM: use objectify here...
  return ReesMatrixSemigroupElement(Range(triple), i^lambda,
  f[i]*ImageElm(gamma,g)/f[j], j^lambda-m);
end);

#

InstallMethod(ImagesRepresentative, 
"for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(triple, x)
  local g, i, j, lambda, gamma, f, m;

  if not x=MultiplicativeZero(Source(triple)) then
    m:=Length(Rows(Source(triple)));

    i:=x[1]; g:=x[2]; j:=x[3]+m;
    lambda:=triple[1]; gamma:=triple[2]; f:=triple[3];

    if f[i]=0 or f[j]=0 then 
      return MultiplicativeZero(Source(triple));
    else
      #JDM: use objectify here...
      return ReesZeroMatrixSemigroupElement(Range(triple), i^lambda,
       f[i]*ImageElm(gamma,g)/f[j], j^lambda-m);
    fi;
  else 
    return x;
  fi;
end);


#

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", 
[IsEndoGeneralMapping and IsRMSIsoByTriple],
function(a)
  local l, g, f, n; 

  n:=Length(Rows(Source(a)))+Length(Columns(Source(a)));

  l:=a[1]; g:=a[2]; f:=a[3];

  return RMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
   (f[x^l]^(g^-1))^-1)]);
end);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", 
[IsEndoGeneralMapping and IsRMSIsoByTriple and IsOne], x-> x);

#

InstallMethod(InverseGeneralMapping, "for objects in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(a)
  local l, g, f, n; 

  n:=Length(Rows(Source(a)))+Length(Columns((Source(a))));

  l:=a[1]; g:=a[2]; f:=a[3];

  return RZMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],
   x->(f[x^l]^(g^-1))^-1)]);
end);

#

InstallMethod(IsOne, "for objects in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple], 99, #JDM why the 99?
function(triple)
  return IsOne(triple[1]) and IsOne(triple[2]) and 
   ForAll(triple[3], IsOne);
end);

#

InstallMethod(IsOne, "for objects in `IsRZMSIsoByTriple'",
[IsEndoGeneralMapping and IsRZMSIsoByTriple],
function(triple)
  return IsOne(triple[1]) and IsOne(triple[2]) and 
   ForAll(triple[3], IsOne);
end);

#

InstallMethod(PreImagesRepresentative,
"for an RMS element under a mapping by a triple", 
FamSourceEqFamElm, [IsRMSIsoByTriple, IsReesMatrixSemigroupElement],
function(triple, x)
  return ImagesRepresentative(triple^-1, x);
end);

#

InstallMethod(PreImagesRepresentative,  
"for an RZMS element under a mapping by a triple", 
FamSourceEqFamElm, [IsRZMSIsoByTriple, IsReesZeroMatrixSemigroupElement],
function(triple, x)
  return ImagesRepresentative(triple^-1, x);
end);

#

InstallMethod(PrintObj, "for object in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple], 
function( obj )
  Print( "RMSIsoByTriple ( ", Source(obj), ",", Range(obj), "," , obj[1], " ", 
    obj[2], " ",  obj[3], " )" );
  return;
end);

#

InstallMethod(PrintObj, "for object in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function( obj )
  Print( "RZMSIsoByTriple ( ", Source(obj) , ",", Range(obj), "," , obj[1], " ", 
    obj[2], " ",  obj[3], " )" );
  return;
end);

#

InstallMethod( ViewObj, "for object in `IsRMSIsoByTriple'",
[IsRMSIsoByTriple], 
function( obj ) 
  Print( "(", obj[1],", ",  obj[2],", ",  obj[3], ")" );
end );

#

InstallMethod( ViewObj, "for object in `IsRZMSIsoByTriple'",
[IsRZMSIsoByTriple],
function(obj) 
  Print( "(", obj[1], ", ", obj[2], ", ", obj[3], ")" );
  return;
end);

#EOF
