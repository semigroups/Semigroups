#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Note about the difference between One and MultiplicativeNeutralElement 
# (the same goes for Zero and MultplicativeZero):
#
# One(s) returns One(Representative(s)) if it belongs to s, so that 
# One(s)=Transformation([1..DegreeOfTransformationSemigroup(s)]) if s is a
# transformation semigroup and it returns fail otherwise, or it returns 
# PartialPerm([1..DegreeOfPartialPermSemigroup]) if this belongs to s. 
#
# MultiplicativeNeutralElement on the other hand returns the element of s that
# acts as the identity, note that this can be equal to One(s) but it can also
# not be equal t One(s). 
#
# A semigroup satisfies IsMonoidAsSemigroup(s) if
# MultiplicativeNeutralElement(x)<>fail, so it could be that One(s) returns
# fail but IsMonoidAsSemigroup is still true. 

InstallMethod(IsGreensDLeq, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local partial, data, comp_index;

  partial:=PartialOrderOfDClasses(S);
  data:=SemigroupData(S);

  comp_index:=function(x, y)
    if y in partial[x] then
      return true;
    elif Length(partial[x])=1 and partial[partial[x][1]]=partial[x] then
      return false;
    fi;
    return ForAny(partial[x], z-> z<>x and comp_index(z,y));
  end;

  return function(x,y)
    return comp_index(OrbSCCLookup(data)[Position(data, x)]-1,
      OrbSCCLookup(data)[Position(data, y)]-1);
  end;
end);

#JDM improve this!

InstallMethod(IsMaximalSubsemigroup, "for a semigroup and semigroup", 
[IsSemigroup, IsSemigroup],
function(S, T)
  if IsSubsemigroup(S, T) and S<>T then 
    return ForAll(S, x-> x in T or Semigroup(GeneratorsOfSemigroup(T), x)=S);
  else
    return false;
  fi;
end); 

#

# Returns a list of lists (connected comps), each list being those (I,J)
# co-ords of the group H-classes in the connected comp
InstallMethod(RMSConnectedComponents, "for a Rees zero matrix semigroup", 
[IsReesZeroMatrixSemigroup],
function(R)
  local I, J, mat, len, graph, comps;
  
  I:=Rows(R); J:=Columns(R); mat:=Matrix(R);
  len:=Length(mat[1]);

  graph:=Graph(Group(()), Union(I, J+len), OnPoints,
    function(i,j)
      if i<=len and j>len then
        return mat[j-len][i]<>0;
      elif j<=len and i>len then
        return mat[i-len][j]<>0;
      fi;
      return false;
    end, true);
    
  comps:=List(ConnectedComponents(graph),x->Intersection(x,I));

  return List(comps,x->
    Concatenation(List(x, y->
    List(graph.adjacencies[y], z->[y,z-len])
    )));
    
end); 

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees matrix semigroup over groups

InstallMethod(MaximalSubsemigroups, "for a Rees matrix subsemigroup",
[IsReesMatrixSubsemigroup], 
function(R)
  local G, out, mat, I, J, P, U, H, j, i;
  
  if not IsReesMatrixSemigroup(R) then 
    TryNextMethod(); 
    return;
  fi;
 
  G:=UnderlyingSemigroup(R);    
  
  if not IsGroup(G) then 
    if IsSimpleSemigroup(R) then 
      #take an isomorphism to a Rees 0-matrix semigroup, then find its maximal
      #subsemigroups, then pull those back, (should specify some methods for the
      #pulling back part)
      Error("not yet implemented,");
      return;
    else
      TryNextMethod();
      return;
    fi;
  fi;

  out:=[];
  mat:=Matrix(R);     I:=Rows(R);         J:=Columns(R);
 
  # Case 1: maximal subsemigroups of the form (IxHxJ) where H is a
  # maximal subgroup of G
  Info(InfoSemigroups, 3, 
   "Case 1: maximal subsemigroups arising from maximal subgroups...");
  for H in MaximalSubgroups(G) do
    U:=Semigroup(GeneratorsOfReesMatrixSemigroupNC(R, I, H, J));
    if Size(U)<Size(R) then 
      Add(out, U);
    fi;
  od;
  Info(InfoSemigroups, 3, "...found ", Length(out));

  # Case 2: maximal subsemigroup of the form (IxGxJ') where J'=J\{j} for some j
  # in J.
  Info(InfoSemigroups, 3, 
   "Case 2: maximal subsemigroups obtained by removing a column...");
  if Length(J)>1 then 
    for j in J do 
      Add(out, ReesMatrixSubsemigroupNC(R, I, G, Difference(J, [j])));
    od;
  fi;
  
  # Case 3: the dual of case 2.
  Info(InfoSemigroups, 3, 
   "Case 3: maximal subsemigroups obtained by removing a row...");
  if Length(I)>1 then 
    for i in I do 
      Add(out, ReesMatrixSubsemigroupNC(R, Difference(I, [i]), G, J));
    od;
  fi;

  return out;
end);

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees 0-matrix semigroup over groups

if not IsBound(GAPInfo.PackagesLoaded.grape) then 
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
  [IsReesZeroMatrixSubsemigroup], 
  function(R)
    Info(InfoWarning, 1, "the GRAPE is not loaded, and so this function does",
         " not work");
    return fail;
  end); 
else
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
  [IsReesZeroMatrixSubsemigroup], 
  function(R)
    local G, out, I, J, mat, P, new, pos, U, JJ, solo, II, len, graph, names, rectangles, gens, i, j, H, r, k;
    
    if not IsReesZeroMatrixSemigroup(R) then 
      TryNextMethod(); 
      return;
    fi;
   
    G:=UnderlyingSemigroup(R);    
    
    if not IsGroup(G) then 
      if IsZeroSimpleSemigroup(R) then 
        #take an isomorphism to a Rees 0-matrix semigroup, then find its maximal
        #subsemigroups, then pull those back, (should specify some methods for
        #the pulling back part)
        Error("not yet implemented,");
        return;
      else
        TryNextMethod();
        return;
      fi;
    fi;

    out:=[]; I:=Rows(R); J:=Columns(R); mat:=Matrix(R);     
   
    # find the set of group elements in the matrix
    P:=Union(mat{J}{I});
    if P[1]=0 then 
      Remove(P, 1);
    else  # S\{0} is a maximal subsemigroup 
          # the unique case when the missing D-class is {0}
      new:=ShallowCopy(GeneratorsOfSemigroup(R));
      if Size(new)>1 then  
        pos:=Position(new, MultiplicativeZero(R)); 
        Remove(new, pos); #remove the zero, which has to be present
      fi;
      Add(out, Semigroup(new));
    fi;
    
    if Length(I)=1 and Length(J)=1 and IsTrivial(G) then 
      # the unique case when {0} is a maximal subsemigroup, if <G> is
      # non-trivial then {0, 1_G} is a subsemigroup containing <0> and not
      # equal to <R>. 
      Add(out, Semigroup(MultiplicativeZero(R)));
    fi;
    
    # Case 1: maximal subsemigroups of the form (IxHxJ)\cup\{0\} where H is a
    # maximal subgroup of G
    
    Info(InfoSemigroups, 3, 
     "Case 1: maximal subsemigroups arising from maximal subgroups...");
    
    for H in MaximalSubgroups(G) do
      U:=Semigroup(GeneratorsOfReesZeroMatrixSemigroupNC(R, I, H, J));
      if Size(U)<Size(R) then 
        Add(out, U);
      fi;
    od;

    Info(InfoSemigroups, 3, "...found ", Length(out));

    # Case 2: maximal subsemigroup of the form (IxGxJ')\cup\{0\} where J'=J\{j}
    # for some j in J, and where the resultant matrix has no zero columns or
    # rows.

    # in the Graham-Houghton IxJ bipartite graph, we can remove any vertex <j>
    # in <J> which is not adjacent to a vertex <i> which is only adjacent to
    # <j>.  So, we run through the vertices <i> of <I> and find the ones of
    # degree 1, and we discard the vertices <j> adjacent to such <i>. 
    
    Info(InfoSemigroups, 3, 
     "Case 2: maximal subsemigroups obtained by removing a column...");
    
    JJ:=ShallowCopy(J);

    for i in I do  
      solo:=false; # keep track of whether <i> has degree 1
      for j in J do
        if mat[j][i]<>0 then
          if solo<>false then    # <i> has degree greater than 1
            solo:=false;         
            break;               # so skip it
          else
            solo:=j;             # <i> is adjacent to <j> and we don't know if
                                 # it is adjacent to any other vertex
          fi;
        fi;
      od;
      if solo<>false then        # <i> is adjacent to <solo> and nothing else
        RemoveSet(JJ, solo);     # so remove it. 
      fi;
    od;
    
    for j in JJ do 
      U:=ReesZeroMatrixSubsemigroupNC(R, I, G, Difference(J, [j]));
      if not MultiplicativeZero(R) in U then 
        U:=Semigroup(U, MultiplicativeZero(R));
        SetIsReesZeroMatrixSemigroup(U, true);
      fi;
      Add(out, U);
    od;
    Info(InfoSemigroups, 3, "...found ", Length(JJ));

    # Case 3: the dual of case 2.
    
    Info(InfoSemigroups, 3, 
     "Case 3: maximal subsemigroups obtained by removing a row...");
    
    II:=ShallowCopy(I);

    for j in J do  
      solo:=false; # keep track of whether <i> has degree 1
      for i in I do
        if mat[j][i]<>0 then
          if solo<>false then    # <i> has degree greater than 1
            solo:=false;         
            break;               # so skip it
          else
            solo:=i;             # <i> is adjacent to <j> and we don't know if
                                 # it is adjacent to any other vertex
          fi;
        fi;
      od;
      if solo<>false then        # <i> is adjacent to <solo> and nothing else
        RemoveSet(II, solo);     # so remove it. 
      fi;
    od;
    
    for i in II do 
      U:=ReesZeroMatrixSubsemigroupNC(R, Difference(I, [i]), G, J);
      if not MultiplicativeZero(R) in U then 
        U:=Semigroup(U, MultiplicativeZero(R));
        SetIsReesZeroMatrixSemigroup(U, true);
      fi;
      Add(out, U);
    od;

    Info(InfoSemigroups, 3, "...found ", Length(II));

    # Case 4: maximal rectangle of zeros in the matrix
    Info(InfoSemigroups, 3, 
     "Case 4: maximal subsemigroups obtained by removing a rectangle...");
    Info(InfoSemigroups, 3, "finding rectangles...");

    len:=Length(mat[1]); # use <mat> to keep the indices correct

    graph:=Graph(Group(()), Union(I, J+len), OnPoints,
     function(i,j)
       if i<=len and j>len then
         return mat[j-len][i]=0;
       elif j<=len and i>len then
         return mat[i-len][j]=0;
       else
         return i<>j;
       fi;
     end, true);

    names:=x-> graph.names[x];

    rectangles:=CompleteSubgraphs(graph);

    Info(InfoSemigroups, 3, "...found ", Length(rectangles));

    gens:=GeneratorsOfGroup(G);
    
    Info(InfoSemigroups, 3, 
     "finding subsemigroups arising from rectangles...");
    for r in [2..Length(rectangles)-1] do 
    #the first and last entries correspond to removing all the rows or columns
      Apply(rectangles[r], names);
       
      # add group generators
      i:=rectangles[r][1];  
      j:=First(Columns(R), j-> mat[j][i]<>0 and not j+len in rectangles[r]); 
      if IsTrivial(G) then 
        new:=[Objectify(TypeReesMatrixSemigroupElements(R),
         [i, mat[j][i]^-1, j, mat])];
      else
        new:=List(gens, x-> Objectify(TypeReesMatrixSemigroupElements(R), 
          [i, x*mat[j][i]^-1, j, mat]));
      fi;
      
      j:=First(rectangles[r], j-> j>len)-len;
      i:=First(Rows(R), i-> mat[j][i]<>0 and not i in rectangles[r]);
      if IsTrivial(G) then 
        Add(new, Objectify(TypeReesMatrixSemigroupElements(R),
         [i, mat[j][i]^-1, j, mat]));
      else
        Append(new, List(gens, x-> 
         Objectify(TypeReesMatrixSemigroupElements(R), 
          [i, x*mat[j][i]^-1, j, mat])));
      fi;

      for k in rectangles[r] do  
        if k<=len then # k in I
          for j in J do 
            Add(new, RMSElement(R, k, One(G), j));
          od;
        else # k-len in J
          for i in I do 
            Add(new, RMSElement(R, i, One(G), k-len));
          od;
        fi;
      od;
      Add(out, Semigroup(new));
    od;
    return out;
  end);
fi;

#

InstallMethod(MaximalCasey, "for a Rees zero matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(s)
  local out, g, mat, components, h, max, i, j, gens, I, J, h1, gen, thing, temp, k, comp, poss, poss2;

  out:=[];
  g:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(s);
  mat:=Matrix(s);
  components:=RMSConnectedComponents(s);
  
  # pick a distinguished group h-class, indexed by I and J. We arbitrarily choose the first one of the first component.
  # This group H-class will contain the elts of the coset (max*mat[J][I]^-1) for each maximal subgroup
  I:=components[1][1][1];
  J:=components[1][1][2];
  
  for max in MaximalSubgroups(g) do
  
    # Firstly get generators for our distinguished h-class in first component
    gens:=List(Generators(max), x->
      ReesZeroMatrixSemigroupElement(s, I, x*(mat[J][I]^-1), J)
    );
    
    # Add to the generators one element which must be in each group h-class
    for comp in components do
      for h in comp do
        Add(gens, ReesZeroMatrixSemigroupElement(s, h[1], (mat[h[2]][h[1]]^-1), h[2]));
      od;
    od;
  
    # If there is only one component, we have now specified enough that
    # our subsemigroup is maximal, or it equals S
    if Length(components) = 1 then    
      Add(out, Semigroup(gens));
    else

      # Otherwise we are very unlikely to have a generating set for what we want yet
      
      poss:=EmptyPlist(Length(components)-1);
      poss2:=EmptyPlist(Length(components)-1);
      
      # Select the distinguished non-group H-classes which we'll need from each 'block' to put our generators in
      for k in [2..Length(components)] do
        Add(poss,components[k][1][2]);
        # So components[k][1] is a group H-class H_i,j in the connected component #k
        # Now need to pick any non-group H-class in its 'complementary block'
        # We choose H_I,j
        Add(poss2,components[k][1][1]);
        # So components[k][1] is a group H-class H_i,j in the connected component #k
        # Now need to pick any non-group H-class in its mirror image 'complementary block'
        # We choose H_i,J
      od;
      
      # Now populate all possible variations
      for thing in Tuples(RightTransversal(g,max),2*(Length(components)-1)) do
        temp:=[];
        
        for i in [1..(Length(components)-1)] do
          Add(temp, ReesZeroMatrixSemigroupElement(s, I, thing[i], poss[i]));
        od;
        for i in [Length(components)..2*(Length(components)-1)] do
          Add(temp, ReesZeroMatrixSemigroupElement(s, poss2[i+1-Length(components)], thing[i], J));
        od;
        
        Add(out, Semigroup(gens, temp, [MultiplicativeZero(s)]));
      od;
    fi;  
  od;
  
  # Known issue: sometimes duplicates are returned (seen in an example with 3 components)
  # return DuplicateFreeList(Filtered(out,x->Size(x)<>Size(s)));
  return Filtered(out,x->Size(x)<>Size(s));

end);

#

InstallMethod(MaximalSubsemigroups, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local out, gens, po, reps, classes, D, lookup, count, max, max2, found_case1, nonmax, tot, gens2, pos, inj, R, V, tuples, ideal, YannRecursion, HClassClosure	, U, A, XX, a, C, i, j, k, gens3, UU, G, I, J, H;
  
  if Size(S) = 1 then
    return [];
  fi;
  
  # Should be made into its own method or something
  if IsGroupAsSemigroup(S) then
    max2:=function(S)
      local out, G, iso, inv, max, g, gens;
  
      out:=[];
      iso:=IsomorphismPermGroup(S);
      inv:=InverseGeneralMapping(iso);
      G:=Range(iso);
      max:=MaximalSubgroups(G);
      for g in max do
        gens:=GeneratorsOfSemigroup(g);
        Add(out, Semigroup(Images(inv,gens)));
      od;
  
      return out;
  
    end;
    return max2(S);
  fi;
  
  # preprocessing...
  out:=[];
  Info(InfoSemigroups, 2, "finding irredundant generating set...");
  gens:=IrredundantGeneratingSubset(S);
  po:=List(PartialOrderOfDClasses(S), x-> ShallowCopy(x));
  classes:=GreensDClasses(S);
  D:=List(gens, x-> PositionProperty(classes, d-> x in d));
  lookup:=[]; #lookup[i] is the positions of gens in classes[i]
  max:=[];    #index of maximal D-classes containing gens
  nonmax:=[]; #index of non-maximal D-classes containing gens.

  Info(InfoSemigroups, 2, "finding D-classes of generators...");
  for i in [1..Length(gens)] do 
    if not ForAny([1..Length(po)], j-> j<>D[i] and D[i] in po[j]) then 
      AddSet(max, D[i]);
    else 
      AddSet(nonmax, D[i]);
    fi;
    if not IsBound(lookup[D[i]]) then 
      lookup[D[i]]:=[];
    fi;
    Add(lookup[D[i]], i);
  od;
  
  # Type 1: maximal subsemigroups arising from maximal subsemigroup of
  # principal factors of maximal D-classes...
  Info(InfoSemigroups, 2, "finding maximal subsemigroups arising from", 
  " maximal D-classes...");
  tot:=0;
  for i in max do 
    Info(InfoSemigroups, 2, "considering D-class ", i);
    if Size(classes[i])=1 then #remove the whole thing...
      gens2:=ShallowCopy(gens);
      Remove(gens2, lookup[i][1]);
      pos:=Position(po[i], i);
      if pos<>fail then 
        Remove(po[i], pos);
      fi;
      Append(gens2, List(classes{po[i]}, Representative));
      if not IsEmpty(gens2) then
        Add(out, SemigroupIdealByGenerators(S, gens2));
      fi;
    else
      inj:=InverseGeneralMapping(InjectionPrincipalFactor(classes[i]));
      R:=Source(inj);
      for U in MaximalSubsemigroups(R) do 
        gens2:=ShallowCopy(gens){Difference([1..Length(gens)], lookup[i])};
        pos:=Position(po[i], i);
        if pos<>fail then 
          Remove(po[i], pos);
        fi;
        Append(gens2, List(classes{po[i]}, Representative));
        if not IsEmpty(gens2) then
          V:=SemigroupIdealByGenerators(S, gens2);
        else
          V:=[];
        fi;
        tuples:=OnTuples(Filtered(GeneratorsOfSemigroup(U), 
             x-> not IsMultiplicativeZero(U, x)), inj);
        if tuples=[] and Size(U) = 1 then  # This will fail if U = {0} ???
          Add(out, Semigroup(OnTuples(
          GeneratorsOfSemigroup(U), inj), V, rec(small:=true)));
        elif tuples<>[] then
          Add(out, Semigroup(tuples, V, rec(small:=true)));
        fi;
      od;
    fi;
    Info(InfoSemigroups, 2, "found ", Length(out)-tot, " maximal subsemigroups");
    tot:=Length(out);
  od;

  if not IsEmpty(nonmax) then
    Info(InfoSemigroups, 2, "finding maximal subsemigroups arising from", 
    " non-maximal D-classes...");
  else
    Info(InfoSemigroups, 2, "no non-maximal D-classes to consider...");    
  fi;
  #Type 2: maximal subsemigroups arising from non-maximal D-classes
  for i in nonmax do 
    Info(InfoSemigroups, 2, "considering D-class ", i);
    if not IsRegularDClass(classes[i]) then #remove the whole thing...
      #find the generators for the ideal...
      gens2:=ShallowCopy(gens);
      Remove(gens2, lookup[i][1]); #there's only one generator in the D-class
      pos:=Position(po[i], i);
      if pos<>fail then
        Remove(po[i], pos);
      fi;
      reps:=List(classes{po[i]}, Representative);
      if not IsEmpty(reps) then
        Append(gens2, 
         GeneratorsOfSemigroup(SemigroupIdealByGenerators(S,
          reps)));
      fi;
      Add(out, Semigroup(gens2, rec(small:=true)));
      Info(InfoSemigroups, 2, "found maximal subsemigroup arising from", 
      " removing whole non-maximal non-regular D-class...");
      #find the generator above <classes[i]>
    else # <classes[i]> is regular
      
      pos:=Position(po[i], i);
      if pos<>fail then 
        Remove(po[i], pos);
      fi;
      count:=0;
      reps:=List(classes{po[i]}, Representative);
      if not IsEmpty(reps) then
        ideal:=GeneratorsOfSemigroup(Semigroup(SemigroupIdealByGenerators(S,
        reps), rec(small:=true)));
      else
        ideal:=[];
      fi;
      YannRecursion:=function(U, known, A, depth)
        local ismax, new_known, a, V, didtest, h, new_depth;
        new_depth:=depth+1;
        count:=count+1;
        Print("call: ", count, ", depth: ", new_depth,"\n");
        ismax:=true; 
        new_known:=ShallowCopy(known);
        didtest:=false;
        while not IsEmpty(A) do
          a:=A[1];
          h:=HClass(S,a);
          if not ForAny(h, x->x in known) then 
            didtest:=true;
            V:=Semigroup(U, h);
            # ? Should above line be: V:=HClassClosure(Semigroup(U, h));
            if ForAll(XX, x-> not x in V) then #i.e. check that V<>S
              ismax:=false;
              if ForAll(new_known, x-> not x in V) then 
                YannRecursion(V, new_known, Difference(A, V), new_depth);
              fi;
              new_known:=Union(new_known, h);
            fi;
            A:=Difference(A,h);
          fi;
        od;
        if ismax and didtest then
          if not ForAny(out, W-> IsSubsemigroup(W, U)) then 
            Add(out, Semigroup(U, ideal));
            Info(InfoSemigroups, 2, "found maximal subsemigroup arising from", 
            " YannRecursion");
          fi;
        fi;
        return;
      end;
      
      # Case 1: Max. subsemigroups which intersect every H-class of classes[i]     
      Info(InfoSemigroups, 2, "\n\nCase 1: Looking for maximal subsemigroups ",
        "which intersect every H-class of the D-class\n");    
      gens3:=gens{lookup[i]};
      gens2:=Difference(ShallowCopy(gens), gens3);
      U:=Semigroup(gens2);
    
      inj:=InverseGeneralMapping(InjectionPrincipalFactor(classes[i]));
      R:=Source(inj);
    
      G:=UnderlyingSemigroup(R); 
      I:=Rows(R); J:=Columns(R);
             
      tot:=0;
      for H in MaximalSubgroups(G) do
        UU:=GeneratorsOfReesZeroMatrixSemigroupNC(R, I, H, J);
        if Size(Semigroup(UU))<Size(R) then
          UU:=Semigroup(Images(inj, UU), U);
          if ForAny(gens3, x-> not x in UU) then
            Info(InfoSemigroups, 2, "found maximal subsemigroup which ", 
              "intersects every H-class of the D-class");
            Add(out, Semigroup(GeneratorsOfSemigroup(UU), ideal));
            tot:=tot+1;
          fi;
        fi;
      od;
      
      if tot > 0 then
        found_case1:=true;
      else
        found_case1:=false;
        Info(InfoSemigroups, 2, "Found no such results\n");    
      fi;
      
      # Case 2: Max. subsemigroups which are a union of H-classes in classes[i]        
      Info(InfoSemigroups, 2, "\n\nCase 2: Looking for maximal subsemigroups ",
        "which are a union of H-classes\n");    
      for k in [1..Length(lookup[i])] do      
        for j in Combinations(lookup[i], k) do 
          Info(InfoSemigroups, 2, "\nTrying to remove gens: ", j, "...");
          # indices of gens in classes[i]
          gens2:=Difference(ShallowCopy(gens), gens{j});
          U:=Semigroup(gens2);
          A:=Difference(classes[i], Intersection(U, classes[i]));
          for a in gens{j} do RemoveSet(A, a); od;
          XX:=Union(List(gens{j},x->Elements(HClass(S,x))));
          while not IsEmpty(A) do 
            a:=A[1];
            C:=Semigroup(a, gens2);
            if ForAny(XX, x-> x in C) then 
              RemoveSet(A, a);
              XX:=Union(XX,Elements(HClass(S,a)));
            else
              A:=Difference(A, C);
            fi;
          od;
          
          if Length(XX)=Size(classes[i]) then #remove the whole class
            if k = 1 and not found_case1 then
              Add(out, Semigroup(gens2, ideal));
              Info(InfoSemigroups, 2, "found maximal subsemigroup arising from", 
              " removing whole non-maximal regular D-class...");
            fi;
            # if k > 1, we can stop considering this subset gens{j}
          else
            A:=Filtered(classes[i], x-> not (x in XX or x in U));
            if IsEmpty(A) then 
              if k = 1 then
                Add(out, Semigroup(GeneratorsOfSemigroup(U), ideal));
                Info(InfoSemigroups, 2, "found maximal subsemigroup arising", 
                " from removing all of XX");
              fi;
              # if k > 1, I think we can stop considering this subset gens{j}
            else # not IsEmpty(A)
              V:=Semigroup(U, A, ideal, rec(small:=true));
              if V<>S then
                if k = 1 then
                  Add(out, V);
                  Info(InfoSemigroups, 2, "found maximal subsemigroup arising",
                  " by removing all of XX");
                elif ForAll(XX, x->not x in V) and not ForAny(out, W-> not IsSubsemigroup(W, V)) then
                  Add(out, V);
                  Info(InfoSemigroups, 2, "found maximal subsemigroup arising",
                  " by removing all of XX");
                fi;
              else
                
                #XX:=Union(List(XX, x-> Elements(HClass(S, x))));
                #A:=Filtered(classes[i], x-> not (x in XX or x in U));
              
                HClassClosure:=function(U)
                  local V, B;
                  B:=Intersection(U, classes[i]);
                  B:=Union(List(B, x-> Elements(HClass(S, x))));
                  if not IsEmpty(B) then
                    return Semigroup(U, B, rec(small:=true));
                  fi;
                  return U;
                end;

                # Set U to be a union of H-classes of S            
                U:=HClassClosure(U);    
                if not ForAny(XX, x->x in U) then           
                  A:=Filtered(classes[i], x-> not (x in XX or x in U));
                  count:=0;
                  YannRecursion(U, [], A, 0);
                fi;
              fi;
            fi;
          fi;
        od;
      od;
    fi;
  od;
  Info(InfoSemigroups, 2, "generating all found maximal subsemigroups...");
  out:=List(out, x-> Semigroup(x, rec(small:=true))); 
  return out;
end);

#

InstallMethod(MaximalDClasses, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, partial, data, pos, i, out, classes, x;

  gens:=GeneratorsOfSemigroup(s);

  partial:=PartialOrderOfDClasses(s);
  data:=SemigroupData(s);
  pos:=[];
  for x in gens do 
    i:=OrbSCCLookup(data)[Position(data, x)]-1; 
    #index of the D-class containing x 
    AddSet(pos, i);
  od;

  out:=[];
  classes:=GreensDClasses(s);
  for i in pos do 
    if not ForAny([1..Length(partial)], j-> j<>i and i in partial[j]) then 
      Add(out, classes[i]);
    fi;
  od;

  return out;
end);

#

InstallMethod(StructureDescriptionSchutzenbergerGroups, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local o, scc, out, m;

  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  out:=[];

  for m in [2..Length(scc)] do 
    AddSet(out, StructureDescription(LambdaOrbSchutzGp(o, m)));
  od;

  return out;
end);

#

InstallMethod(StructureDescriptionMaximalSubgroups, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local out, d;

  out:=[];
  for d in DClasses(s) do 
    if IsRegularClass(d) then 
      AddSet(out, StructureDescription(GroupHClass(d)));
    fi;
  od;

  return out;
end);

#

InstallMethod(GroupOfUnits, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  deg:=DegreeOfTransformationSemigroup(s);   
 
  u:=Monoid(List(GeneratorsOfGroup(g), x-> AsTransformation(x, deg)));
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, PermutationOfImage, 
   x-> AsTransformation(x, deg)));
   
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(GroupOfUnits, "for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  deg:=Maximum(DegreeOfPartialPermSemigroup(s),
   CodegreeOfPartialPermSemigroup(s));   
 
  u:=Monoid(List(GeneratorsOfGroup(g), x-> AsPartialPerm(x, deg)));
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, AsPermutation, 
   x-> AsPartialPerm(x, deg)));
   
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(GroupOfUnits, "for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local R, G, deg, U;

  if MultiplicativeNeutralElement(S)=fail then
    return fail;
  fi;

  R:=GreensRClassOfElementNC(S, MultiplicativeNeutralElement(S));
  G:=SchutzenbergerGroup(R);
  deg:=DegreeOfBipartitionSemigroup(S);
  
  U:=Monoid(List(GeneratorsOfGroup(G), x-> AsBipartition(x, deg)));
  
  SetIsomorphismPermGroup(U, MappingByFunction(U, G, AsPermutation, 
   x-> AsBipartition(x, deg)));
   
  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, G);

  return U;
end);

#

InstallMethod(GroupOfUnits, "for a Rees 0-matrix subsemigroup with generators",
[IsReesZeroMatrixSubsemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, i, j, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  i:=MultiplicativeNeutralElement(s)![1];
  j:=MultiplicativeNeutralElement(s)![3];

  u:=Semigroup(List(GeneratorsOfGroup(g), x-> RMSElement(s, i, x, j)));
  
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(IdempotentGeneratedSubsemigroup, 
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Semigroup(Idempotents(s), rec(small:=true)));

#

InstallMethod(IdempotentGeneratedSubsemigroup, 
"for an inverse op acting semigroup with generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
s-> InverseSemigroup(Idempotents(s), rec(small:=true)));

#

InstallMethod(InjectionPrincipalFactor, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, inv_l, inv_r, lambdaperm, leftact, rightact, f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("usage: <d> must be a regular D-class,");
    return;
  fi;

  g:=GroupHClass(d);
  rep:=Representative(g);
  g:=Range(IsomorphismPermGroup(g));

  rreps:=HClassReps(LClass(d, rep));
  lreps:=HClassReps(RClass(d, rep));
  mat:=[];

  inv_l:=EmptyPlist(Length(lreps));
  inv_r:=EmptyPlist(Length(rreps));

  lambdaperm:=LambdaPerm(Parent(d));
  if IsTransformationSemigroupGreensClass(d) 
    or IsPartialPermSemigroupGreensClass(d) 
    or IsBipartitionSemigroupGreensClass(d) then 
    leftact:=PROD;
  elif IsReesZeroMatrixSubsemigroup(Parent(d)) then 
    leftact:=function(x, y)
      return Objectify(TypeObj(y), [y![1], y![4][rep![3]][rep![1]]^-1
      *x*rep![2]^-1*y![2], y![3], y![4]]);
    end;
  fi;

  rightact:=StabilizerAction(Parent(d));

  for i in [1..Length(lreps)] do
    mat[i]:=[];
    for j in [1..Length(rreps)] do
      f:=lreps[i]*rreps[j];
      if f in d then
        mat[i][j]:=lambdaperm(rep, f);
        if not IsBound(inv_r[j]) then
          # could use lreps[i]*rreps[j]^-1*lreps[i] instead if there was a
          # method for ^-1...
          inv_r[j]:=leftact(mat[i][j]^-1, lreps[i]);
        fi;
        if not IsBound(inv_l[i]) then
          inv_l[i]:=rightact(rreps[j], mat[i][j]^-1);
        fi;
      else
        mat[i][j]:=0;
      fi;
    od;
  od;
  
  if NrIdempotents(d)=NrHClasses(d) then 
    rms:=ReesMatrixSemigroup(g, mat);
  else
    rms:=ReesZeroMatrixSemigroup(g, mat);
  fi;
  
  iso:=function(f)
    local o, m, i, j;
    o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
    i:=Position(o, LambdaFunc(Parent(d))(f));

    if i=fail or OrbSCCLookup(o)[i]<>m then
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
      m:=RhoOrbSCCIndex(d);
    fi;
    j:=Position(o, RhoFunc(Parent(d))(f));
    if j=fail or OrbSCCLookup(o)[j]<>m then
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);
    return Objectify(TypeReesMatrixSemigroupElements(rms), 
     [j, lambdaperm(rep, rep*inv_r[j]*f*inv_l[i]), i, mat]);
  end;

  inv:=function(x)
    if x![1]=0 then 
      return fail;
    fi;
    return rightact(rreps[x![1]], x![2])*lreps[x![3]];
  end;
  
  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);

#

InstallMethod(IsomorphismReesMatrixSemigroup, 
"for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], InjectionPrincipalFactor);

#

InstallMethod(IrredundantGeneratingSubset,
"for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
  local gens, nrgens, deg, out, redund, i, f;
    
  if not (IsActingSemigroup(coll) or IsGeneratorsOfActingSemigroup(coll)) then 
    Error();
  fi;

  if IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll) then
    coll:=ShallowCopy(GeneratorsOfSemigroup(coll));
  fi;
  
  if Size(coll)=1 then 
    return coll;
  fi;

  gens:=Set(ShallowCopy(coll)); 
  nrgens:=Length(gens); 
  deg:=ActionDegree(coll);
  coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y) return ActionRank(x, deg)>ActionRank(y, deg); end);
 
  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;

  repeat
    i:=i+1; f:=coll[i];
    if InfoLevel(InfoSemigroups)>=3 then 
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund),
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then
      if f in Semigroup(Difference(gens, [f])) then
        AddSet(redund, f); gens:=Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund)+Length(out)=nrgens;

  if InfoLevel(InfoSemigroups)>1 then
    Print("\n");
  fi;
  return out;
end);

#

InstallMethod(IsomorphismReesMatrixSemigroup, 
"for a simple  or 0-simple acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local D, iso, inv;
  if not (IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S)) then 
    TryNextMethod();
  fi;
  D:=GreensDClasses(S)[1];
  if IsZeroSimpleSemigroup(S) 
   and IsMultiplicativeZero(S, Representative(D)) then 
    D:=GreensDClasses(S)[2];
  fi;
  iso:=IsomorphismReesMatrixSemigroup(D);
  inv:=InverseGeneralMapping(iso);
  return MagmaIsomorphismByFunctionsNC(S, Range(iso), x-> x^iso, x->x^inv);
end);

#

InstallMethod(InversesOfSemigroupElement, 
"for acting semigroup with generators and associative element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)

  if f in s then
    return InversesOfSemigroupElementNC(s, f);
  fi;

  return fail;
end);

#

InstallMethod(InversesOfSemigroupElementNC, 
"for an acting semigroup and acting element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts, grades, rho_f, lambdarank, creator, inv, out, k, g, rho, name, i, x;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  lambda:=LambdaFunc(s)(f);
  rank:=LambdaRank(s)(LambdaFunc(s)(f)); 
  rhorank:=RhoRank(s);
  tester:=IdempotentTester(s);
  j:=0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  # JDM is this really true?
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
    rhos:=EmptyPlist(Length(o));
    for rho in o do
      if rhorank(rho)=rank and tester(lambda, rho) then
        j:=j+1;
        rhos[j]:=rho;
      fi;
    od;
  else
      
    opts:=rec(  treehashsize:=s!.opts.hashlen.M, 
                gradingfunc:=function(o, x) return rhorank(x); end,
                onlygrades:=function(x, y) return x>=rank; end,
                onlygradesdata:=fail );
    
    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name):=LambdaOrbOpts(s).(name);
    od;

    o:=Orb(s, RhoOrbSeed(s), RhoAct(s), opts);
    Enumerate(o, infinity);
    
    grades:=Grades(o);
    rhos:=EmptyPlist(Length(o));
    for i in [2..Length(o)] do 
      if grades[i]=rank and tester(lambda, o[i]) then 
        j:=j+1;
        rhos[j]:=o[i];
      fi;
    od;
  fi;
  ShrinkAllocationPlist(rhos);
  
  rho_f:=RhoFunc(s)(f);
  lambdarank:=LambdaRank(s);
  creator:=IdempotentCreator(s);
  inv:=LambdaInverse(s);
  
  out:=[]; k:=0; 
 
  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    for i in [2..Length(o)] do
      if lambdarank(o[i])=rank and tester(o[i], rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(o[i], f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  else
     opts:=rec(  treehashsize:=s!.opts.hashlen.M, 
                gradingfunc:=function(o, x) return lambdarank(x); end,
                onlygrades:=function(x, y) return x>=rank; end,
                onlygradesdata:=fail ); #shouldn't this be fail
    
    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name):=LambdaOrbOpts(s).(name);
    od;

    o:=Orb(s, LambdaOrbSeed(s), LambdaAct(s), opts);
    Enumerate(o);
    grades:=Grades(o);
    
    for i in [2..Length(o)] do 
      if grades[i]=rank and tester(o[i], rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(o[i], f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  fi; 
 
  return out;
end);

#

InstallMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, rank, lambda, max, r, rep, f;

  gens:=Generators(s);
  rank:=LambdaRank(s);
  lambda:=LambdaFunc(s);
  max:=0;
  rep:=gens[1];
  
  for f in gens do 
    r:=rank(lambda(f));
    if r>max then 
      max:=r;
      rep:=f;
    fi;
  od;

  if max=ActionDegree(s) and IsMultiplicativeElementWithOneCollection(s) then
    return One(s);
  fi;

  r:=GreensRClassOfElementNC(s, rep);

  if not NrIdempotents(r)=1 then
    Info(InfoSemigroups, 2, "the number of idempotents in the R-class of the",
    " first maximum rank");
    Info(InfoSemigroups, 2, " generator is not 1");
    return fail;
  fi;

  f:=Idempotents(r)[1];

  if ForAll(gens, x-> x*f=x and f*x=x) then
    return f;
  fi;

  Info(InfoSemigroups, 2, "the unique idempotent in the R-class of the first",
  " maximum rank");
  Info(InfoSemigroups, 2, " generator is not the identity");
  return fail;
end);

# it just so happens that the MultiplicativeNeutralElement of a semigroup of
# partial permutations has to coincide with the One. This is not the case for
# transformation semigroups

InstallMethod(MultiplicativeNeutralElement, "for a partial perm semigroup",
[IsPartialPermSemigroup], One);

#

InstallMethod(MultiplicativeZero, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local min, o, rank, i, pos, f, m, rank_i, min_found, n;
  
  min:=MinActionRank(s);
  o:=LambdaOrb(s);
  rank:=LambdaRank(s);
  
  #is there an element in s with minimum possible rank
  if IsTransformationSemigroup(s) then 
    i:=0;
    repeat 
      i:=i+1;
      pos:=EnumeratePosition(o, [i], false);
    until pos<>fail or i=ActionDegree(s);
  elif IsPartialPermSemigroup(s) then 
    pos:=EnumeratePosition(o, [], false);
  else
    pos:=LookForInOrb(o, function(o, x) return rank(x)=min; end, 2);
  fi;

  if pos<>fail and pos<>false then
    f:=EvaluateWord(GeneratorsOfSemigroup(s), 
     TraceSchreierTreeForward(o, pos));
  else
    # lambda orb is closed, find an element with minimum rank
    min_found:=rank(o[2]); pos:=2; i:=1; 
    
    while min_found>min and i<Length(o) do 
      i:=i+1;
      rank_i:=rank(o[i]);
      if rank_i<min_found then 
        min_found:=rank_i;
        pos:=i;
      fi;
    od;
    f:=EvaluateWord(GeneratorsOfSemigroup(s), TraceSchreierTreeForward(o, pos));
  fi;

  if IsIdempotent(f) and Size(GreensRClassOfElementNC(s, f))=1 then
    return f;
  fi;

  return fail;
end);

#JDM better if this returned an actual semigroup ideal!!

InstallMethod(MinimalIdeal, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local rank, o, pos, min, len, m, f, d, I, n, i;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);
  
  pos:=LookForInOrb(o, function(o, x) return rank(x)=MinActionRank(s); end, 2);

  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);

    for i in [3..len] do 
      m:=rank(o[i]);
      if m<min then
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, pos));
  d:=GreensDClassOfElementNC(s, f);
  I:=Semigroup(Elements(d), rec(small:=true));
  SetMinimalDClass(s, d);
  SetIsSimpleSemigroup(I, true);
  return I; 
end);

# same method for inverse

InstallMethod(MinimalDClass, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local rank, o, pos, min, len, m, f, I, n, i;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);
  
  pos:=LookForInOrb(o, function(o, x) return rank(x)=MinActionRank(s); end, 2);

  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);
    for i in [3..len] do 
      m:=rank(o[i]);
      if m<min then
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, pos));
  return GreensDClassOfElementNC(s, f);
end);

#

InstallMethod(MinimalIdeal, "for an inverse acting semigroup",
[IsActingSemigroupWithInverseOp],
function(s)
  local rank, o, pos, min, len, m, f, I, n, i;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);

  pos:=LookForInOrb(o, function(o, x) return rank(x)=MinActionRank(s); end, 2);
  
  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);
    for i in [3..len] do 
      m:=rank(o[i]); 
      if m<min then 
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, pos));
  I:=InverseSemigroup(Elements(GreensDClassOfElementNC(s, f)), 
   rec(small:=true));
  SetIsGroupAsSemigroup(I, true);
  return I;
end);

#

InstallMethod(PrincipalFactor, "for a D-class", 
[IsGreensDClass and IsActingSemigroupGreensClass], 
d-> Range(InjectionPrincipalFactor(d)));

#

InstallMethod(PrincipalFactor, "for a D-class",
[IsGreensDClass], AssociatedReesMatrixSemigroupOfDClass);

#

InstallMethod(SmallGeneratingSet, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  if IsEmpty(Generators(s)) then 
    return Generators(s);
  else
    return Generators(Semigroup(Generators(s), rec(small:=true)));
  fi;
end);

#

InstallMethod(SmallGeneratingSet, 
"for an acting semigroup with inverse op and generators", 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(s)
  if IsEmpty(Generators(s)) then 
    return Generators(s);
  else
    return Generators(InverseSemigroup(Generators(s), rec(small:=true)));
  fi;
end);

#

InstallMethod(StructureDescription, "for an acting Brandt semigroup",
[IsActingSemigroup and IsBrandtSemigroup],
function(s)
  local x, d;
  
  x:=First(Generators(s), x-> x<>MultiplicativeZero(s));
  d:=GreensDClassOfElementNC(s, x);
  
  return Concatenation("B(", StructureDescription(GroupHClass(d)), ", ",
  String(NrRClasses(d)), ")");
end);

#

InstallMethod(StructureDescription, 
"for an acting group as semigroup",
[IsActingSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));

#

InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local id, dom, gens, inv;

  if IsMonoid(s) then 
    return MappingByFunction(s, s, IdFunc, IdFunc);
  fi;

  if MultiplicativeNeutralElement(s)=fail then 
    Error( "usage: <s> must have a multiplicative neutral element,");
    return;
  fi;
  
  id:=MultiplicativeNeutralElement(s);
  dom:=ImageSetOfTransformation(id);
  
  gens:=List(Generators(s), x-> TransformationOp(x, dom));

  inv:=function(f)
    local out, i;

    out:=[1..DegreeOfTransformationSemigroup(s)];
    for i in [1..Length(dom)] do 
      out[dom[i]]:=dom[i^f];
    od;
    return id*Transformation(out);
  end;

  return MappingByFunction(s, Monoid(gens), f-> TransformationOp(f, dom), 
   inv);
end);

#

InstallMethod(IsomorphismTransformationSemigroup, 
"for a matrix semigroup",
[IsMatrixSemigroup], 
function(S)        
  local n, F, T;
  n:=Length(GeneratorsOfSemigroup(S)[1][1]);
  F:=FieldOfMatrixList(GeneratorsOfSemigroup(S));        
  T:=Semigroup(List(GeneratorsOfSemigroup(S), x-> 
   TransformationOp(x, Elements(F^n), OnRight)));        
  return MappingByFunction(S, T,
   x-> TransformationOp(x, Elements(F^Size(F)), OnRight));
end);

#

InstallMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
 
  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a transformation semigroup satisfying IsGroupAsSemigroup,");
   return; 
  fi;

  return MagmaIsomorphismByFunctionsNC(s, 
   Group(List(Generators(s), PermutationOfImage)), 
    PermutationOfImage, 
    x-> AsTransformation(x, DegreeOfTransformationSemigroup(s)));
end);

#

InstallMethod(IsomorphismPermGroup,
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a partial perm semigroup satisfying IsGroupAsSemigroup,");
   return; 
  fi;

  return MagmaIsomorphismByFunctionsNC(s,
   Group(List(GeneratorsOfSemigroup(s), AsPermutation)),
    AsPermutation, x-> AsPartialPerm(x, DomainOfPartialPermCollection(s)));
end);

#

InstallMethod(IsomorphismPermGroup,
"for a subsemigroup of a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup and HasGeneratorsOfSemigroup],
function(s)
  local rep;

  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a subsemigroup of a Rees 0-matrix semigroup satisfying\n", 
    "IsGroupAsSemigroup,");
   return; 
  fi;

  rep:=s.1;
  if rep![1]=0 then # special case for the group consisting of 0
    return MagmaIsomorphismByFunctionsNC(s, Group(()), x-> (), x-> rep);
  fi;

  return MagmaIsomorphismByFunctionsNC(s, 
    Group(List(GeneratorsOfSemigroup(s), x-> x![2])),
      x-> x![2], x-> RMSElement(s, rep![1], x, rep![3]));
end);

#JDM  

InstallMethod(IsomorphismTransformationSemigroup, 
"for semigroup of binary relations with generators", 
[IsBinaryRelationSemigroup and HasGeneratorsOfSemigroup], 2, 
#to beat the method for a semigroups of general mappings
function(s)        
  local n, pts, o, t, pos, i;

  n:=DegreeOfBinaryRelation(Generators(s)[1]);
  pts:=EmptyPlist(2^n);

  for i in [1..n] do 
    o:=Orb(s, [i], OnPoints); #JDM multiseed orb
    Enumerate(o);
    pts:=Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  pos:=List([1..n], x-> Position(pts, [x]));
  t:=Semigroup(List(Generators(s), x-> TransformationOpNC(x, pts, OnPoints)));
  
  return MappingByFunction(s, t, x-> TransformationOpNC(x, pts, OnPoints),
  x-> BinaryRelationOnPoints(List([1..n], i-> pts[pos[i]^x])));
end);

#

InstallMethod(Size, "for a monogenic transformation semigroup",
[IsTransformationSemigroup and IsMonogenicSemigroup],
function(s)
  local ind;
  
  ind:=IndexPeriodOfTransformation(GeneratorsOfSemigroup(s)[1]);
  if ind[1]>0 then 
    return Sum(ind)-1;
  fi;
  return Sum(ind);
end);

#

Subsemigroups:=function(R)
  local max, o, U, V;
  
  max:=Set(MaximalSubsemigroups(R));
  o:=ShallowCopy(max);
  
  for U in o do 
    if Size(U)>1 then 
      for V in MaximalSubsemigroups(U) do 
        if not V in max then 
          AddSet(max, V);
          Add(o, V);
        fi;
      od;
    fi;
  od;

  return Concatenation(max, [R]);
end;

#

NumberOfSubsemigroups:=function(R)
  local max, o, U, V, count;
  
  max:=Set(MaximalSubsemigroups(R));
  o:=ShallowCopy(max);
  count:=Length(o)+1; # +1 for R itself
  
  while not IsEmpty(o) do
    U:=o[1];
    if Size(U)>1 then 
      for V in MaximalSubsemigroups(U) do 
        if not V in max then 
          AddSet(max, V);
          Add(o, V);
          count:=count+1;
          Print(count,"\n");
        fi;
      od;
    fi;
    Remove(o,1);
  od;

  return count;
end;


#EOF
