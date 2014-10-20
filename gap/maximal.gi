#############################################################################
##
#W  maximal.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# WAW The more complicated version incorporating maximal subsemigroup theory did
# not seem to perform significantly better and so was removed.

InstallMethod(IsMaximalSubsemigroup, "for an acting semigroup and acting semigroup", 
[IsSemigroup, IsSemigroup],
function(S, T)
  if IsSubsemigroup(S, T) and S<>T then 
    return ForAll(S, x-> x in T or Semigroup(GeneratorsOfSemigroup(T), x)=S);
  else
    return false;
  fi;
end); 

#

InstallMethod(MaximalSubsemigroups, "for a Rees matrix subsemigroup and a group",
[IsReesMatrixSubsemigroup, IsGroup], 
function(R, H)
  local G, basicgens, i, j, I, J, mat;
 
  if not IsReesMatrixSemigroup(R) then 
    TryNextMethod();
    return;
  fi;
 
  G:=UnderlyingSemigroup(R);
  
  if not IsGroup(G) then 
    return fail;
  elif not IsSubgroup(G,H) then
    return fail;
  elif not H in MaximalSubgroups(G) then
    return fail;
  fi;
  
  mat:=Matrix(R);     I:=Length(mat[1]);         J:=Length(mat);
  
  # Produce a necessary elt for every row and column for our generating set
  basicgens:=[];  
  for i in [1..Minimum(I,J)] do
    Add(basicgens, RMSElement(R, i, (mat[i][i]^-1), i));
  od;
  for i in [J+1..I] do
    Add(basicgens, RMSElement(R, i, (mat[1][i]^-1), 1)); 
  od;
  for j in [I+1..J] do
    Add(basicgens, RMSElement(R, 1, (mat[j][1]^-1), j)); 
  od;
  
  return MaximalSubsemigroupsNC(R, H, basicgens, mat[1][1]);

end);

#

InstallMethod(MaximalSubsemigroupsNC, "for a Rees matrix subsemigroup and a group",
[IsReesMatrixSubsemigroup, IsGroup, IsList, IsAssociativeElement], 
function(R, H, basicgens, h)
  local U;
  
  U:=Semigroup(basicgens, List(GeneratorsOfGroup(H), 
   x->RMSElement(R, 1, x*(h^-1), 1)));
  if Size(U)<Size(R) then
    return [U];
  fi;
  
  return [];
end);

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees matrix semigroup over groups

InstallMethod(MaximalSubsemigroups, "for a Rees matrix subsemigroup",
[IsReesMatrixSubsemigroup], 
function(R)
  local G, out, mat, I, J, P, U, H, j, i, basicgens;
  
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
  basicgens:=[];
  # The below 3 loops use more code, but ensure a smallest generating set
  for i in [1..Minimum(Length(I),Length(J))] do
    Add(basicgens, RMSElement(R, i, (mat[i][i]^-1), i));
  od;
  for i in [Length(J)+1..Length(I)] do
    Add(basicgens, RMSElement(R, i, (mat[1][i]^-1), 1)); 
  od;
  for j in [Length(I)+1..Length(J)] do
    Add(basicgens, RMSElement(R, 1, (mat[j][1]^-1), j)); 
  od;
  for H in MaximalSubgroups(G) do
    out:=Concatenation(out, MaximalSubsemigroupsNC(R, H, basicgens, mat[1][1]));
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
    Info(InfoSemigroups, 3, "...found ", Length(J));
  else
    Info(InfoSemigroups, 3, "...found none");
  fi;
  
  # Case 3: the dual of case 2.
  Info(InfoSemigroups, 3, 
   "Case 3: maximal subsemigroups obtained by removing a row...");
  if Length(I)>1 then 
    for i in I do 
      Add(out, ReesMatrixSubsemigroupNC(R, Difference(I, [i]), G, J));
    od;
    Info(InfoSemigroups, 3, "...found ", Length(I));
  else
    Info(InfoSemigroups, 3, "...found none");
  fi;

  return out;
end);

#

if not IsGrapeLoaded then 
  InstallMethod(MaximalSubsemigroups, 
  "for a Rees 0-matrix subsemigroup and a group",
  [IsReesZeroMatrixSubsemigroup, IsGroup], 
  function(R, H)
    Info(InfoWarning, 1, GrapeIsNotLoadedString);
    return fail;
  end); 
else
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup and a group",
  [IsReesZeroMatrixSubsemigroup, IsGroup], 
  function(R, H)
    local G, mat, graph, basicgens, i, j, maxgens, I, J;

    if not IsReesZeroMatrixSemigroup(R) then 
      TryNextMethod(); 
      return;
    fi;
    
    # Check that matrix is regular (i.e. no zero rows or columns)
    if not IsRegularSemigroup(R) then
      Error("not yet implemented,");
      return;
    fi;
     
    G:=UnderlyingSemigroup(R);    
      
    if not IsGroup(G) then 
      return fail;
    elif not IsSubgroup(G,H) then
      return fail;
    elif not H in MaximalSubgroups(G) then
      return fail;
    fi;

    mat:=Matrix(R); I:=Rows(R); J:=Columns(R);
    graph:=RZMSGraph(R);
    
    # Add to the generators one element which *must* be in each group H-class of
    # any maximal subsemigroup of the desired Case 1 form.
    basicgens:=[MultiplicativeZero(R)];
    for i in Rows(R) do
      for j in Columns(R) do
        if mat[j][i] <> 0 then
          Add(basicgens, RMSElement(R, i, mat[j][i]^-1, j));      
        fi;
      od;
    od;
          
    # Pick a distinguished group H-class in the first component: H_i,j
    # For each maximal subgroup H we have: H_i,j = (i, H*(mat[j][i]^-1), j)  
    i:=1; j:=graph.adjacencies[1][1] - Length(mat[1]);
    
    return MaximalSubsemigroupsNC(R, H, graph, ConnectedComponents(graph),
     basicgens, [i, j]);
  end);
fi;

#

InstallMethod(MaximalSubsemigroupsNC, "for a Rees 0-matrix subsemigroup and a group",
[IsReesZeroMatrixSubsemigroup, IsGroup, IsRecord, IsList, IsList, IsList], 
function(R, H, graph, components, basicgens, indices)
  local nrcomponents, nrrows, NonGroupRecursion, out, i, j, maxgens, Hsize, transversal, mat;
  
  out:=[];
  mat:=Matrix(R);
  i:=indices[1];
  j:=indices[2];
  nrcomponents:=Length(components);
  nrrows:=Length(mat[1]); 
  Hsize:=Size(H);
  transversal:=RightTransversal(UnderlyingSemigroup(R),H);
  maxgens:=List(GeneratorsOfSemigroup(H), 
   x-> RMSElement(R, i, x*(mat[j][i]^-1), j));

  # Recursive depth-first search    
  NonGroupRecursion:=function(k, t, choice)
    local nextchoice, x, a, b, h;
    
    # Generate the semigroup which includes our most recent choice
    if k = 1 then
      t:=Semigroup(basicgens, choice);
    else
      t:=ClosureSemigroup(t, choice);
    fi;

    # Test if adding our new choice has already made too much stuff
    # The below logical condition need to be improved if possible
    if Size(GreensHClassOfElementNC(t, choice[1]))<=Hsize then
      
      # Make next choice, if any left to make.
      if k < nrcomponents then
        # We are making our choice for H_i,b; this forces our choice of H_a,j
        a:=components[k+1][1];
        b:=graph.adjacencies[a][1] - nrrows;
        # one choice for each coset
        for x in transversal do
          h:=mat[b][a]^(-1) * x^(-1) * mat[j][i]^(-1);
          nextchoice:=[RMSElement(R, i, x, b), RMSElement(R, a, h, j)];
          NonGroupRecursion(k+1, t, nextchoice);
        od;
      else
        Add(out, t);
      fi;
    fi;
      
    # At this stage, can we rule out other cases from the level above?
    return;
  end;
    
  NonGroupRecursion(1, fail, maxgens);
  return out;
end);

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees 0-matrix semigroup over groups

if not IsGrapeLoaded or not IsGrapeCompiled then 
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
  [IsReesZeroMatrixSubsemigroup], 
  function(R)
    Info(InfoWarning, 1, GrapeIsNotCompiledString);
    return fail;
  end); 
else
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
  [IsReesZeroMatrixSubsemigroup], 
  function(R)
    local G, out, I, J, mat, nrrows, graph, components, nrcomponents, P, basicgens, new, pos, i, j, NonGroupRecursion, transversal, maxgens, JJ, solo, U, II, len, names, rectangles, gens, H, r, k;
    
    if not IsReesZeroMatrixSemigroup(R) then 
      TryNextMethod(); 
      return;
    fi;
    
    # Check that matrix is regular (i.e. no zero rows or columns)
    if not IsRegularSemigroup(R) then
      Error("not yet implemented,");
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
    
    out:=[]; I:=Rows(R); J:=Columns(R); nrrows:=Length(I); mat:=Matrix(R);

    # find the set of group elements in the matrix
    
    if Length(I)=1 and Length(J)=1 and IsTrivial(G) then 
      # the unique case when {0} is a maximal subsemigroup, if <G> is
      # non-trivial then {0, 1_G} is a subsemigroup containing <0> and not
      # equal to <R>. 
      Add(out, Semigroup(MultiplicativeZero(R)));
    fi;
    
    graph:=RZMSGraph(R);
    components:=ConnectedComponents(graph);
    nrcomponents:=Length(components);

    Info(InfoSemigroups, 3, 
     "...the matrix has ", nrcomponents, " connected component(s)");
  
    # Add to the generators one element which *must* be in each group H-class of
    # any maximal subsemigroup of the Case 1 form.
    P:=[];
    basicgens:=[MultiplicativeZero(R)];
    for i in I do
      for j in J do
        AddSet(P, mat[j][i]);
        if mat[j][i] <> 0 then
          Add(basicgens, RMSElement(R, i, mat[j][i]^-1, j)); 
        fi;
      od;
    od;
  
    if P[1]=0 then 
      Remove(P, 1);
      pos:=0;
    else  # S\{0} is a maximal subsemigroup 
          # the unique case when the missing D-class is {0}
      new:=ShallowCopy(GeneratorsOfSemigroup(R));
      if Size(new)>1 then  
        pos:=Position(new, MultiplicativeZero(R)); 
        Remove(new, pos); #remove the zero, which has to be present
      fi;
      Add(out, Semigroup(new));
      pos:=1;
      Info(InfoSemigroups, 3, "found maximal subsemigroup by removing the 0...");
    fi;
    
    # Case 1: maximal subsemigroups of the form (IxHxJ)\cup\{0\} where H is a
    # maximal subgroup of G
    
    Info(InfoSemigroups, 3, 
     "Case 1: maximal subsemigroups arising from maximal subgroups...");
    
    # Pick a distinguished group H-class in the first component: H_i,j
    # For each maximal subgroup H we have: H_i,j = (i, H*(mat[j][i]^-1), j)  
    i:=1; j:=graph.adjacencies[1][1] - nrrows;
  
    # For each max subgroup, start recursion with basic gens, and gens for H_i,j
    for H in MaximalSubgroups(G) do
      out:=Concatenation(out,
       MaximalSubsemigroupsNC(R, H, graph, components, basicgens, [i, j]));
    od;
    
    Info(InfoSemigroups, 3, "...found ", Length(out)-pos);

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

    Info(InfoSemigroups, 3, "...found ", Length(II)); pos:=Length(out);

    # Case 4: maximal rectangle of zeros in the matrix
    Info(InfoSemigroups, 3, 
     "Case 4: maximal subsemigroups obtained by removing a rectangle...");
    Info(InfoSemigroups, 3, "finding rectangles...");

    len:=Length(mat[1]); # use <mat> to keep the indices correct

    graph:=ComplementGraph(RZMSGraph(R));
    names:=x-> graph.names[x];
    
    graph:=NewGroupGraph(AutomorphismGroup(graph), graph);
    rectangles:=CompleteSubgraphs(graph);
    
    # Surely not the best way to let the automorphism gp act on the rectangles
    rectangles:=Set(Concatenation(
                 List(rectangles, x->Orbit(graph.autGroup, x, OnSets))));
    # A hack to get round problems caused by immutability...
    rectangles:=List(rectangles,ShallowCopy);

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
      Add(out, Semigroup(new, [MultiplicativeZero(R)]));
    od;
    Info(InfoSemigroups, 3, "...found ", Length(out)-pos);
    return out;
  end);
fi;

#

if not (IsGrapeLoaded and IsGrapeCompiled) then 
  InstallMethod(MaximalSubsemigroups, "for an acting semigroup", 
  [IsActingSemigroup], 
  function(S) 
    Info(InfoWarning, 1, GrapeIsNotCompiledString);
    return fail;
  end);
else
  InstallMethod(MaximalSubsemigroups, "for an acting semigroup",
  [IsActingSemigroup],
  function(S)
    local out, gens, po, reps, classes, D, lookup, count, max, found_case1,
    nonmax, tot, gens2, pos, inj, R, V, tuples, ideal, UnionOfHClassRecursion,
    HClassClosure	, U, A, XX, a, C, i, j, k, gens3, UU, G, I, J, H, basicgens,
    graph, components, nrcomponents, rows, NonGroupRecursion, transversal,
    maxgens, mat, h, ii, jj, lastideal;
    
    # Trivial semigroup has no proper subsemigroups so no maximal subsemigroups
    if Size(S) = 1 then
      return [];
    fi;
    
    # Subsemigroups of finite groups are subgroups; hence so are maximal ones
    if IsGroupAsSemigroup(S) then
      max:=function(S)
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
      return max(S);
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
      # Calculate S \ classes[i]
      gens2:=ShallowCopy(gens){Difference([1..Length(gens)], lookup[i])};
      pos:=Position(po[i], i);
      if pos<>fail then 
        Remove(po[i], pos);
      fi;
      Append(gens2, List(classes{po[i]}, Representative));
      if not IsEmpty(gens2) then
        Info(InfoSemigroups, 2, "calculating the ideal S minus D");
        V:=SemigroupIdealByGenerators(S, gens2);
      else
        V:=[];
      fi;
      
      if Size(classes[i])=1 then   # Remove the whole of any trivial D-class
        Add(out, V);   # V <> [] since S is non-trivial at this point
      else   # Adjoin maximal subsemigroups of principal factor to S\D
        if not IsEmpty(V) then
          V:=GeneratorsOfSemigroup(V);
        fi;
        inj:=InverseGeneralMapping(InjectionPrincipalFactor(classes[i]));
        R:=Source(inj);
        for U in MaximalSubsemigroups(R) do
          if IsSimpleSemigroup(R) then 
            # We don't want to remove multiplicative
            # zero in this case, if it exists
            Add(out, Semigroup(
              OnTuples(GeneratorsOfSemigroup(U), inj), V));
          else # Remove 0 from the generators since it's not an elt of classes[i]
            tuples:=OnTuples(Filtered(
              GeneratorsOfSemigroup(U), x-> not IsMultiplicativeZero(R, x)), inj);
            Add(out, Semigroup(V, tuples));
          fi;
          # Don't need to worry about U = {0}, which could only happen if
          # Size(classes[i]) = 1. So tuples is always non-empty
        od;
      fi;
      Info(InfoSemigroups, 2, "found ", Length(out)-tot, 
       " maximal subsemigroups in total in this D-class");
      tot:=Length(out);
    od;

    # Info statements
    if not IsEmpty(nonmax) then
      Info(InfoSemigroups, 2, "finding maximal subsemigroups arising from", 
      " non-maximal D-classes...");
    else
      Info(InfoSemigroups, 2, "no non-maximal D-classes to consider...");
    fi;
    
    # Type 2: maximal subsemigroups arising from non-maximal D-classes
    for i in nonmax do 
      Info(InfoSemigroups, 2, "considering D-class ", i); 
      # Calculate D-class reps directly below classes[i]
      pos:=Position(po[i], i);
      if pos<>fail then
        Remove(po[i], pos);
      fi;
      reps:=List(classes{po[i]}, Representative);
      # Calculate ideal of D-class reps directly below classes[i] if necessary
      if not(IsBound(lastideal) and lastideal = reps) then
        if not IsEmpty(reps) then
          Info(InfoSemigroups, 2, "calculating ideal..."); 
          ideal:=GeneratorsOfSemigroup(SemigroupIdeal(S, reps));
        else
          ideal:=[];
        fi;
        lastideal:=reps;
      fi;
      
      if not IsRegularDClass(classes[i]) then #remove the whole thing...
        gens2:=ShallowCopy(gens);
        Remove(gens2, lookup[i][1]); # there's only one generator in the D-class
        Add(out, Semigroup(gens2, ideal));
        Info(InfoSemigroups, 2, "found maximal subsemigroup arising from", 
        " removing whole non-maximal non-regular D-class...");
      else # <classes[i]> is regular; lots of work to be done

        UnionOfHClassRecursion:=function(U, known, A, depth)
          local ismax, new_known, a, V, didtest, h, new_depth;
          new_depth:=depth+1;
          count:=count+1;
          Info(InfoSemigroups, 3, "call: ", count, ", depth: ", new_depth,"\r");
          #Print("call: ", count, ", depth: ", new_depth,"\r");
          ismax:=true; 
          new_known:=ShallowCopy(known);
          didtest:=false;
          while not IsEmpty(A) do
            a:=A[1];
            h:=HClass(S,a);
            if not ForAny(h, x->x in known) then 
              didtest:=true;
              V:=Semigroup(U, h);
              # Above line could be: V:=HClassClosure(Semigroup(U, h)); but this
              # is slower
              if ForAll(XX, x-> not x in V) then #i.e. check that V<>S
                ismax:=false;
                if ForAll(new_known, x-> not x in V) then 
                  UnionOfHClassRecursion(V, new_known, Difference(A, V), new_depth);
                fi;
                new_known:=Union(new_known, h);
              fi;
              A:=Difference(A,h);
            fi;
          od;
          #if ismax and didtest then
          if ismax then
            if not ForAny(out, W-> IsSubsemigroup(W, U)) then 
              Add(out, Semigroup(U, ideal));
              Info(InfoSemigroups, 2, "found maximal subsemigroup arising from",
              " UnionOfHClassRecursion");
            fi;
          fi;
          return;
        end;
        
        # Case 1: Max. subsemigroups which intersect every H-class of classes[i]
        Info(InfoSemigroups, 2, "Case 1: Looking for maximal subsemigroups ",
          "which intersect every H-class of the D-class");
        gens3:=gens{lookup[i]}; # gens3 is the set of generators in classes[i]
        gens2:=Difference(ShallowCopy(gens), gens3);
        U:=Semigroup(gens2);
      
        inj:=InverseGeneralMapping(InjectionPrincipalFactor(classes[i]));
        R:=Source(inj);
        G:=UnderlyingSemigroup(R);
        I:=Length(Rows(R)); J:=Length(Columns(R));
        mat:=Matrix(R);      
        tot:=0;

        if IsReesMatrixSemigroup(R) then
          basicgens:=[]; 
          for ii in [1..Minimum(I,J)] do
            Add(basicgens, RMSElement(R, ii, (mat[ii][ii]^-1), ii));
          od;
          for ii in [J+1..I] do
            Add(basicgens, RMSElement(R, ii, (mat[1][ii]^-1), 1)); 
          od;
          for jj in [I+1..J] do
            Add(basicgens, RMSElement(R, 1, (mat[jj][1]^-1), jj)); 
          od;
          
          for H in MaximalSubgroups(G) do
            for UU in MaximalSubsemigroupsNC(R,H,basicgens,mat[1][1]^-1) do
              UU:=Semigroup(Images(inj, GeneratorsOfSemigroup(UU)), U);
              # UU (along with the ideal) is either maximal or equals S. So check
              # if it lacks some of our generating set
              if ForAny(gens3, x-> not x in UU) then
                Info(InfoSemigroups, 2, "found maximal subsemigroup which ", 
                 "intersects every H-class of the D-class (RMS-type)");
                Add(out, Semigroup(GeneratorsOfSemigroup(UU), ideal));
                tot:=tot+1;
              fi;
            od;
          od;
    
        elif IsReesZeroMatrixSemigroup(R) then
    
          graph:=RZMSGraph(R);
          components:=ConnectedComponents(graph);
          # Add to the generators one element which *must* be in each group
          # H-class of any maximal subsemigroup of the Case 1 form.
          basicgens:=[];
          for ii in [1..I] do
            for jj in [1..J] do
              if mat[jj][ii] <> 0 then
                Add(basicgens, RMSElement(R, ii, (mat[jj][ii]^-1), jj));
              fi;
            od;
          od;
    
          # Pick a distinguished group H-class in the first component: H_i,jj
          # For each maximal subgroup H we have: H_i,jj = (ii,
          # H*(mat[jj][ii]^-1), jj)
          ii:=1; jj:=graph.adjacencies[1][1] - I;
        
          # For each max subgroup, start recursion with basic gens, and gens for
          # H_ii,jj
          for H in MaximalSubgroups(G) do
            for UU in MaximalSubsemigroupsNC(R, H, graph, components, basicgens,
              [ii, jj]) do
              UU:=Semigroup(Images(inj, GeneratorsOfSemigroup(UU)), U);
              # UU (along with the ideal) is either maximal or equals S. So check
              # if it lacks some of our generating set
              if ForAny(gens3, z-> not z in UU) then
                Info(InfoSemigroups, 2, "found maximal subsemigroup which ", 
                "intersects every H-class of the D-class (RZMS-type)");
                Add(out, Semigroup(GeneratorsOfSemigroup(UU), ideal));
                tot:=tot+1;
              fi;
            od;
          od;
        fi;  
        if tot > 0 then
          found_case1:=true;
          Info(InfoSemigroups, 2, "Found ", tot, " such result(s)");
        else
          found_case1:=false;
          Info(InfoSemigroups, 2, "Found no such results");
        fi;
        
        # Case 2: Max. subsemigroups which are a union of H-classes in classes[i]
        Info(InfoSemigroups, 2, "Case 2: Looking for maximal subsemigroups ",
          "which are a union of H-classes");
        for k in [1..Length(lookup[i])] do
          for j in Combinations(lookup[i], k) do
            Info(InfoSemigroups, 2, "Trying to remove gens: ", j, "...");
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
              # if k > 1, we are done since our gen set is irredund.
            else
              A:=Filtered(classes[i], x-> not (x in XX or x in U));
              if IsEmpty(A) then 
                if k = 1 then
                  Add(out, Semigroup(GeneratorsOfSemigroup(U), ideal));
                  Info(InfoSemigroups, 2, "found maximal subsemigroup arising", 
                  " from removing all of XX; A is empty");
                fi;
                # if k > 1, we are done since our gen set is irredund.
              else # not IsEmpty(A)
                V:=Semigroup(U, A, ideal, rec(small:=true));
                if V<>S then
                  if k = 1 then
                    Add(out, V);
                    Info(InfoSemigroups, 2, "found maximal subsemigroup arising",
                    " by removing all of XX, and keeping all of A");
                  elif ForAll(XX, x->not x in V) 
                   and not ForAny(out, W->IsSubsemigroup(W, V)) then
                    Add(out, V);
                    Info(InfoSemigroups, 2, "found maximal subsemigroup arising",
                    " by removing all of XX, and keeping all of A");
                  fi;
                else
                  
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
                    UnionOfHClassRecursion(U, [], A, 0);
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
fi;

#

#Subsemigroups:=function(R)
#  local max, o, U, V;
#  
#  max:=Set(MaximalSubsemigroups(R));
#  o:=ShallowCopy(max);
#  
#  for U in o do 
#    if Size(U)>1 then 
#      for V in MaximalSubsemigroups(U) do 
#        if not V in max then 
#          AddSet(max, V);
#          Add(o, V);
#        fi;
#      od;
#    fi;
#  od;
#
#  return Concatenation(max, [R]);
#end;
#
##
#
#NumberOfSubsemigroups:=function(R)
#  local max, o, U, V, count;
#  
#  max:=Set(MaximalSubsemigroups(R));
#  o:=ShallowCopy(max);
#  count:=Length(o)+1; # +1 for R itself
#  
#  while not IsEmpty(o) do
#    U:=o[1];
#    if Size(U)>1 then 
#      for V in MaximalSubsemigroups(U) do 
#        if not V in max then 
#          AddSet(max, V);
#          Add(o, V);
#          count:=count+1;
#          Print(count,"\n");
#        fi;
#      od;
#    fi;
#    Remove(o,1);
#  od;
#
#  return count;
#end;
