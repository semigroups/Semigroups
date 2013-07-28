
#

InstallMethod(RightBlocks, "for a bipartition", [IsBipartition],
function(f)
  local n, blocks, tab, out, nrblocks, i;
  
  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  tab:=EmptyPlist(2*n);
  out:=[];
  nrblocks:=0;

  for i in [n+1..2*n] do 
    if not IsBound(tab[blocks[i]]) then 
      nrblocks:=nrblocks+1;
      tab[blocks[i]]:=nrblocks;
      if blocks[i]<=NrLeftBlocks(f) then 
        out[n+1+nrblocks]:=1; #signed
      else 
        out[n+1+nrblocks]:=0; #unsigned
      fi;
    fi;
    out[i-n+1]:=tab[blocks[i]];
  od;
  out[1]:=nrblocks;
  return out;
end);

# could use TransverseBlocksLookup if known here JDM

InstallMethod(LeftBlocks, "for a bipartition", [IsBipartition],
function(f)
  local n, blocks, tab, out, nrblocks, i;
  
  n:=DegreeOfBipartition(f);
  blocks:=f!.blocks;
  tab:=List([1..n], x-> false);
  out:=EmptyPlist(n+2);
  out[1]:=0;
  out[n+2]:=[];
  nrblocks:=0;

  for i in [1..n] do 
    out[i+1]:=blocks[i];
    if not tab[blocks[i]] then 
      out[1]:=out[1]+1;
      out[n+1+blocks[i]]:=0;
      tab[blocks[i]]:=true;
    fi;
  od;
  
  for i in [n+1..2*n] do 
    if blocks[i]<=out[1] then #transverse block!
      out[n+1+blocks[i]]:=1;
    fi;
  od;

  return out;
end);

# JDM use FuseRightBlocks here!

InstallGlobalFunction(OnRightBlocks, 
function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, tab, out,
   next, i;

  n:=DegreeOfBlocks(blocks); # length of partition!!
  nrblocks:=blocks[1];
  
  if nrblocks=0 then   # special case for dummy/seed 
    return RightBlocks(f);
  fi;

  nrfblocks:=NrBlocks(f); 
  fblocks:=f!.blocks;
  
  fuse:=[1..nrblocks+nrfblocks];
  sign:=EmptyPlist(nrfblocks+nrblocks);

  for i in [1..nrblocks] do 
    sign[i]:=blocks[n+1+i];
  od;
  for i in [nrblocks+1..nrfblocks+nrblocks] do 
    sign[i]:=0;
  od;

  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  for i in [1..n] do
    x := fuseit(blocks[i+1]);
    y := fuseit(fblocks[i]+nrblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y]=1 then 
          sign[x]:=1;
        fi;
      else
        fuse[x] := y;
        if sign[x]=1 then 
          sign[y]:=1;
        fi;
      fi;
    fi;
  od;

  tab:=0*fuse;
  out:=[];
  next:=0;
  
  for i in [n+1..2*n] do
    x := fuseit(fblocks[i]+nrblocks);
    if tab[x]=0 then
      next:=next+1;
      tab[x]:=next;
    fi;
    out[i-n+1]:=tab[x];
    out[n+1+tab[x]]:=sign[x];
  od;
  out[1]:=next;
  return out;
end);

#

InstallGlobalFunction(OnLeftBlocks, 
function(blocks, f)
  local n, nrblocks, nrfblocks, fblocks, fuse, sign, fuseit, x, y, tab, out, next, i;

  n:=DegreeOfBlocks(blocks);  # length of <blocks>
  nrblocks:=blocks[1];
  
  if nrblocks=0 then 
    return LeftBlocks(f);
  fi;

  nrfblocks:=NrBlocks(f);
  fblocks:=f!.blocks;
  
  fuse:=[1..nrfblocks+nrblocks];
  sign:=EmptyPlist(nrfblocks+nrblocks);

  for i in [1..nrfblocks] do 
    sign[i]:=0;
  od;
  for i in [1..nrblocks] do 
    sign[i+nrfblocks]:=blocks[n+1+i];
  od;

  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  for i in [1..n] do
    x := fuseit(fblocks[n+i]);
    y := fuseit(blocks[i+1]+nrfblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y]=1 then 
          sign[x]:=1; 
        fi;
      else
        fuse[x] := y;
        if sign[x]=1 then 
          sign[y]:=1;
        fi;
      fi;
    fi;
  od;

  tab:=0*fuse;
  out:=[];
  next:=0;
  
  for i in [1..n] do
    x := fuseit(fblocks[i]);
    if tab[x]=0 then
      next := next + 1;
      tab[x] := next;
    fi;
    out[i+1]:=tab[x];
    out[n+1+tab[x]]:=sign[x];
  od;
  out[1]:=next;
  return out;
end);

#

InstallGlobalFunction(ExtRepOfBlocks,
function(blocks)
  local n, sign, out, i;
  
  n:=DegreeOfBlocks(blocks);
  out:=EmptyPlist(n);
  for i in [1..n] do 
    out[i]:=blocks[i+1];
    if blocks[n+1+blocks[i+1]]=0 then 
      out[i]:=out[i]*-1;
    fi;
  od;
    
  return out;
end);

#

InstallGlobalFunction(BlocksByExtRep,
function(ext)
  local n, tab, out, nr, i;
  
  n:=Length(ext);
  tab:=EmptyPlist(n);
  out:=EmptyPlist(n+2);
  out[n+2]:=[];
  nr:=0;
  
  for i in [1..n] do
    if ext[i]<0 then 
      out[i+1]:=-1*ext[i];
      out[n+1+out[i+1]]:=0;
    else
      out[i+1]:=ext[i];
      out[n+1+ext[i]]:=1;
    fi;
    if not IsBound(tab[out[i+1]]) then 
      tab[out[i+1]]:=true;
      nr:=nr+1;
    fi;
  od;

  out[1]:=nr;
  return out;
end);

#

InstallGlobalFunction(RankOfBlocks, 
function(blocks)
  local n, rank, i;
  
  n:=DegreeOfBlocks(blocks);
  rank:=0;
  for i in [1..blocks[1]] do 
    if blocks[n+1+i]=1 then 
      rank:=rank+1;
    fi;
  od;
  return rank;
end);

#

InstallGlobalFunction(DegreeOfBlocks,
function(blocks)
  return Length(blocks)-blocks[1]-1;
end);


InstallGlobalFunction(BlocksIdempotentTester,
function(lambda, rho)
  local n, lambdanr, rhonr, fuse, fuseit, sign, x, y, seen, i;

  if DegreeOfBlocks(lambda)<>DegreeOfBlocks(rho) then 
    Error("the degrees of the blocks <lambda> and <rho> must be equal,");
    return;
  fi;

  if RankOfBlocks(lambda)<>RankOfBlocks(rho) then 
    return false;
  fi;

  if RankOfBlocks(lambda)=0 then 
    return true;
  fi;

  n:=DegreeOfBlocks(lambda);
  lambdanr:=lambda[1]; 
  rhonr:=rho[1];

  fuse:=[1..lambdanr+rhonr];
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  sign:=[1..lambdanr]*0;
  for i in [lambdanr+1..lambdanr+rhonr] do #copy the signs from <rho>
    sign[i]:=rho[n+1+i-lambdanr]; 
  od;
  
  for i in [1..n] do
    x := fuseit(lambda[i+1]);
    y := fuseit(rho[i+1]+lambdanr);
    if x <> y then
      if x < y then
        fuse[y] := x;
        if sign[y]=1 then
          sign[x]:=1;
        fi;
      else
        fuse[x] := y;
        if sign[x]=1 then 
          sign[y]:=1;
        fi;
      fi;
    fi;
  od;

  #check if we are injective on signed classes of <lambda> and that the fused
  #blocks are also signed. 

  seen:=BlistList([1..lambdanr], []);
  for i in [1..lambdanr] do 
    if lambda[n+1+i]=1 then # is block <i> a signed block?
      x:=fuseit(i);
      if seen[x] or sign[x]=0 then 
        return false;
      fi;
      seen[x]:=true;
    fi;
  od;
  return true;
end);

# assumes that BlocksIdempotentTester returns true!

InstallGlobalFunction(BlocksIdempotentCreator,
function(lambda, rho)
  local n, lambdanr, rhonr, fuse, fuseit, x, y, tab1, tab2, out, next, i;

  n:=DegreeOfBlocks(lambda);
  lambdanr:=lambda[1]; 
  rhonr:=rho[1];

  fuse:=[1..lambdanr+rhonr];
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  for i in [1..n] do
    x := fuseit(lambda[i+1]);
    y := fuseit(rho[i+1]+lambdanr);
    if x <> y then
      if x < y then
        fuse[y] := x;
      else
        fuse[x] := y;
      fi;
    fi;
  od;

  tab1:=EmptyPlist(lambdanr);

  #find new names for the signed blocks of rho
  for i in [1..rhonr] do 
    if rho[n+1+i]=1 then 
      tab1[fuseit(i+lambdanr)]:=i;
    fi;
  od;
  
  tab2:=EmptyPlist(lambdanr);
  out:=EmptyPlist(2*n);
  next:=rho[1];
  
  for i in [1..n] do 
    out[i]:=rho[i+1];
    if lambda[n+1+lambda[i+1]]=1 then 
      out[i+n]:=tab1[fuseit(lambda[i+1])];
    else
      if not IsBound(tab2[lambda[i+1]]) then 
        next:=next+1;
        tab2[lambda[i+1]]:=next;
      fi;
      out[i+n]:=tab2[lambda[i+1]];
    fi;
  od;

  out:=Objectify(BipartitionType, rec(blocks:=out)); 
  SetDegreeOfBipartition(out, n);
  SetRankOfBipartition(out, RankOfBlocks(rho));
  SetNrLeftBlocks(out, rho[1]);
  SetNrBlocks(out, next);
  return out;
end);

# permutation of indices of signed (connected) blocks of <blocks> under the
# action of <f> which is assumed to stabilise <blocks>.

InstallMethod(PermRightBlocks, "for blocks and bipartition",
[IsList, IsBipartition],
function(blocks, f)
  local n, nrblocks, fblocks, fuseit, signed, tab, next, x, i;

  n:=DegreeOfBlocks(blocks); # length of partition!!
  nrblocks:=blocks[1];
  fblocks:=f!.blocks;

  fuseit:=FuseRightBlocks(blocks, f, false); 
  signed:=[]; tab:=[]; next:=0;

  # JDM could stop here after reaching the maximum signed class of <blocks>
  for i in [n+1..2*n] do 
    if blocks[n+1+blocks[i-n]]=1 then 
      Add(signed, blocks[i-n]);
    fi;
    x:=fuseit(fblocks[i]+nrblocks);
    if not IsBound(tab[x]) then 
      next:=next+1;
      tab[x]:=next;
    fi;
  od;
  
  return MappingPermListList(signed, List(signed, i-> tab[fuseit(i)]));
end);

# LambdaInverse - fuse <blocks> with the left blocks of <f> keeping track of
# the signs of the fused classes. 
# 
# The left blocks of the output are then:
# 1) disconnected right blocks of <f> (before fusing)
# 2) disconnected right blocks of <f> (after fusing) 
# 3) connected right blocks of <f> (after fusing)
# both types 1+2 of the disconnected blocks are unioned into one left block of
# the output with index <junk>. The connected blocks 3 of <f> are given the next
# available index, if they have not been seen before. The table <tab1> keeps
# track of which connected right blocks of <f> have been seen before and the
# corresponding index in the output, i.e. <tab1[x]> is the index in <out> of
# the fused block with index <x>.

# The right blocks of the output are:
# 1) disconnected blocks of <blocks>; or
# 2) connected blocks of <blocks>.
# The disconnected blocks 1 are given the next available index, if they have
# not been seen before. The table <tab2> keeps
# track of which disconnected blocks of <blocks> have been seen before and the
# corresponding index in the output, i.e. <tab2[x]> is the index in <out> of
# the disconnected block of <blocks> with index <x>. The connected blocks 2 of
# <blocks> is given the index <tab1[x]> where <x> is the fused index of the
# block.

InstallGlobalFunction(InverseRightBlocks,
function(blocks, f)
  local n, nrblocks, fblocks, fusesign, fuse, sign, fuseit, out, junk, next,
   tab1, x, nrleft, tab2, i;

  n:=DegreeOfBlocks(blocks); # length of partition!!
  nrblocks:=blocks[1];
  fblocks:=f!.blocks;
  
  fusesign:=FuseRightBlocks(blocks, f, true); 
  fuseit:=fusesign[1];
  sign:=fusesign[2];

  out:=[]; junk:=0; next:=0;
 
  # find the left blocks of the output
  tab1:=[];
  for i in [1..n] do 
    if fblocks[i+n]>NrLeftBlocks(f) then #disconnected before fusing
      if junk=0 then 
        next:=next+1;
        junk:=next;
      fi;
      out[i]:=junk;
    else
      x:=fuseit(fblocks[i+n]+nrblocks);
      if sign[x]=0 then #disconnected after fusing
        if junk=0 then 
          next:=next+1;
          junk:=next;
        fi;
        out[i]:=junk;
      else              # connected block
        if not IsBound(tab1[x]) then 
          next:=next+1;
          tab1[x]:=next;
        fi;
        out[i]:=tab1[x];
      fi;
    fi;
  od;
  nrleft:=next;
  
  # find the right blocks of the output
  tab2:=[];
  for i in [n+1..2*n] do 
    x:=blocks[i-n+1];
    if blocks[n+1+x]=1 then 
      x:=fuseit(x);
      out[i]:=tab1[x];
    else
      if not IsBound(tab2[x]) then 
        next:=next+1;
        tab2[x]:=next;
      fi;
      out[i]:=tab2[x];
    fi;
  od;

  out:=Objectify(BipartitionType, rec(blocks:=out));

  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, nrleft);
  SetNrBlocks(out, next);
  return out;
end);

# RhoInverse

InstallGlobalFunction(InverseLeftBlocks,
function(blocks, f)
  local n, nrblocks, fblocks, fuseit, out, tab, x, i;

  n:=DegreeOfBlocks(blocks); # length of partition!!
  nrblocks:=blocks[1];
  fblocks:=f!.blocks;
  
  fuseit:=FuseLeftBlocks(blocks, f); 
  out:=[]; tab:=[];

  for i in [1..nrblocks] do 
    if blocks[n+1+i]=1 then 
      tab[fuseit(i)]:=i;
    fi;
  od;

  # find the left blocks of the output
  for i in [1..n] do
    out[i]:=blocks[i+1];
    x:=fuseit(fblocks[i]+nrblocks);
    if x>blocks[1] or not IsBound(tab[x]) then 
      out[i+n]:=blocks[1]+1; #junk
    else
      out[i+n]:=tab[x];
    fi;
  od;    

  out:=Objectify(BipartitionType, rec(blocks:=out));
  SetDegreeOfBipartition(out, n);
  SetNrLeftBlocks(out, blocks[1]);
  SetNrBlocks(out, blocks[1]+1);
  return out;
end);

# fuse <blocks> with <f>. <sign> should be true to keep track of signed and
# unsigned blocks and false not to keep track.

InstallGlobalFunction(FuseRightBlocks,
function(blocks, f, sign)
  local n, fblocks, nrblocks, nrfblocks, fuse, fuseit, x, y, i;
  
  n:=DegreeOfBlocks(blocks);
  fblocks:=f!.blocks;
  nrblocks:=blocks[1];
  nrfblocks:=NrBlocks(f);

  fuse:=[1..nrblocks+nrfblocks];
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;
  
  if sign then 
    sign:=EmptyPlist(nrfblocks+nrblocks);

    for i in [1..nrblocks] do
      sign[i]:=blocks[n+1+i];
    od;
    for i in [nrblocks+1..nrfblocks+nrblocks] do
      sign[i]:=0;
    od;
    
    for i in [1..n] do
      x := fuseit(blocks[i+1]);
      y := fuseit(fblocks[i]+nrblocks);
      if x <> y then
        if x < y then
          fuse[y] := x;
          if sign[y]=1 then
            sign[x]:=1;
          fi;
        else
          fuse[x] := y;
          if sign[x]=1 then
            sign[y]:=1;
          fi;
        fi;
      fi;
    od;
    return [fuseit, sign];
  else 
    for i in [1..n] do
      x := fuseit(blocks[i+1]);
      y := fuseit(fblocks[i]+nrblocks);
      if x <> y then
        if x < y then
          fuse[y] := x;
        else
          fuse[x] := y;
        fi;
      fi;
    od;
    return fuseit;
  fi;
end);

# 

InstallGlobalFunction(FuseLeftBlocks,
function(blocks, f)
  local n, fblocks, nrblocks, nrfblocks, fuse, fuseit, x, y, i;
  
  n:=DegreeOfBlocks(blocks);
  fblocks:=f!.blocks;
  nrblocks:=blocks[1];
  nrfblocks:=NrBlocks(f);

  fuse:=[1..nrblocks+nrfblocks];
  fuseit := function(i) 
    while fuse[i] < i do 
      i := fuse[i]; 
    od; 
    return i; 
  end;

  for i in [1..n] do
    x := fuseit(blocks[i+1]);
    y := fuseit(fblocks[n+i]+nrblocks);
    if x <> y then
      if x < y then
        fuse[y] := x;
      else
        fuse[x] := y;
      fi;
    fi;
  od;
  return fuseit;
end);

