#############################################################################
##
#W  io.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallGlobalFunction(IteratorFromGeneratorsFile, 
function(str)
  local file, record;
  
  file:=IO_CompressedFile(str, "r");
  
  if file=fail then 
    return fail;
  fi;
 
  record:=rec(file:=file, curr:=ReadGeneratorsLine(IO_ReadLine(file)));

  record.NextIterator:=function(iter)
    local next, line;
    next:=iter!.curr;
    line:=IO_ReadLine(iter!.file);
    if line<>"" then 
      iter!.curr:=ReadGeneratorsLine(line);
    else 
      iter!.curr:=line;
    fi;
    return next;
  end;
  
  record.IsDoneIterator:=function(iter)
    if iter!.curr="" then 
      if not iter!.file!.closed then 
        IO_Close(iter!.file);
      fi;
      return true;
    else
      return false;
    fi;
  end;

  record.ShallowCopy:=function(iter)
    local file;
    file:=IO_CompressedFile(str, "r");
    return rec(file:=file, curr:=ReadGeneratorsLine(IO_ReadLine(file)));
  end;

  InstallAtExit(function() if not file!.closed then IO_Close(file); fi; end);
  
  return IteratorByFunctions(record);
end);

#

InstallGlobalFunction(ReadGenerators, 
function(arg)
  local name, line_nr, file, i, line;

  if Length(arg) = 1 then 
    name := arg[1];
    line_nr := 0;
  elif Length(arg) = 2 then 
    name := arg[1];
    line_nr := arg[2];
  else
    Error("usage: there should be at most 2 arguments,");
    return;
  fi;

  if IsString(name) then 
    file:=IO_CompressedFile(name, "r");
    if file=fail then 
      return fail;
    fi;
  elif IsFile(name) then 
    file:=name;
  else
    Error("usage: the 1st argument must be a string or a file,");
    return;
  fi;

  if not (IsInt(line_nr) and line_nr >= 0) then 
    Error("usage: the 2nd argument must be a positive integer,");
    return;
  fi;
    
  if line_nr <> 0 then 
    i := 0;
    repeat  
      i := i + 1; 
      line := IO_ReadLine(file);
    until i = line_nr or line="";

    if IsString(arg[1]) then 
      IO_Close(file);
    fi;
    if line="" then
      Error("the file only has ", i-1, " lines,"); 
      return;
    else
      return ReadGeneratorsLine(Chomp(line));
    fi;
  else 
    line := IO_ReadLines(file);
    
    if IsString(arg[1]) then 
      IO_Close(file);
    fi;
    return List(line, x-> ReadGeneratorsLine(Chomp(x)));
  fi;
end);

#

InstallGlobalFunction(ReadGeneratorsLine, 
function(line)
  local i, k, out, m, deg, f, j;
 
  i:=2; k:=0; out:=[];

  while i<Length(line) do
    m:=Int([line[i]]);                                      # blocksize
    deg:=Int(NormalizedWhitespace(line{[i+1..m+i]}));       # max domain
    f:=line{[m+i+1..i+m*(deg+1)]};
    k:=k+1;
    out[k]:=EmptyPlist(deg);
    for j in [1..deg] do 
      Add(out[k], Int(NormalizedWhitespace(f{[(j-1)*m+1..j*m]})));
    od;
    i:=i+m*(deg+1)+1;
  od;
  
  if line[1]='t' then   # transformations
    Apply(out, TransformationNC); 
  elif line[1]='p' then # partial perms
    Apply(out, DensePartialPermNC);
  elif line[1]='b' then #bipartitions
    Apply(out, BipartitionByIntRepNC);
  fi;

  return out;
end);

#

InstallGlobalFunction(WriteGenerators, 
function(arg)
  local file, trans, gens, append, gzip, mode, line, deg, nrdigits, blocks, i, writin, s, f;
  
  if not (Length(arg)=3 or Length(arg)=2) then
    Error("usage: there should be 2 or 3 arguments,"); 
    return;
  fi;

  if IsString(arg[1]) then
    if IsBound(arg[3]) then 
      file:=IO_CompressedFile(arg[1], arg[3]);
    else 
      file:=IO_CompressedFile(arg[1], "a");
    fi;
  elif IsFile(arg[1]) then 
    file:=arg[1];
  else 
    Error("usage: the 1st argument must be a string or a file,");
    return;
  fi;

  if file=fail then 
    Error("couldn't open the file ", file, ",");
    return;
  fi;

  if IsTransformationCollection(arg[2]) 
    or IsPartialPermCollection(arg[2])
    or IsBipartitionCollection(arg[2]) then 
      trans:=[arg[2]];
  elif IsList(arg[2]) and IsBound(arg[2][1]) 
   and (IsTransformationCollection(arg[2][1]) 
    or IsPartialPermCollection(arg[2][1])
    or IsBipartitionCollection(arg[2][1])) then 
      trans:=arg[2];
  else
    Error("usage: the 2nd argument must be a transformation or partial perm\n",
    "semigroup or collection, or a list of such semigroups or collections,");
    return;
  fi;

  gens:=EmptyPlist(Length(trans));

  for i in [1..Length(trans)] do 
    if IsTransformationSemigroup(trans[i]) 
     or IsPartialPermSemigroup(trans[i]) 
     or IsBipartitionSemigroup(trans[i]) then 
      gens[i]:=GeneratorsOfSemigroup(trans[i]);
      # we could use a smaller generating set (i.e. GeneratorsOfMonoid,
      # GeneratorsOfInverseSemigroup etc) but we have no way of knowing which
      # generators we wrote, so better always use GeneratorsOfSemigroup
    else
      gens:=trans;
    fi;
  od;
  
  #####

  append:=function(str, pt, m)
    local i, j;
    
    i:=String(pt);
    for j in [1..m-Length(i)] do 
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;

  #####

  if IsTransformationCollection(gens[1]) then 
    for s in gens do
      line:="t";
      for f in s do
        deg:=String(DegreeOfTransformation(f));
        nrdigits:=Length(deg);
        Append(line, String(nrdigits));
        Append(line, deg);
        for i in [1..DegreeOfTransformation(f)] do 
          append(line, i^f, nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    od;
  elif IsPartialPermCollection(gens[1]) then 
    for s in gens do 
      line:="p";
      for f in s do 
        deg:=String(DegreeOfPartialPerm(f));
        nrdigits:=Length(String(Maximum(
         DegreeOfPartialPerm(f), CodegreeOfPartialPerm(f))));
        Append(line, String(nrdigits));
        append(line, deg, nrdigits);
        for i in [1..DegreeOfPartialPerm(f)] do 
          append(line, i^f, nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    od;
  elif IsBipartitionCollection(gens[1]) then 
    for s in gens do 
      line:="b";
      for f in s do 
        deg:=String(2*DegreeOfBipartition(f));
        nrdigits:=Length(deg);
        Append(line, String(nrdigits));
        Append(line, deg);
        blocks:=f!.blocks;
        for i in [1..Length(blocks)] do 
          append(line, blocks[i], nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    od;
  fi;
  
  if IsString(arg[1]) then  
    IO_Close(file);
  fi;
  return true;
end);

# JDM: this appears to be unused . . .

InstallGlobalFunction(WriteGeneratorsLine, 
function(f)
  local append, line, deg, strdeg, nrdigits, func, nr, i;
  
  append:=function(str, pt, m)
    local i, j;
    i:=String(pt);
    for j in [1..m-Length(i)] do 
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;

  if IsTransformation(f) then 
    line:="t";
    deg:=DegreeOfTransformation(f);
    strdeg:=String(deg);
    nrdigits:=Length(strdeg);
    func:=POW;
    nr:=deg;
  elif IsPartialPerm(f) then 
    line:="p";
    deg:=DegreeOfPartialPerm(f);
    strdeg:=String(deg);
    nrdigits:=Length(String(Maximum(deg, CodegreeOfPartialPerm(f))));
    func:=POW;
    nr:=deg;
  elif IsBipartition(f) then 
    line:="b";
    deg:=DegreeOfBipartition(f);
    strdeg:=String(deg);
    nrdigits:=Length(strdeg);
    func:=function(i, f)
      return f!.blocks[i];
    end;
    nr:=2*deg;
  fi;
  
  Append(line, String(nrdigits));
  Append(line, strdeg);
  for i in [1..nr] do 
    append(line, func(i,f), nrdigits);
  od;
  return line;
end);

#

InstallMethod(ShortStringRep, "for a transformation",
[IsTransformation],
function(f)
  local append, line, deg, nrdigits, i;

  append:=function(str, pt, m)
    local i, j;
    i:=String(pt);
    for j in [1..m-Length(i)] do 
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;
  line:=""; 
  deg:=String(DegreeOfTransformation(f));
  nrdigits:=Length(deg);
  Append(line, String(nrdigits));
  Append(line, deg);
  for i in [1..DegreeOfTransformation(f)] do 
    append(line, i^f, nrdigits);
  od;
  return line;
end);

