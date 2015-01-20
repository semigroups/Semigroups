#############################################################################
##
#W  io.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallGlobalFunction(IteratorFromGeneratorsFile,
function(str)
  local file, record;

  file := IO_CompressedFile(str, "r");

  if file = fail then
    return fail;
  fi;

  record := rec(file := file, curr := ReadGeneratorsLine(IO_ReadLine(file)));

  record.NextIterator := function(iter)
    local next, line;
    next := iter!.curr;
    line := IO_ReadLine(iter!.file);
    if line <> "" then
      iter!.curr := ReadGeneratorsLine(line);
    else
      iter!.curr := line;
    fi;
    return next;
  end;

  record.IsDoneIterator := function(iter)
    if iter!.curr = "" then
      if not iter!.file!.closed then
        IO_Close(iter!.file);
      fi;
      return true;
    else
      return false;
    fi;
  end;

  record.ShallowCopy := function(iter)
    local file;
    file := IO_CompressedFile(str, "r");
    return rec(file := file, curr := ReadGeneratorsLine(IO_ReadLine(file)));
  end;

  # TODO is this still necessary?
  InstallAtExit(
    function()
      if not file!.closed then
        IO_Close(file);
      fi;
    end);

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
    Error("Semigroups: ReadGenerators: usage,\n",
          "there should be at most 2 arguments,");
    return;
  fi;

  if IsString(name) then
    file := IO_CompressedFile(name, "r");
    if file = fail then
      Error("Semigroups: ReadGenerators:\n",
            "could not open the file ", file, ",");
      return;
    fi;
  elif IsFile(name) then
    file := name;
  else
    Error("Semigroups: ReadGenerators: usage,\n",
          "the first argument must be a string or a file,");
    return;
  fi;

  if not (IsInt(line_nr) and line_nr >= 0) then
    Error("Semigroups: ReadGenerators: usage,\n",
          "the second argument must be a positive integer,");
    return;
  fi;

  if line_nr <> 0 then
    i := 0;
    repeat
      i := i + 1;
      line := IO_ReadLine(file);
    until i = line_nr or line = "";

    if IsString(arg[1]) then
      IO_Close(file);
    fi;
    if line = "" then
      Error("Semigroups: ReadGenerators:\n",
            "the file only has ", i - 1, " lines,");
      return;
    else
      return ReadGeneratorsLine(Chomp(line));
    fi;
  else
    line := IO_ReadLines(file);

    if IsString(arg[1]) then
      IO_Close(file);
    fi;
    return List(line, x -> ReadGeneratorsLine(Chomp(x)));
  fi;
end);

#

InstallGlobalFunction(ReadGeneratorsLine,
function(line)
  local i, k, out, m, deg, f, j;

  i := 2;
  k := 0;
  out := [];

  while i < Length(line) do
    m := Int([line[i]]);                                      # blocksize
    deg := Int(NormalizedWhitespace(line{[i + 1 .. m + i]}));       # max domain
    f := line{[m + i + 1 .. i + m * (deg + 1)]};
    k := k + 1;
    out[k] := EmptyPlist(deg);
    for j in [1 .. deg] do
      Add(out[k], Int(NormalizedWhitespace(f{[(j - 1) * m + 1 .. j * m]})));
    od;
    i := i + m * (deg + 1) + 1;
  od;

  if line[1] = 't' then   # transformations
    Apply(out, TransformationNC);
  elif line[1] = 'p' then # partial perms
    Apply(out, DensePartialPermNC);
  elif line[1] = 'b' then #bipartitions
    Apply(out, BipartitionByIntRepNC);
  fi;

  return out;
end);

#

InstallGlobalFunction(WriteGenerators,
function(arg)
  local name, coll, mode, file, append, line, deg, nrdigits, blocks, i, x, f;

  if Length(arg) = 2 then
    name := arg[1];
    coll := arg[2];
    mode := "w";
  elif Length(arg) = 3 then
    name := arg[1];
    coll := arg[2];
    mode := arg[3];
  else
    Error("Semigroups: WriteGenerators: usage,\n",
          "there should be 2 or 3 arguments,");
    return;
  fi;

  if not (mode = "a" or mode = "w") then
    Error("Semigroups: WriteGenerators: usage,\n",
          "the third argument must be \"a\" or \"w\",");
  fi;

  if IsTransformationCollection(coll)
    or IsPartialPermCollection(coll)
    or IsBipartitionCollection(coll) then
    coll := [ coll ];
  elif not (IsTransformationCollColl(coll)
    or IsPartialPermCollColl(coll)
    or IsBipartitionCollColl(coll)) then
    Error("Semigroups: WriteGenerators: usage,\n",
          "the second arg must be a transformation, partial perm,\n",
          "or bipartition collection, or coll coll,");
    return;
  fi;

  if IsString(name) then
    file := IO_CompressedFile(name, mode);
    if file = fail then
      Error("Semigroups: WriteGenerators:\n",
            "couldn't open the file ", name, ",");
      return;
    fi;
  elif IsFile(name) then
    file := name;
  else
    Error("Semigroups: WriteGenerators: usage,\n",
          "the first argument must be a string or a file,");
    return;
  fi;

  for i in [ 1 .. Length(coll) ] do
    if IsSemigroup(coll[i]) then
      coll[i] := GeneratorsOfSemigroup(coll[i]);
      # we could use a smaller generating set (i.e. GeneratorsOfMonoid,
      # GeneratorsOfInverseSemigroup etc) but we have no way of knowing which
      # generators we wrote, so better always use GeneratorsOfSemigroup
    fi;
  od;

  #####

  append := function(str, pt, m)
    local i, j;

    i := String(pt);
    for j in [1 .. m - Length(i)] do
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;

  #####

  for x in coll do
    if IsTransformationCollection(x) then
      line := "t";
      for f in x do
        deg := String(DegreeOfTransformation(f));
        nrdigits := Length(deg);
        Append(line, String(nrdigits));
        Append(line, deg);
        for i in [1 .. DegreeOfTransformation(f)] do
          append(line, i ^ f, nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    elif IsPartialPermCollection(x) then
      line := "p";
      for f in x do
        deg := String(DegreeOfPartialPerm(f));
        nrdigits := Length(String(Maximum(
         DegreeOfPartialPerm(f), CodegreeOfPartialPerm(f))));
        Append(line, String(nrdigits));
        append(line, deg, nrdigits);
        for i in [1 .. DegreeOfPartialPerm(f)] do
          append(line, i ^ f, nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    elif IsBipartitionCollection(x) then
      line := "b";
      for f in x do
        deg := String(2 * DegreeOfBipartition(f));
        nrdigits := Length(deg);
        Append(line, String(nrdigits));
        Append(line, deg);
        blocks := f!.blocks;
        for i in [1 .. Length(blocks)] do
          append(line, blocks[i], nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    fi;
  od;

  if IsString(arg[1]) then
    IO_Close(file);
  fi;
  return true;
end);

#

InstallMethod(ShortStringRep, "for a transformation",
[IsTransformation],
function(f)
  local append, line, deg, nrdigits, i;

  append := function(str, pt, m)
    local i, j;
    i := String(pt);
    for j in [1 .. m - Length(i)] do
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;
  line := "";
  deg := String(DegreeOfTransformation(f));
  nrdigits := Length(deg);
  Append(line, String(nrdigits));
  Append(line, deg);
  for i in [1 .. DegreeOfTransformation(f)] do
    append(line, i ^ f, nrdigits);
  od;
  return line;
end);

