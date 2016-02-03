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

  record := rec(file := file, current := IO_Unpickle(file));

  record.NextIterator := function(iter)
    local next, line;
    next := iter!.current;
    iter!.current := IO_Unpickle(iter!.file);
    return next;
  end;

  record.IsDoneIterator := function(iter)
    if iter!.current = IO_Nothing then
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
    return rec(file := file, current := IO_Unpickle(file));
  end;

  return IteratorByFunctions(record);
end);

#

InstallGlobalFunction(ReadGenerators,
function(arg)
  local name, line_nr, file, i, obj, out;

  if Length(arg) = 1 then
    name := arg[1];
    line_nr := 0; # read all lines
  elif Length(arg) = 2 then
    name := arg[1];
    line_nr := arg[2];
  else
    ErrorNoReturn("Semigroups: ReadGenerators: usage,\n",
                  "there should be at most 2 arguments,");
  fi;

  if IsString(name) then
    file := IO_CompressedFile(name, "r");
    if file = fail then
      ErrorNoReturn("Semigroups: ReadGenerators:\n",
                    "could not open the file ", file, ",");
    fi;
  elif IsFile(name) then
    file := name;
  else
    ErrorNoReturn("Semigroups: ReadGenerators: usage,\n",
                  "the first argument must be a string or a file,");
  fi;

  if not (IsInt(line_nr) and line_nr >= 0) then
    ErrorNoReturn("Semigroups: ReadGenerators: usage,\n",
                  "the second argument must be a positive integer,");
  fi;

  if line_nr <> 0 then
    i := 0;
    repeat
      i := i + 1;
      obj := IO_Unpickle(file);
    until i = line_nr or obj = IO_Nothing;

    if IsString(arg[1]) then
      IO_Close(file);
    fi;
    if obj = IO_Nothing then
      ErrorNoReturn("Semigroups: ReadGenerators:\n",
                    "the file only has ", i - 1, " rows,");
    fi;
    return obj;
  else
    i := 0;
    out := [];
    obj := IO_Unpickle(file);
    while obj <> IO_Nothing do
      i := i + 1;
      out[i] := obj;
      obj := IO_Unpickle(file);
    od;

    if IsString(arg[1]) then
      IO_Close(file);
    fi;
    return out;
  fi;
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
    ErrorNoReturn("Semigroups: WriteGenerators: usage,\n",
                  "there should be 2 or 3 arguments,");
  fi;

  if not (mode = "a" or mode = "w") then
    ErrorNoReturn("Semigroups: WriteGenerators: usage,\n",
                  "the third argument must be \"a\" or \"w\",");
  fi;

  if IsString(name) then
    file := IO_CompressedFile(name, mode);
    if file = fail then
      ErrorNoReturn("Semigroups: WriteGenerators:\n",
                    "couldn't open the file ", name, ",");
    fi;
  elif IsFile(name) then
    file := name;
  else
    ErrorNoReturn("Semigroups: WriteGenerators: usage,\n",
                  "the first argument must be a string or a file,");
  fi;

  for i in [1 .. Length(coll)] do
    if IsSemigroup(coll[i]) then
      coll[i] := GeneratorsOfSemigroup(coll[i]);
      # we could use a smaller generating set (i.e. GeneratorsOfMonoid,
      # GeneratorsOfInverseSemigroup etc) but we have no way of knowing which
      # generators we wrote, so better always use GeneratorsOfSemigroup
    fi;
  od;

  for x in coll do
    if IO_Pickle(file, x) = IO_Error then
      return false;
    fi;
  od;

  if IsString(arg[1]) then
    IO_Close(file);
  fi;
  return true;
end);

SEMIGROUPS.ReadGeneratorsLine := function(line)
  local i, k, out, m, deg, f, j;

  i := 2;
  k := 0;
  out := [];

  while i < Length(line) do
    m := Int([line[i]]);                                      # blocksize
    deg := Int(NormalizedWhitespace(line{[i + 1 .. m + i]})); # max domain
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
    Apply(out, BIPART_NC);
  fi;

  return out;
end;

InstallGlobalFunction(ReadOldGenerators,
function(arg)
  local name, line_nr, file, i, line, ReadGeneratorsLine;

  if Length(arg) = 1 then
    name := arg[1];
    line_nr := 0;
  elif Length(arg) = 2 then
    name := arg[1];
    line_nr := arg[2];
  else
    Error("Semigroups: ReadOldGenerators: usage,\n",
          "there should be at most 2 arguments,");
    return;
  fi;

  if IsString(name) then
    file := IO_CompressedFile(name, "r");
    if file = fail then
      Error("Semigroups: ReadOldGenerators:\n",
            "could not open the file ", file, ",");
      return;
    fi;
  elif IsFile(name) then
    file := name;
  else
    Error("Semigroups: ReadOldGenerators: usage,\n",
          "the first argument must be a string or a file,");
    return;
  fi;

  if not (IsInt(line_nr) and line_nr >= 0) then
    Error("Semigroups: ReadOldGenerators: usage,\n",
          "the second argument must be a positive integer,");
    return;
  fi;

  ReadGeneratorsLine := SEMIGROUPS.ReadGeneratorsLine;

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
      Error("Semigroups: ReadOldGenerators:\n",
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

