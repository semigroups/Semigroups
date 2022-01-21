#############################################################################
##
##  iterators.gi
##  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

MakeReadWriteGlobal("IsDoneIterator_List");
UnbindGlobal("IsDoneIterator_List");
BindGlobal("IsDoneIterator_List",
           iter -> (iter!.pos >= iter!.len));

# No attempt has been made to get good test coverage for this file, since it
# will hopefully be binned in the near future.

# technical...

# returns func(iter, pos) once at least position <pos> of the orbit <o> is
# known, the starting value for <pos> is <start>. The idea is that this returns
# something that depends on position <pos> of <o> being known but does not
# depend on the entire orbit being known.

InstallGlobalFunction(IteratorByOrbFunc,
function(o, func, start)
  local func2, record;

  if not IsOrbit(o) then
    ErrorNoReturn("the 1st argument <o> must be an orbit");
  elif not IsFunction(func) then
    ErrorNoReturn("the 2nd argument <func> must be a function");
  fi;

  # change 1 argument <func> to 2 argument
  if NumberArgumentsFunction(func) = 1 then
    func2 := function(iter, x)
      return func(x);
    end;
  else
    func2 := func;
  fi;

  record := rec();
  record.pos := start - 1;

  record.NextIterator := function(iter)
    local pos;

    pos := iter!.pos;

    if IsClosedOrbit(o) and pos >= Length(o) then
      return fail;
    fi;

    pos := pos + 1;

    if pos > Length(o) then
      Enumerate(o, pos);
      if pos > Length(o) then
        return fail;
      fi;
    fi;

    iter!.pos := pos;
    return func2(iter, pos);
  end;

  record.ShallowCopy := iter -> rec(pos := 1);

  return IteratorByNextIterator(record);
end);

# NextIterator in <opts> must return fail if the iterator is finished.

InstallGlobalFunction(IteratorByNextIterator,
function(record)
  local iter, comp, shallow;

  if not (IsRecord(record) and IsBound(record.NextIterator)
                           and IsBound(record.ShallowCopy)) then
    ErrorNoReturn("the argument <record> must be a record with components ",
                  "`NextIterator' and `ShallowCopy'");
  elif IsRecord(record) and (IsBound(record.last_called_by_is_done)
                             or IsBound(record.next_value)
                             or IsBound(record.IsDoneIterator)) then
    ErrorNoReturn("the argument <record> must be a record with no ",
                  "components named `last_called_by_is_done', ",
                  "`next_value', or `IsDoneIterator'");
  fi;

  iter := rec(last_called_by_is_done := false,

              next_value := fail,

              IsDoneIterator := function(iter)
                if iter!.last_called_by_is_done then
                  return iter!.next_value = fail;
                fi;
                iter!.last_called_by_is_done := true;
                iter!.next_value := record!.NextIterator(iter);
                if iter!.next_value = fail then
                  return true;
                fi;
                return false;
              end,

              NextIterator := function(iter)
                if not iter!.last_called_by_is_done then
                  IsDoneIterator(iter);
                fi;
                iter!.last_called_by_is_done := false;
                return iter!.next_value;
              end);

  for comp in RecNames(record) do
    if comp <> "NextIterator" then
      iter.(comp) := record.(comp);
    fi;

  od;
  shallow := record.ShallowCopy(iter);
  shallow.last_called_by_is_done := false;
  shallow.next_value := fail;

  iter.ShallowCopy := iter -> shallow;

  return IteratorByFunctions(iter);
end);

# <baseiter> should be an iterator where NextIterator(baseiter) has a method
# for Iterator. More specifically, if iter:=Iterator(x) where <x> is a
# returned value of convert(NextIterator(baseiter)), then NextIterator of
# IteratorByIterOfIters returns NextIterator(iter) until IsDoneIterator(iter)

InstallGlobalFunction(IteratorByIterOfIters,
function(record, baseiter, convert, filts)
  local iter, filt;

  if not IsRecord(record) or IsBound(record.baseiter)
      or IsBound(record.iterofiters) or IsBound(record.IsDoneIterator)
      or IsBound(record.NextIterator) or IsBound(record.ShallowCopy) then
    ErrorNoReturn("the 1st argument <record> must be a record with ",
                  "no components named: `baseiter', `iterofiters',",
                  "`IsDoneIterator', `NextIterator', or `ShallowCopy'");
  elif not IsIterator(baseiter) then
    ErrorNoReturn("the 2nd argument <baseiter> must be an iterator");
  elif not IsFunction(convert) then
    ErrorNoReturn("the 3rd argument <convert> must be a function");
  elif not (IsList(filts) and ForAll(filts, IsFilter)) then
    ErrorNoReturn("the 4th argument <filts> must be a list of filters");
  fi;

  record.baseiter := baseiter;
  record.iterofiters := Iterator(convert(NextIterator(baseiter)));

  #
  record.IsDoneIterator := function(iter)
    return IsDoneIterator(iter!.baseiter) and
           IsDoneIterator(iter!.iterofiters);
  end;

  #
  record.NextIterator := function(iter)

    if IsDoneIterator(iter) then
      return fail;
    fi;

    if IsDoneIterator(iter!.iterofiters) then
      iter!.iterofiters := Iterator(convert(NextIterator(iter!.baseiter)));
    fi;

    return NextIterator(iter!.iterofiters);
  end;

  #
  record.ShallowCopy := iter -> rec(baseiter := baseiter,
                                    iterofiters := fail);

  iter := IteratorByFunctions(record);

  for filt in filts do
    SetFilterObj(iter, filt);
  od;

  return iter;
end);

# for: baseiter, convert[, list of filts, isnew, record]

InstallGlobalFunction(IteratorByIterator,
function(arg)
  local iter, filt, convert, isnew;

  # process incoming functions
  if NumberArgumentsFunction(arg[2]) = 1 then
    convert := function(iter, x)
      return arg[2](x);
    end;
  else
    convert := arg[2];
  fi;

  if not IsBound(arg[3]) then
    arg[3] := [];
  fi;

  if IsBound(arg[4]) and IsFunction(arg[4]) then
    if NumberArgumentsFunction(arg[4]) = 1 then
      isnew := function(iter, x)
        return arg[4](x);
      end;
    else
      isnew := arg[4];
    fi;
  fi;

  # prepare iterator rec()
  if IsBound(arg[5]) then
    iter := arg[5];
  else
    iter := rec();
  fi;

  iter.baseiter := arg[1];

  iter.ShallowCopy := iter -> rec(baseiter := ShallowCopy(arg[1]));

  # get NextIterator
  if not IsBound(isnew) then
    iter.IsDoneIterator := iter -> IsDoneIterator(iter!.baseiter);
    iter.NextIterator := function(iter)
      local x;
      x := NextIterator(iter!.baseiter);
      if x = fail then
        return fail;
      fi;
      return convert(iter, x);
    end;
    iter := IteratorByFunctions(iter);
  else
    iter.NextIterator := function(iter)
      local baseiter, x;
      baseiter := iter!.baseiter;
      repeat
        x := NextIterator(baseiter);
      until IsDoneIterator(baseiter) or isnew(iter, x);

      if x <> fail and isnew(iter, x) then
        return convert(iter, x);
      fi;
      return fail;
    end;
    iter := IteratorByNextIterator(iter);
  fi;

  for filt in arg[3] do  # filters
    SetFilterObj(iter, filt);
  od;

  return iter;
end);
