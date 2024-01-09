#############################################################################
##
##  tools/iterators.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# <convert_in> must return <fail> if it is not possible to convert
# <convert_out> must check if its argument is <fail> and if it is, then it
# should return <fail>, <convert_out> should have two arguments <enum> and <nr>
# where <nr> refers to the position in <baseenum>.

InstallGlobalFunction(WrappedEnumerator,
function(obj, baseenum, convert_out, convert_in)
  local record;

  if IsRecord(obj) and not IsBound(obj.parent) then
    ErrorNoReturn("if the 1st argument is a record, it must have component ",
                  "named `parent`");
  elif not IsList(baseenum) then
    ErrorNoReturn("the 2nd argument <baseenum> must be a list ");
  elif not (IsFunction(convert_out) and IsFunction(convert_in)) then
    ErrorNoReturn("the 3rd and 4th arguments <convert_out> and ",
                  "<convert_in> must be functions");
  fi;

  if IsRecord(obj) then
    record := obj;
  else
    record := rec();
    record.parent := obj;
  fi;

  record.baseenum    := baseenum;
  record.convert_out := convert_out;
  record.convert_in  := convert_in;

  record.NumberElement := function(enum, elt)
    local converted;
    converted := enum!.convert_in(enum, elt);
    if converted = fail then
      return fail;
    fi;
    return Position(enum!.baseenum, converted);
  end;

  record.ElementNumber := function(enum, nr)
    if nr > Length(enum) then
      return fail;
    fi;
    return enum!.convert_out(enum, enum!.baseenum[nr]);
  end;

  record.Length := enum -> Size(enum!.parent);

  return EnumeratorByFunctions(record.parent, record);
end);

# Helpers to replace the less helpful GAP library methods

BindGlobal("SEMIGROUPS_IsDoneIterator_List",
           iter -> (iter!.pos >= iter!.len));

BindGlobal("SEMIGROUPS_ShallowCopy_List",
    iter -> rec(list := iter!.list,
                pos  := 0,
                len  := iter!.len));

InstallGlobalFunction(IteratorFiniteList,
function(list)
  local it;
  it                 := IteratorList(list);
  it!.ShallowCopy    := SEMIGROUPS_ShallowCopy_List;
  it!.IsDoneIterator := SEMIGROUPS_IsDoneIterator_List;
  return it;
end);

# NextIterator in <opts> must return fail if the iterator is finished.

InstallGlobalFunction(IteratorByNextIterator,
function(record)
  local result;

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

  result := ShallowCopy(record);

  result._NextIterator := record.NextIterator;

  result._ShallowCopy  := record.ShallowCopy;

  result.last_called_by_is_done := false;

  result.next_value := fail;

  result.IsDoneIterator := function(iter)
    if iter!.last_called_by_is_done then
      return iter!.next_value = fail;
    fi;
    iter!.last_called_by_is_done := true;
    iter!.next_value := iter!._NextIterator(iter);
    if iter!.next_value = fail then
      return true;
    fi;
    return false;
  end;

  result.NextIterator := function(iter)
    if not iter!.last_called_by_is_done then
      IsDoneIterator(iter);
    fi;
    iter!.last_called_by_is_done := false;
    return iter!.next_value;
  end;

  result.ShallowCopy := function(iter)
    local copy;
    copy := iter!._ShallowCopy(iter);
    copy._NextIterator := iter!._NextIterator;
    copy._ShallowCopy  := iter!._ShallowCopy;
    copy.last_called_by_is_done := false;
    copy.next_value := fail;
    return copy;
  end;

  return IteratorByFunctions(result);
end);

# <baseiter> should be an iterator where NextIterator(baseiter) has a method
# for Iterator. More specifically, if iter:=Iterator(x) where <x> is a
# returned value of convert(NextIterator(baseiter)), then NextIterator of
# ChainIterators returns NextIterator(iter) until IsDoneIterator(iter)

InstallGlobalFunction(ChainIterators,
function(chain)
  local record;

  record := rec();

  if IsList(chain) then
    record.chain := Iterator(chain);
  elif IsIterator(chain) then
    record.chain := chain;
  else
    ErrorNoReturn("the argument <chain> must be an iterator or list");
  fi;

  record.current := Iterator(NextIterator(record.chain));

  record.IsDoneIterator := function(iter)
    return IsDoneIterator(iter!.chain) and
           IsDoneIterator(iter!.current);
  end;

  record.NextIterator := function(iter)
    if IsDoneIterator(iter) then
      return fail;
    elif IsDoneIterator(iter!.current) then
      iter!.current := Iterator(NextIterator(iter!.chain));
    fi;
    return NextIterator(iter!.current);
  end;

  record.ShallowCopy := iter -> rec(chain := ShallowCopy(iter!.chain),
                                    current := Iterator(NextIterator(~.chain)));

  return IteratorByFunctions(record);
end);

# for: baseiter, convert[, record, isnew]

InstallGlobalFunction(WrappedIterator,
function(arg...)
  local iter;

  iter   := rec();

  # prepare iterator rec()
  if IsBound(arg[3]) then
    Assert(1, IsRecord(arg[3]));
    iter := arg[3];
  fi;

  iter.baseiter := arg[1];

  if iter.baseiter!.ShallowCopy = ShallowCopy_List then
    iter.baseiter!.ShallowCopy := SEMIGROUPS_ShallowCopy_List;
    iter.baseiter!.IsDoneIterator := SEMIGROUPS_IsDoneIterator_List;
  fi;

  iter.unwrap := arg[2];

  if IsBound(arg[4]) then
    Assert(1, IsFunction(arg[4]));
    iter.isnew := arg[4];
  fi;

  iter.ShallowCopy := function(iter)
    local result, name;

    result := rec();
    result.baseiter := ShallowCopy(iter!.baseiter);
    result.unwrap   := iter!.unwrap;
    for name in NamesOfComponents(iter) do
      if not name in ["baseiter", "unwrap"] then
        result.(name) := iter!.(name);
      fi;
    od;
    return result;
  end;

  # get NextIterator
  if not IsBound(iter.isnew) then
    iter.IsDoneIterator := iter -> IsDoneIterator(iter!.baseiter);
    iter.NextIterator := function(iter)
      local x;
      x := NextIterator(iter!.baseiter);
      if x = fail then
        return fail;
      fi;
      return iter!.unwrap(iter, x);
    end;
    return IteratorByFunctions(iter);
  fi;

  iter.NextIterator := function(iter)
    local baseiter, x;
    baseiter := iter!.baseiter;
    repeat
      x := NextIterator(baseiter);
    until IsDoneIterator(baseiter) or iter!.isnew(iter, x);

    if x <> fail and iter!.isnew(iter, x) then
      return iter!.unwrap(iter, x);
    fi;
    return fail;
  end;

  return IteratorByNextIterator(iter);
end);
