#############################################################################
##
##  tools/iterators.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Not sure why this is here.

BindGlobal("SEMIGROUPS_IsDoneIterator_List",
           iter -> (iter!.pos >= iter!.len));
           
BindGlobal("SEMIGROUPS_ShallowCopy_List",
    iter -> rec(list := iter!.list,
                pos  := 0,
                len  := iter!.len));

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
function(arg)
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

  iter.unwrap   := arg[2];

  if IsBound(arg[4]) then
    Assert(1, IsFunction(arg[4]));
    iter.isnew := arg[4];
  fi;

  iter.ShallowCopy := iter -> rec(baseiter := ShallowCopy(iter!.baseiter),
                                  unwrap   := iter!.unwrap);

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
