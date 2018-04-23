#############################################################################
##
##  iterators.gi
##  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# No attempt has been made to get good test coverage for this file, since it
# will hopefully be binned in the near future.

InstallGlobalFunction(IteratorSortedOp,
function(arg)
  local record, iter;

  if Length(arg) = 1 and IsList(arg[1]) then
    arg := arg[1];
  fi;

  if Length(arg) > 0 and IsIteratorSorted(arg[1]) then
    record := rec();
    record.indices := [1 .. Length(arg)];
    record.base := arg;
    record.current := [];
    for iter in record.base do
      Add(record.current, NextIterator(iter));
    od;

    record.NextIterator := function(iter)
      local current, indices, next, pos, i, base;

      current := iter!.current;
      indices := iter!.indices;
      next := fail;
      pos := fail;

      for i in indices do
        if current[i] <> fail then
          if next <> fail then
            if current[i] < next then
              next := current[i];
              pos := i;
            fi;
          else
            next := current[i];
            pos := i;
          fi;
        fi;
      od;

      base := iter!.base;
      if IsDoneIterator(base[pos]) then
        current[pos] := fail;
      else
        current[pos] := NextIterator(base[pos]);
      fi;
      return next;
    end;

    record.IsDoneIterator := function(iter)
      return ForAll(iter!.current, x -> x = fail);
    end;

    record.ShallowCopy := function(iter)
      local base;
      base := arg(iter!.base, ShallowCopy);
      return rec(base := base,
                 indices := iter!.indices,
                 current := arg(base, NextIterator));
    end;

    iter := IteratorByFunctions(record);
    SetFilterObj(iter, IsIteratorSorted);
    return iter;
  fi;
  return fail;
end);

InstallGlobalFunction(IteratorOfArrangements,
function(n, m)
  local convert;

  if not IsPosInt(n) then
    ErrorNoReturn("Semigroups: IteratorOfArrangements: usage,\n",
                  "the first arg <n> must be a positive integer,");
  elif not (IsInt(m) and m >= 0) then
    ErrorNoReturn("Semigroups: IteratorOfArrangements: usage,\n",
                  "the second arg <m> must be a non-negative integer,");
  elif m > n then
    ErrorNoReturn("Semigroups: IteratorOfArrangements: usage,\n",
                  "the second arg <m> must be no greater than the first arg ",
                  "<n>,");
  fi;

  convert := function(iter, x)
    return ArrangementNumber(x, m, n);
  end;

  return IteratorByIterator(IteratorList([1 .. NrArrangements([1 .. n], m)]),
                            convert);
end);

# technical...

# returns func(iter, pos) once at least position <pos> of the orbit <o> is
# known, the starting value for <pos> is <start>. The idea is that this returns
# something that depends on position <pos> of <o> being known but does not
# depend on the entire orbit being known.

InstallGlobalFunction(IteratorByOrbFunc,
function(o, func, start)
  local func2, record;

  if not IsOrbit(o) then
    ErrorNoReturn("Semigroups: IteratorByOrbFunc: usage,\n",
                  "the first arg <o> must be an orbit,");
  elif not IsFunction(func) then
    ErrorNoReturn("Semigroups: IteratorByOrbFunc: usage,\n",
                  "the second arg <func> must be a function,");
  fi;

  # change 1 arg <func> to 2 arg
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
    ErrorNoReturn("Semigroups: IteratorByNextIterator: usage,\n",
                  "the arg <record> must be a record with components ",
                  "`NextIterator'\nand `ShallowCopy',");
  elif IsRecord(record) and (IsBound(record.last_called_by_is_done)
                             or IsBound(record.next_value)
                             or IsBound(record.IsDoneIterator)) then
    ErrorNoReturn("Semigroups: IteratorByNextIterator: usage,\n",
                  "the arg <record> must be a record with no components named",
                  "\n`last_called_by_is_done', `next_value', or ",
                  "`IsDoneIterator',");
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
# then iter is replaced by Iterator(convert(NextIterator(baseiter))) until
# IsDoneIterator(baseiter), where <convert> is a function.

InstallGlobalFunction(IteratorByIterOfIters,
function(record, baseiter, convert, filts)
  local iter, filt;

  if not IsRecord(record) or IsBound(record.baseiter)
      or IsBound(record.iterofiters) or IsBound(record.IsDoneIterator)
      or IsBound(record.NextIterator) or IsBound(record.ShallowCopy) then
    ErrorNoReturn("Semigroups: IteratorByIterOfIters: usage,\n",
                  "the first arg <record> must be a record with no components",
                  "named:\n",
                  "`baseiter', `iterofiters', `IsDoneIterator', ",
                  "`NextIterator', or\n`ShallowCopy'");
  elif not IsIterator(baseiter) then
    ErrorNoReturn("Semigroups: IteratorByIterOfIters: usage,\n",
                  "the second arg <baseiter> must be an iterator,");
  elif not IsFunction(convert) then
    ErrorNoReturn("Semigroups: IteratorByIterOfIters: usage,\n",
                  "the third arg <convert> must be a function,");
  elif not (IsList(filts) and ForAll(filts, IsFilter)) then
    ErrorNoReturn("Semigroups: IteratorByIterOfIters: usage,\n",
                  "the fourth arg <filts> must be a list of filters,");
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

# iterator [, length of iterator]

InstallGlobalFunction(ListIterator,
function(arg)
  local out, i, x;

  if IsBound(arg[2]) then
    if not IsPosInt(arg[2]) then
      ErrorNoReturn("Semigroups: ListIterator: usage,\n",
                    "the second argument must be a positive integer,");
    fi;
    out := EmptyPlist(arg[2]);
  else
    out := [];
  fi;

  i := 0;

  for x in arg[1] do
    i := i + 1;
    out[i] := x;
  od;

  return out;
end);

# for general acting semigroups...

# Notes: the previous inverse method used D-classes instead of R-classes.

# same method for regular/inverse
# FIXME move this!

InstallMethod(Iterator, "for an acting semigroup",
[IsActingSemigroup], 5,  # to beat the method for semigroup ideals
function(S)
  local iter;

  if HasAsSSortedList(S) then
    iter := IteratorList(AsSSortedList(S));
  else
    iter := IteratorByIterOfIters(rec(parent := S), IteratorOfRClasses(S),
                                  IdFunc, []);
  fi;
  SetParent(iter, S);
  return iter;
end);

# different method for regular/inverse
# FIXME move this!

InstallMethod(Iterator, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local iter;

  if HasAsSSortedList(d) then
    iter := IteratorList(AsSSortedList(d));
    return iter;
  fi;

  return IteratorByIterOfIters(rec(parent := Parent(d)),
                               Iterator(GreensRClasses(d)), IdFunc,
                               []);
end);

# different method for inverse
# FIXME move this!

InstallMethod(Iterator, "for a regular D-class of an acting semigroup",
[IsRegularDClass and IsActingSemigroupGreensClass],
function(d)
  local iter, baseiter, convert;

  if HasAsSSortedList(d) then
    iter := IteratorList(AsSSortedList(d));
    return iter;
  fi;

  baseiter := IteratorOfCartesianProduct(OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)],
                SchutzenbergerGroup(d),
                OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)]);

  convert := function(x)

    return StabilizerAction(Parent(d))(RhoOrbMult(RhoOrb(d),
                                                  RhoOrbSCCIndex(d),
                                                  x[1])[1]
                                       * Representative(d), x[2])
                                       * LambdaOrbMult(LambdaOrb(d),
                                                       LambdaOrbSCCIndex(d),
                                                       x[3])[1];
  end;

  return IteratorByIterator(baseiter,
                            convert,
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of D-class>");
                                  return;
                                end));
end);

# JDM again this method is redundant if we introduce RhoOrb for inverse
# semigroups
# FIXME move this!

InstallMethod(Iterator,
"for a D-class of an inverse acting semigroup rep",
[IsGreensDClass and IsInverseActingRepGreensClass
 and IsActingSemigroupGreensClass],
function(d)
  local iter, scc, baseiter, convert;

  if HasAsSSortedList(d) then
    iter := IteratorList(AsSSortedList(d));
    return iter;
  fi;

  scc := OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
  baseiter := IteratorOfCartesianProduct(scc, SchutzenbergerGroup(d), scc);

  convert := function(x)
    return StabilizerAction(Parent(d))(LambdaOrbMult(LambdaOrb(d),
                                                     LambdaOrbSCCIndex(d),
                                                     x[1])[2]
                                       * Representative(d), x[2])
                                       * LambdaOrbMult(LambdaOrb(d),
                                                       LambdaOrbSCCIndex(d),
                                                       x[3])[1];
  end;

  return IteratorByIterator(baseiter,
                            convert,
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of D-class>");
                                  return;
                                end));
end);

# same method for regular/inverse
# FIXME move this!

InstallMethod(Iterator, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local iter, s;

  if HasAsSSortedList(h) then
    iter := IteratorList(AsSSortedList(h));
    return iter;
  fi;

  s := Parent(h);
  return IteratorByIterator(Iterator(SchutzenbergerGroup(h)),
                            x -> StabilizerAction(s)(Representative(h), x),
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of H-class>");
                                  return;
                                end));
end);

# same method for regular, different method for inverse
# FIXME move this!

InstallMethod(Iterator, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local iter, baseiter, convert;

  if HasAsSSortedList(l) then
    iter := IteratorList(AsSSortedList(l));
    return iter;
  fi;

  baseiter := IteratorOfCartesianProduct(OrbSCC(RhoOrb(l))[RhoOrbSCCIndex(l)],
                                         Enumerator(SchutzenbergerGroup(l)));

  convert := function(x)
    return StabilizerAction(Parent(l))(RhoOrbMult(RhoOrb(l),
                                                  RhoOrbSCCIndex(l),
                                                  x[1])[1]
                                       * Representative(l), x[2]);
  end;

  return IteratorByIterator(baseiter,
                            convert,
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of L-class>");
                                  return;
                                end));
end);

# Notes: this method makes Iterator of a semigroup much better!!

# same method for regular/inverse
# FIXME move this!

InstallMethod(Iterator, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local iter, baseiter, convert;

  if HasAsSSortedList(r) then
    iter := IteratorList(AsSSortedList(r));
    return iter;
  fi;
  baseiter := IteratorOfCartesianProduct(Enumerator(SchutzenbergerGroup(r)),
                OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)]);

  convert := function(x)
    return StabilizerAction(Parent(r))(Representative(r), x[1])
           * LambdaOrbMult(LambdaOrb(r), LambdaOrbSCCIndex(r), x[2])[1];
  end;

  return IteratorByIterator(baseiter,
                            convert,
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of R-class>");
                                  return;
                                end));
end);

# JDM could use IteratorOfRClasses here instead, not sure which is better...
# JDM could be different method for regular/inverse, see inverse_old.gi in
# semigroups-dev.
# FIXME move this!

InstallMethod(IteratorOfHClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;

  if HasGreensHClasses(s) then
    iter := IteratorList(GreensHClasses(s));
    return iter;
  fi;

  return IteratorByIterOfIters(rec(parent := s), IteratorOfDClasses(s),
                               GreensHClasses, []);
end);

# different method for regular/inverse

# FIXME move this!
InstallMethod(IteratorOfLClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter;

  if HasGreensLClasses(S) then
    iter := IteratorList(GreensLClasses(S));
    return iter;
  fi;

  return IteratorByIterOfIters(rec(parent := S), IteratorOfDClasses(S),
                               GreensLClasses, []);
end);

# FIXME move this!
# different method for regular/inverse

InstallMethod(IteratorOfRClassData, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return IteratorByNextIterator(rec(i := 1,

                                    NextIterator := function(iter)
                                      local data;

                                      iter!.i := iter!.i + 1;

                                      data := Enumerate(SemigroupData(s),
                                                        iter!.i, ReturnFalse);

                                      if iter!.i > Length(data!.orbit) then
                                        return fail;
                                      fi;
                                      return data[iter!.i];
                                    end,

                                    ShallowCopy := iter -> rec(i := 1)));
end);

# no method required for inverse/regular

# FIXME move this!
InstallMethod(Iterator, "for a full transformation semigroup",
[IsTransformationSemigroup and IsFullTransformationSemigroup and
 HasGeneratorsOfSemigroup],
7,  # to beat the method for acting semigroups TODO check this is necessary!!
function(S)
  local iter;

  if HasAsSSortedList(S) or HasAsListCanonical(S) then
    # This is much faster
    TryNextMethod();
  fi;

  iter := IteratorByFunctions(rec(
    tups := IteratorOfTuples([1 .. DegreeOfTransformationSemigroup(S)],
                             DegreeOfTransformationSemigroup(S)),
    parent := S,

    NextIterator := iter -> TransformationNC(NextIterator(iter!.tups)),

    IsDoneIterator := iter -> IsDoneIterator(iter!.tups),

    ShallowCopy := iter ->
      rec(parent := S,
          tups := IteratorOfTuples([1 .. DegreeOfTransformationSemigroup(S)],
                                   DegreeOfTransformationSemigroup(S))),

    PrintObj := function(iter)
                  Print("<iterator of semigroup>");
                  return;
                end));
  return iter;
end);

# FIXME move this!
InstallMethod(Iterator, "for a symmetric inverse semigroup",
[IsPartialPermSemigroup and IsSymmetricInverseSemigroup
 and HasGeneratorsOfSemigroup],
function(s)
  local deg, record, iter;

  if HasAsSSortedList(s) or HasAsListCanonical(s) then
    # This is much faster
    TryNextMethod();
  fi;

  deg := DegreeOfPartialPermSemigroup(s);

  record := rec(parent := s);

  record.iter_doms := IteratorOfCombinations([1 .. deg]);

  record.dom := fail;

  record.iter_imgs := fail;

  record.NextIterator := function(iter)
    local img;

    if iter!.iter_imgs = fail or IsDoneIterator(iter!.iter_imgs) then
      if IsDoneIterator(iter!.iter_doms) then
        return fail;
      fi;
      iter!.dom := NextIterator(iter!.iter_doms);
      iter!.iter_imgs := IteratorOfArrangements(deg, Length(iter!.dom));
    fi;

    img := NextIterator(iter!.iter_imgs);
    return PartialPermNC(iter!.dom, img);
  end;

  record.ShallowCopy := iter -> rec(parent := s,
                                    iter_doms := IteratorOfCombinations([1 ..
                                                                         deg]),
                                    dom := fail,
                                    iter_imgs := fail);

  record.PrintObj := function(iter)
                       Print("<iterator of semigroup>");
                       return;
                     end;

  iter := IteratorByNextIterator(record);
  return iter;
end);

# Notes: required until Enumerator for a trans. semigroup does not call
# iterator. This works but is maybe not the best!

# same method for regular/inverse
# FIXME move this!

InstallMethod(Iterator, "for a trivial acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsTrivial], 9999,
function(S)
  return TrivialIterator(GeneratorsOfSemigroup(S)[1]);
end);

# different method for regular/inverse
# FIXME move this!

InstallMethod(IteratorOfDClassReps, "for an acting semigroup",
[IsActingSemigroup],
S -> IteratorByIterator(IteratorOfDClasses(S), Representative,
                        [],
                        fail,
                        rec(PrintObj := function(iter)
                              Print("<iterator of D-class reps>");
                              return;
                            end)));

# JDM could be a different method for regular/inverse using
# IteratorOfHClassData (not yet written), see inverse_old.gi in semigroups-dev
# FIXME move this!

InstallMethod(IteratorOfHClassReps, "for an acting semigroup",
[IsActingSemigroup],
S -> IteratorByIterator(IteratorOfHClasses(S),
                        Representative,
                        [],
                        fail,
                        rec(PrintObj := function(iter)
                              Print("<iterator of H-class reps>");
                              return;
                            end)));

# different method for regular/inverse
# FIXME move this!

InstallMethod(IteratorOfLClassReps, "for an acting semigroup",
[IsActingSemigroup],
S -> IteratorByIterator(IteratorOfLClasses(S),
                        Representative,
                        [],
                        fail,
                        rec(PrintObj := function(iter)
                              Print("<iterator of L-class reps>");
                              return;
                            end)));

# same method for inverse/regular.
# FIXME move this!

InstallMethod(IteratorOfRClassReps, "for an acting semigroup",
[IsActingSemigroup],
S -> IteratorByIterator(IteratorOfRClassData(S),
                        x -> x[4],
                        [],
                        fail,
                        rec(PrintObj := function(iter)
                              Print("<iterator of R-class reps>");
                              return;
                            end)));

# for regular acting semigroups...
# FIXME move this!

# different method for inverse

InstallMethod(IteratorOfDClassData, "for regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local record, o, scc, func;

  if not IsClosedOrbit(LambdaOrb(s)) then
    record := rec(m := fail, graded := IteratorOfGradedLambdaOrbs(s));
    record.NextIterator := function(iter)
      local l, rep, m;

      m := iter!.m;

      if IsBound(iter!.o) and iter!.o = fail then
        return fail;
      fi;

      if m = fail or m = Length(OrbSCC(iter!.o)) then
        m := 1;
        l := 1;
        iter!.o := NextIterator(iter!.graded);
        if iter!.o = fail then
          return fail;
        fi;
      else
        m := m + 1;
        l := OrbSCC(iter!.o)[m][1];
      fi;
      iter!.m := m;

      # rep has rectified lambda val and rho val.
      rep := LambdaOrbRep(iter!.o, m) * LambdaOrbMult(iter!.o, m, l)[2];
      return [s, m, iter!.o, 1, GradedRhoOrb(s, rep, false)[1], rep, false];
    end;

    record.ShallowCopy := iter -> rec(m := fail,
                                      graded := IteratorOfGradedLambdaOrbs(s));
    return IteratorByNextIterator(record);
  else
    o := LambdaOrb(s);
    scc := OrbSCC(o);

    func := function(iter, m)
      local rep;
      # rep has rectified lambda val and rho val.
      rep := EvaluateWord(o, TraceSchreierTreeForward(o, scc[m][1]));
      return [s, m, o, 1, GradedRhoOrb(s, rep, false)[1], rep, false];
    end;

    return IteratorByIterator(IteratorList([2 .. Length(scc)]), func);
  fi;
end);

# no method required for inverse (it's not used for anything)
# FIXME move this!

InstallMethod(IteratorOfLClassData, "for regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local o, func, iter;

  o := LambdaOrb(s);

  func := function(iter, i)
    local rep;

    # <rep> has lambda val corresponding to <i>
    rep := EvaluateWord(o, TraceSchreierTreeForward(o, i));

    # <rep> has rho val in position 1 of GradedRhoOrb(s, rep, false).
    # We don't rectify the rho val of <rep> in <o> since we require to
    # enumerate RhoOrb(s) to do this, if we use GradedRhoOrb(s, rep,
    # true) then this get more complicated.
    return [s, 1, GradedRhoOrb(s, rep, false)[1], rep, true];
  end;

  if not IsClosedOrbit(o) then
    iter := IteratorByOrbFunc(o, func, 2);
  else
    return IteratorByIterator(IteratorList([2 .. Length(o)]), func);
  fi;

  return iter;
end);

# different method for inverse
# FIXME move this!

InstallMethod(IteratorOfRClassData, "for regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local o, func, iter;

  o := RhoOrb(s);

  func := function(iter, i)
    local rep;

    # <rep> has rho val corresponding to <i>
    rep := EvaluateWord(o, Reversed(TraceSchreierTreeForward(o, i)));

    # <rep> has lambda val in position 1 of GradedLambdaOrb(s, rep, false).
    # We don't rectify the lambda val of <rep> in <o> since we require to
    # enumerate LambdaOrb(s) to do this, if we use GradedLambdaOrb(s, rep,
    # true) then this gets more complicated.
    return [s, 1, GradedLambdaOrb(s, rep, false), rep, true];
  end;

  if not IsClosedOrbit(o) then
    # JDM should we use IteratorOfGradedRhoOrbs here instead??
    iter := IteratorByOrbFunc(o, func, 2);
  else
    return IteratorByIterator(IteratorList([2 .. Length(o)]), func);
  fi;

  return iter;
end);

# same method for inverse
# FIXME move this!

InstallMethod(IteratorOfDClassReps, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  if HasDClassReps(s) then
    return IteratorList(DClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfDClassData(s),
                            x -> x[6],
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of D-class reps>");
                                  return;
                                end));
end);

# different method for inverse
# FIXME move this!

InstallMethod(IteratorOfLClassReps, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
S -> IteratorByIterator(IteratorOfLClassData(S),
                        x -> x[4],
                        [],
                        fail,
                        rec(PrintObj := function(iter)
                              Print("<iterator of L-class reps>");
                              return;
                            end)));

# for inverse acting semigroups...
# FIXME move this!

InstallMethod(IteratorOfDClassData, "for inverse acting semigroup",
[IsInverseActingSemigroupRep and IsRegularSemigroup],
function(s)
  local graded, record, o, scc, func;

  if not IsClosedOrbit(LambdaOrb(s)) then
    graded := IteratorOfGradedLambdaOrbs(s);
    record := rec(m := 0, graded := graded, o := NextIterator(graded));
    record.NextIterator := function(iter)
      local m, o;
      m := iter!.m;
      if iter!.o = fail then
        return fail;
      elif m = fail or m = Length(OrbSCC(iter!.o)) then
        m := 1;
        iter!.o := NextIterator(iter!.graded);
        if iter!.o = fail then
          return fail;
        fi;
      else
        m := m + 1;
      fi;
      iter!.m := m;
      o := iter!.o;

      # rep has rectified lambda val and rho val.
      # don't use trace schreier tree forward since often l=1 and so
      # this returns the identity partial perm
      return [s, m, o, fail, fail, RightOne(LambdaOrbRep(o, m)), false];
    end;

    record.ShallowCopy := iter -> rec(m := fail,
                                      graded := IteratorOfGradedLambdaOrbs(s));
    return IteratorByNextIterator(record);
  else
    o := LambdaOrb(s);
    scc := OrbSCC(o);

    func := function(iter, m)
      local rep;
      # rep has rectified lambda val and rho val.
      rep := RightOne(EvaluateWord(o, TraceSchreierTreeForward(o, scc[m][1])));

      return [s, m, o, fail, fail, rep, false];
    end;

    return IteratorByIterator(IteratorList([2 .. Length(scc)]), func);
  fi;
end);

# FIXME move this!

InstallMethod(IteratorOfRClassData, "for acting inverse semigroup rep",
[IsInverseActingSemigroupRep],
function(s)
  local o, func, iter, lookup;

  o := LambdaOrb(s);
  if not IsClosedOrbit(o) then
    func := function(iter, i)
      local rep;
      rep := Inverse(EvaluateWord(o, TraceSchreierTreeForward(o, i)));
      # <rep> has rho val corresponding to <i> and lambda val in position 1 of
      # GradedLambdaOrb(s, rep, false), if we use <true> as the last arg, then
      # this is no longer the case, and this is would be more complicated.

      return [s, 1, GradedLambdaOrb(s, rep, false), rep, true];
    end;
    iter := IteratorByOrbFunc(o, func, 2);
  else
    lookup := OrbSCCLookup(o);

    func := function(iter, i)
      local rep;

      # <rep> has rho val corresponding to <i>
      rep := Inverse(EvaluateWord(o, TraceSchreierTreeForward(o, i)));

      # rectify the lambda value of <rep>
      rep := rep * LambdaOrbMult(o,
                                 lookup[i],
                                 Position(o, LambdaFunc(s)(rep)))[2];

      return [s, lookup[i], o, rep, false];
    end;

    iter := IteratorByIterator(IteratorList([2 .. Length(o)]), func);
  fi;

  return iter;
end);

# FIXME move this!

InstallMethod(IteratorOfLClassReps, "for acting inverse semigroup rep",
[IsInverseActingSemigroupRep],
S -> IteratorByIterator(IteratorOfRClassData(S),
                        x -> Inverse(x[4]),
                        [],
                        fail,
                        rec(PrintObj := function(iter)
                              Print("<iterator of L-class reps>");
                              return;
                            end)));

# FIXME move this!

InstallMethod(Iterator, "for an L-class of an inverse acting semigroup",
[IsInverseActingRepGreensClass and IsGreensLClass
 and IsActingSemigroupGreensClass],
function(L)
  local iter, m, baseiter, convert;

  if HasAsSSortedList(L) then
    iter := IteratorList(AsSSortedList(L));
    return iter;
  fi;
  m := LambdaOrbSCCIndex(L);
  baseiter := IteratorOfCartesianProduct(OrbSCC(LambdaOrb(L))[m],
                                         Enumerator(SchutzenbergerGroup(L)));

  convert := function(x)
    return StabilizerAction(Parent(L))(LambdaOrbMult(LambdaOrb(L),
                                                     LambdaOrbSCCIndex(L),
                                                     x[1])[2]
                                       * Representative(L), x[2]);
  end;

  return IteratorByIterator(baseiter,
                            convert,
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of L-class>");
                                  return;
                                end));
end);
