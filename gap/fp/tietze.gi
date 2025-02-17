############################################################################
##
##  fp/tietze.gi
##  Copyright (C) 2021-2022                               Tom Conti-Leslie
##                                                              Ben Spiers
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##

########################################################################
# This file is organised as follows:
#
# 1.  Definition of the Semigroup Tietze (IsStzPresentation) object
# 2.  Viewing methods for IsStzPresentation objects
# 3.  Internal Tietze transformation functions
# 4.  User-accessible Tietze transformation functions (wrappers)
# 5.  Internal helper functions for word/relation manipulation etc.
# 6.  Internal auto-checkers and appliers for presentation simplifying
# 7.  SimplifyPresentation etc.
# 8.  Other
########################################################################

########################################################################
# 1. Definition of the Semigroup Tietze (IsStzPresentation) object
########################################################################

InstallMethod(StzPresentation, "for a finitely presented semigroup",
[IsFpSemigroup],
function(S)
  local gens, out, rels, type;

  type := NewType(NewFamily("StzFamily", IsStzPresentation),
                  IsStzPresentation and IsComponentObjectRep);

  rels := List(RelationsOfFpSemigroup(S),
              x -> [LetterRepAssocWord(x[1]), LetterRepAssocWord(x[2])]);
  gens := List(GeneratorsOfSemigroup(S), ViewString);

  out := rec(GeneratorsOfStzPresentation := gens,
             RelationsOfStzPresentation  := rels,
             UnreducedFpSemigroup        := S,
             TietzeForwardMap            := List(
               [1 .. Length(GeneratorsOfSemigroup(S))], x -> [x]),
             TietzeBackwardMap           := List(
               [1 .. Length(GeneratorsOfSemigroup(S))], x -> [x]),
             usedGens                    := Set(gens));

  return Objectify(type, out);
end);

InstallMethod(SetRelationsOfStzPresentation,
[IsStzPresentation, IsList],
function(stz, arg)
  if not ForAll(arg, IsList) or
      not ForAll(arg, x -> Length(x) = 2 and ForAll(x, IsList)) or
      not ForAll(arg, x -> ForAll(x, y -> ForAll(y, IsPosInt))) then
        ErrorNoReturn("parameter <arg> must be a list of pairs of words in ",
                      "LetterRep format");
  fi;
  stz!.RelationsOfStzPresentation := arg;
end);

InstallMethod(RelationsOfStzPresentation, [IsStzPresentation],
stz -> stz!.RelationsOfStzPresentation);

InstallMethod(UnreducedFpSemigroup, [IsStzPresentation],
stz -> stz!.UnreducedFpSemigroup);

InstallMethod(TietzeForwardMap, [IsStzPresentation],
stz -> stz!.TietzeForwardMap);

InstallMethod(TietzeBackwardMap, [IsStzPresentation],
stz -> stz!.TietzeBackwardMap);

InstallMethod(GeneratorsOfStzPresentation, [IsStzPresentation],
stz -> stz!.GeneratorsOfStzPresentation);

InstallMethod(SetGeneratorsOfStzPresentation, [IsStzPresentation, IsList],
function(stz, newGens)
  stz!.GeneratorsOfStzPresentation := newGens;
end);

InstallMethod(StzIsomorphism,
[IsStzPresentation],
function(stz)
  local source, range, forward_dict, forward_map, backward_dict, backward_map;
  source := UnreducedFpSemigroup(stz);
  range  := SEMIGROUPS.StzConvertObjToFpSemigroup(stz);

  # build forward map
  forward_dict := TietzeForwardMap(stz);
  forward_map  := function(word)
    local new_word;
    new_word := SEMIGROUPS.StzExpandWord(
                LetterRepAssocWord(UnderlyingElement(word)), forward_dict);
    return Product(new_word, x -> GeneratorsOfSemigroup(range)[x]);
  end;

  # build backward map
  backward_dict := TietzeBackwardMap(stz);
  backward_map  := function(word)
    local new_word;
    new_word := SEMIGROUPS.StzExpandWord(
                LetterRepAssocWord(UnderlyingElement(word)), backward_dict);
    return Product(new_word, x -> GeneratorsOfSemigroup(source)[x]);
  end;

  # TODO(later) are we okay to assume this is necessarily an isomorphism?
  return SemigroupIsomorphismByFunctionNC(source,
                                          range,
                                          forward_map,
                                          backward_map);
end);

InstallMethod(SetTietzeForwardMap,
"for an stz presentation and list",
[IsStzPresentation, IsList],
function(stz, newMaps)
    if not ForAll(newMaps, x -> IsList(x) and ForAll(x, IsPosInt)) then
      ErrorNoReturn("the 2nd argument <newMaps> must ",
                    "be a list of lists of positive integers");
    fi;
    stz!.TietzeForwardMap := newMaps;
end);

InstallMethod(SetTietzeBackwardMap,
"for an stz presentation and list",
[IsStzPresentation, IsList],
function(stz, newMaps)
    if not ForAll(newMaps, x -> IsList(x) and ForAll(x, IsPosInt)) then
      ErrorNoReturn("the 2nd argument <newMaps> must ",
                    "be a list of lists of positive integers");
    fi;
    stz!.TietzeBackwardMap := newMaps;
end);

InstallMethod(TietzeForwardMapReplaceSubword,
"for an stz presentation, list, and list",
[IsStzPresentation, IsList, IsList],
function(stz, subWord, newSubWord)
    local newMaps;
    newMaps := List(stz!.TietzeForwardMap,
                    x -> SEMIGROUPS.StzReplaceSubwordRel(x,
                                                         subWord,
                                                         newSubWord));
    stz!.TietzeForwardMap := newMaps;
end);

# Length of an StzPresentation is defined as the number of generators plus the
# length of every word in the defining relations
InstallMethod(Length, "for an stz presentation", [IsStzPresentation],
function(stz)
  local out, rel;
  out := Length(stz!.GeneratorsOfStzPresentation);
  for rel in RelationsOfStzPresentation(stz) do
    out := out + Length(rel[1]) + Length(rel[2]);
  od;
  return out;
end);

InstallMethod(\<, "for stz presentations",
[IsStzPresentation, IsStzPresentation],
{stz1, stz2} -> Length(stz1) < Length(stz2));

########################################################################
# 2. Viewing methods for IsStzPresentation objects
########################################################################

InstallMethod(ViewString,
[IsStzPresentation],
function(stz)
  local num_gens, num_rels;
  num_gens := Length(stz!.GeneratorsOfStzPresentation);
  num_rels := Length(stz!.RelationsOfStzPresentation);
  return Concatenation(
    StringFormatted("<fp semigroup presentation with {} and {}",
                    Pluralize(num_gens, "generator"),
                    Pluralize(num_rels, "relation")),
    StringFormatted("\< with length {}>", Length(stz)));
end);

SEMIGROUPS.StzRelationDisplayString := function(stz, i)
  local rels, f, gens, w1, w2;
  rels := RelationsOfStzPresentation(stz);
  if i > Length(rels) then
    return fail;
  else
    # We'd like patterns to be grouped, i.e. abab=(ab)^2 when displayed. To
    # do this we sneakily piggyback off display methods for the free semigroup.
    f    := FreeSemigroup(GeneratorsOfStzPresentation(stz));
    gens := GeneratorsOfSemigroup(f);
    w1   := Product(rels[i][1], x -> gens[x]);
    w2   := Product(rels[i][2], x -> gens[x]);
    return Concatenation(PrintString(i),
                         ". ",
                         PrintString(w1),
                         " = ",
                         PrintString(w2));
  fi;
end;

InstallMethod(StzPrintRelations,
"for an stz presentation and a list of pos ints",
[IsStzPresentation, IsList],
function(stz, list)
  local i;
  # This function displays the current relations in terms of the current
  # generators for a semigroup Tietze presentation.
  if IsEmpty(RelationsOfStzPresentation(stz)) then
    Info(InfoFpSemigroup,
         1,
         "There are no relations in the presentation <stz>");
  fi;

  # Print relations at each index of the list, unless incorrect index,
  # in which case skip.
  for i in list do
    if IsPosInt(i) and i <= Length(RelationsOfStzPresentation(stz)) then
      Info(InfoFpSemigroup, 1, SEMIGROUPS.StzRelationDisplayString(stz, i));
    fi;
  od;
end);

InstallMethod(StzPrintRelations, "for an stz presentation",
[IsStzPresentation],
function(stz)
  StzPrintRelations(stz, [1 .. Length(RelationsOfStzPresentation(stz))]);
end);

InstallMethod(StzPrintRelation, "for an stz presentation and a pos int",
[IsStzPresentation, IsPosInt],
function(stz, int)
  StzPrintRelations(stz, [int]);
end);

InstallMethod(StzPrintGenerators,
"for an stz presentation and a list of pos ints",
[IsStzPresentation, IsList],
function(stz, list)
  local flat, gens, out, rel, i;
  # This function displays a list of generators and number of occurrences
  # of each

  # warn if there are no generators in the list (not sure this could happen)
  if IsEmpty(GeneratorsOfStzPresentation(stz)) then
    Info(InfoFpSemigroup, 1,
         "There are no generators in the presentation <stz>");
  fi;

  # create flat flat list of relations to count occurrences
  flat := [];
  for rel in RelationsOfStzPresentation(stz) do
    Append(flat, rel[1]);
    Append(flat, rel[2]);
  od;

  # enumerate and count generators
  gens := GeneratorsOfStzPresentation(stz);
  for i in list do
    # only print if requested index is valid
    if IsPosInt(i) and i <= Length(gens) then
      out := Concatenation(PrintString(i),
                           ".  ",
                           gens[i],
                           "  ",
                           PrintString(Number(flat, x -> x = i)),
                           " occurrences");
      Info(InfoFpSemigroup, 1, out);
    fi;
  od;
end);

InstallMethod(StzPrintGenerators, "for an stz presentation",
[IsStzPresentation],
function(stz)
  StzPrintGenerators(stz, [1 .. Length(GeneratorsOfStzPresentation(stz))]);
end);

InstallMethod(StzPrintPresentation, "for an stz presentation",
[IsStzPresentation],
function(stz)
  local status, oldgens, oldfree, oldelms, newgens, newfree, newelms, w, i;
  # prints everything that is known about the presentation.

  # current generators
  Info(InfoFpSemigroup, 1, "Current generators:");
  StzPrintGenerators(stz);

  # current relations
  Info(InfoFpSemigroup, 1, "");
  Info(InfoFpSemigroup, 1, "Current relations:");
  StzPrintRelations(stz);

  # total number and total length
  Info(InfoFpSemigroup, 1, "");
  status := "There ";
  if Length(stz!.GeneratorsOfStzPresentation) = 1 then
    status := Concatenation(status, "is 1 generator");
  else
    status := Concatenation(status,
                            "are ",
                            PrintString(
                            Length(GeneratorsOfStzPresentation(stz))),
                            " generators");
  fi;
  status := Concatenation(status, " and ");
  if Length(stz!.RelationsOfStzPresentation) = 1 then
    status := Concatenation(status, "1 relation");
  else
    status := Concatenation(status,
               PrintString(Length(stz!.RelationsOfStzPresentation)),
               " relations");
  fi;
  status := Concatenation(status,
                          " of total length ",
                          PrintString(Length(stz)),
                          ".");
  Info(InfoFpSemigroup, 1, status);

  # build free semigroup of old gens for display
  oldgens := GeneratorsOfSemigroup(UnreducedFpSemigroup(stz));
  oldgens := List(oldgens, ViewString);
  oldfree := FreeSemigroup(oldgens);
  oldelms := GeneratorsOfSemigroup(oldfree);

  # build free semigroup of new gens for display
  newgens := GeneratorsOfStzPresentation(stz);
  newfree := FreeSemigroup(newgens);
  newelms := GeneratorsOfSemigroup(newfree);

  # old generators expressed using current ones
  Info(InfoFpSemigroup, 1, "");
  Info(InfoFpSemigroup, 1, "Generators of original fp semigroup expressed as");
  Info(InfoFpSemigroup, 1,
       "combinations of generators in current presentation:");
  for i in [1 .. Length(oldgens)] do
    w := Product(TietzeForwardMap(stz)[i], x -> newelms[x]);
    Info(InfoFpSemigroup, 1, Concatenation(PrintString(i),
                                       ". ",
                                       oldgens[i],
                                       " = ",
                                       PrintString(w)));
  od;

  # new generators expressed using old ones
  Info(InfoFpSemigroup, 1, "");
  Info(InfoFpSemigroup, 1, "Generators of current presentation expressed as");
  Info(InfoFpSemigroup, 1,
       "combinations of generators of original fp semigroup:");
  for i in [1 .. Length(newgens)] do
    w := Product(TietzeBackwardMap(stz)[i], x -> oldelms[x]);
    Info(InfoFpSemigroup, 1, Concatenation(PrintString(i),
                                       ". ",
                                       newgens[i],
                                       " = ",
                                       PrintString(w)));
  od;
end);

########################################################################
# 3. Internal Tietze transformation functions
########################################################################

# TIETZE TRANSFORMATION 1: INTERNAL: ADD REDUNDANT RELATION - NO CHECK
SEMIGROUPS.TietzeTransformation1 := function(stz, pair)
  local rels_copy;
  # Arguments:
  # - <stz> should be a Semigroup Tietze (IsStzPresentation) object.
  # - <pair> should be a list containing two LetterRep words.
  #
  # This function returns nothing, and modifies <stz> in place by adding a new
  # relation given by <pair>. This relation is assumed to be redundant based
  # on the relations already present in RelationsOfStzPresentation(<stz>)
  # (although this is not checked).
  #
  # WARNING: this is an internal function and performs only minimal argument
  # checks. Entering arguments in the wrong format may result in errors that
  # are difficult to interpret. Argument checks are carried out in the
  # analogous, documented function: StzAddRelation. However, these checks
  # may not terminate in some cases.

  # Add relation
  rels_copy := ShallowCopy(RelationsOfStzPresentation(stz));
  Add(rels_copy, pair);
  stz!.RelationsOfStzPresentation := rels_copy;
  return;
end;

# TIETZE TRANSFORMATION 2: INTERNAL: REMOVE REDUNDANT RELATION - NO CHECK
SEMIGROUPS.TietzeTransformation2 := function(stz, index)
  local rels_copy;
  # Arguments:
  # - <stz> should be a Semigroup Tietze (IsStzPresentation) object.
  # - <index> should be the index of the relation in
  #   RelationsOfStzPresentation(<stz>) that needs to be removed.
  #
  # This function returns nothing, and modifies <stz> in place by removing
  # relation number <index>, assumed to be redundant (though redundancy of
  # that relation is not checked).
  #
  # WARNING: this is an internal function and performs only minimal argument
  # checks. Entering arguments in the wrong format may result in errors that
  # are difficult to interpret. Argument checks are carried out in the
  # analogous, documented function: StzRemoveRelation. However, these checks
  # may not terminate in some cases.

  # Remove relation
  rels_copy := ShallowCopy(RelationsOfStzPresentation(stz));
  Remove(rels_copy, index);
  stz!.RelationsOfStzPresentation := rels_copy;
end;

# TIETZE TRANSFORMATION 3: INTERNAL: ADD NEW GENERATOR - NO CHECK
SEMIGROUPS.TietzeTransformation3 := function(stz, word, name)
  local new_gens, new_rels, back_word, new_maps, letter;
  # Arguments:
  # - <stz> should be a Semigroup Tietze (IsStzPresentation) object.
  # - <word> should be a LetterRep word.
  # - <name> should be a string, or fail.
  #
  # This function returns nothing, and modifies <stz> in place by adding a new
  # generator and the relation gen = <word>.
  # The name for the new generator is <name>, unless this argument is fail,
  # in which case the name is automatically generated using
  # SEMIGROUPS.NewGeneratorName.
  # This function also updates the Tietze backwards map so that the new
  # generator can be expressed as a word on the generators of the original
  # semigroup.
  #
  # WARNING: this is an internal function and performs only minimal argument
  # checks. Entering arguments in the wrong format may result in errors that
  # are difficult to interpret. Argument checks are carried out in the
  # analogous, documented function: StzAddGenerator.

  # Add new generator string to the list of gens in similar format
  new_gens := ShallowCopy(stz!.GeneratorsOfStzPresentation);
  new_rels := ShallowCopy(stz!.RelationsOfStzPresentation);
  if name = fail or name in stz!.GeneratorsOfStzPresentation then
    # either name was not specified, or it was but is already a generator.
    # generate a new generator name, as of yet unused.
    Add(new_gens, SEMIGROUPS.NewGeneratorName(List(stz!.usedGens)));
  else
    # this must mean the argument is a string as of yet unused, so add that
    Add(new_gens, ShallowCopy(name));
  fi;

  # in either case we have added a new generator: for cosmetic reasons,
  # prohibit that generator name from being auto-used again (in case we delete
  # it)
  UniteSet(stz!.usedGens, new_gens);

  # Add relation setting new gen equal to <word>
  Add(new_rels, [word, [Length(stz!.GeneratorsOfStzPresentation) + 1]]);

  # Update internal representation of the stz object
  SetGeneratorsOfStzPresentation(stz, new_gens);
  SetRelationsOfStzPresentation(stz, new_rels);

  # Now we need to update the backwards map to express the new generator in
  # terms of the original generators.
  back_word := [];
  new_maps  := ShallowCopy(TietzeBackwardMap(stz));
  for letter in word do
    Append(back_word, new_maps[letter]);
  od;
  Add(new_maps, back_word);
  SetTietzeBackwardMap(stz, new_maps);
end;

# TIETZE TRANSFORMATION 4: INTERNAL: REMOVE GENERATOR - MINIMAL CHECKS
SEMIGROUPS.TietzeTransformation4 := function(stz, gen, index)
  local found_expr, expr, decrement, tempMaps, tempRels, tempGens;
  # Arguments:
  # - <stz> should be a Semigroup Tietze (IsStzPresentation) object.
  # - <gen> should be a pos int.
  # - <index> should be a pos int.
  #
  # This function returns nothing, and modifies <stz> in place by removing
  # generator number <gen> using relation number <index>.
  # If relation number <index> of <stz> is not of the form
  # [<gen>] = *some word not including <gen>* (or that relation flipped),
  # then this function throws ErrorNoReturn.
  # This function also updates the Tietze forward map, so that any generator
  # in the original semigroup expressed as a combination of generators
  # including <gen> is now represented without using <gen> (since <gen>
  # has been removed).
  #
  # WARNING: this is an internal function and performs only minimal argument
  # checks. Entering arguments in the wrong format may result in errors that
  # are difficult to interpret. Argument checks are carried out in the
  # analogous, documented function: StzRemoveGenerator.

  # check we can express <gen> using only other generators with relation
  # number <index>
  found_expr := false;
  if RelationsOfStzPresentation(stz)[index][1] = [gen] and
      not gen in RelationsOfStzPresentation(stz)[index][2] then
    expr := RelationsOfStzPresentation(stz)[index][2];
    found_expr := true;
  elif RelationsOfStzPresentation(stz)[index][2] = [gen] and
      not gen in RelationsOfStzPresentation(stz)[index][1] then
    expr := RelationsOfStzPresentation(stz)[index][1];
    found_expr := true;
  fi;

  # check we found an expression for <gen>. If not, nothing can be done.
  if not found_expr then
    ErrorNoReturn("TietzeTransformation4, internal function: third argument\n",
                  "<index> does not point to a relation expressing second\n",
                  "argument <gen> as a combination of other generators");
  fi;

  # Define decrement function to bump down generator numbers past the one
  # we're going to remove
  decrement := function(z)
    if z <= gen then  # shouldn't be equal but just in case
      return z;
    else
      return z - 1;
    fi;
  end;

  # update forward mapping component
  # TODO(later) do we really need TietzeForwardMapReplaceSubword?
  TietzeForwardMapReplaceSubword(stz, [gen], expr);
  tempMaps := ShallowCopy(TietzeForwardMap(stz));
  Apply(tempMaps, x -> List(x, decrement));
  SetTietzeForwardMap(stz, tempMaps);

  # remove generator from backward mapping component
  tempMaps := ShallowCopy(TietzeBackwardMap(stz));
  Remove(tempMaps, gen);
  SetTietzeBackwardMap(stz, tempMaps);

  # sub in expression we found and remove relation we used for gen
  tempRels := ShallowCopy(RelationsOfStzPresentation(stz));
  Remove(tempRels, index);
  tempRels := SEMIGROUPS.StzReplaceSubword(tempRels, [gen], expr);
  SetRelationsOfStzPresentation(stz, tempRels);
  Apply(stz!.RelationsOfStzPresentation, x -> List(x, y -> List(y, decrement)));

  # remove generator.
  tempGens := ShallowCopy(GeneratorsOfStzPresentation(stz));
  Remove(tempGens, gen);
  SetGeneratorsOfStzPresentation(stz, tempGens);
end;

########################################################################
# 4. User-accessible Tietze transformation functions (wrappers)
########################################################################

# Tietze Transformation 1: Add relation
InstallMethod(StzAddRelation, "for an stz presentation and a pair of words",
[IsStzPresentation, IsList],
1,  # bump ahead of next implementation and do the list size arg check here
function(stz, pair)
  local n, f, free_fam, r, s, fp_fam, w1, w2, p1, p2, word, letter;
  # <pair> should be a pair of LetterRep words.

  # argument checks:
  if Length(pair) <> 2 then
    ErrorNoReturn("StzAddRelation: second argument <pair> should be a list\n",
                  "of length 2");
  fi;
  n := Length(stz!.GeneratorsOfStzPresentation);
  for word in pair do
    if not IsList(word) then
      TryNextMethod();  # pass this on to the case where the list may be a pair
                        # of words in OG semigroup
    elif IsEmpty(word) then
      ErrorNoReturn("StzAddRelation: words in second argument <pair> should\n",
                    "be non-empty");
    else
      for letter in word do
        if not (IsPosInt(letter) and letter <= n) then
          ErrorNoReturn("StzAddRelation: words in second argument <pair>\n",
                        "should be lists of pos ints no greater than the\n",
                        "number of generators of first argument <stz>");
        fi;
      od;
    fi;
  od;

  # check that the pair can be deduced from the other relations, by
  # creating fp semigroup with current relations.

  # base free semigroup
  f        := FreeSemigroup(stz!.GeneratorsOfStzPresentation);
  free_fam := FamilyObj(f.1);            # marrow for creating free semigp words
  r        := List(stz!.RelationsOfStzPresentation,
                   x -> List(x, y -> AssocWordByLetterRep(free_fam, y)));
  s        := f / r;                    # fp semigroup
  fp_fam   := FamilyObj(s.1);           # marrow for creating fp words
  # words first in free semigroup, then map to fp semigroup:
  w1       := AssocWordByLetterRep(free_fam, pair[1]);
  w2       := AssocWordByLetterRep(free_fam, pair[2]);
  p1       := ElementOfFpSemigroup(fp_fam, w1);
  p2       := ElementOfFpSemigroup(fp_fam, w2);
  # check if words are equal in the fp semigroup
  # WARNING: may run forever if undecidable
  if p1 = p2 then
    SEMIGROUPS.TietzeTransformation1(stz, pair);
    return;
  else
    ErrorNoReturn("StzAddRelation: second argument <pair> must list two\n",
                  "words that are equal in the presentation <stz>");
  fi;
end);

InstallMethod(StzAddRelation,
"for an stz presentation and a pair of semigroup elements",
[IsStzPresentation, IsList],
function(stz, pair)
  local s, pairwords, word;
  # retrieve original semigroup
  s := UnreducedFpSemigroup(stz);
  for word in pair do
    if not word in s then
      TryNextMethod();
    fi;
  od;

  # convert into words in original semigroup
  pairwords := List(pair, UnderlyingElement);
  Apply(pairwords, LetterRepAssocWord);
  # map to words in new semigroup
  Apply(pairwords, x -> SEMIGROUPS.StzExpandWord(x, TietzeForwardMap(stz)));

  # apply tietze1, with argument checks
  StzAddRelation(stz, pairwords);
  return;
end);

# Tietze Transformation 1: Add relation (NO REDUNDANCY CHECK)
InstallMethod(StzAddRelationNC, "for an stz presentation and a pair of words",
[IsStzPresentation, IsList],
1,  # bump priority, do list size check here
function(stz, pair)
  local n, word, letter;
  # <pair> should be a pair of LetterRep words.

  # argument checks:
  if Length(pair) <> 2 then
    ErrorNoReturn("StzAddRelationNC: second argument <pair> should be a list\n",
                  "of length 2");
  fi;
  n := Length(stz!.GeneratorsOfStzPresentation);
  for word in pair do
    if not IsList(word) then
      TryNextMethod();  # pass this on to the case where the list may be a pair
                        # of words in OG semigroup
    elif IsEmpty(word) then
      ErrorNoReturn("StzAddRelationNC: words in second argument <pair>\n",
                    "should be non-empty");
    else
      for letter in word do
        if not (IsPosInt(letter) and letter <= n) then
          ErrorNoReturn("StzAddRelationNC: words in second argument <pair>\n",
                        "should be lists of pos ints no greater than the\n",
                        "number of generators of first argument <stz>");
        fi;
      od;
    fi;
  od;

  # WARNING: no checks are run to verify that the pair is redundant. This
  # may result in an output semigroup which is non-isomorphic to the
  # starting semigroup.
  SEMIGROUPS.TietzeTransformation1(stz, pair);
  return;
end);

InstallMethod(StzAddRelationNC,
"for an stz presentation and a pair of semigroup elements",
[IsStzPresentation, IsList],
function(stz, pair)
  local s, pairwords, word;
  # retrieve original semigroup
  s := UnreducedFpSemigroup(stz);
  for word in pair do
    if not word in s then
      TryNextMethod();
    fi;
  od;

  # convert into words in original semigroup
  pairwords := List(pair, UnderlyingElement);
  Apply(pairwords, LetterRepAssocWord);
  # map to words in new semigroup
  Apply(pairwords, x -> SEMIGROUPS.StzExpandWord(x, TietzeForwardMap(stz)));

  # apply tietze1, without argument checks
  # WARNING: no checks are run to verify that the pair is redundant. This
  # may result in an output semigroup which is non-isomorphic to the
  # starting semigroup.
  SEMIGROUPS.TietzeTransformation1(stz, pairwords);
  return;
end);

# Tietze Transformation 2: Remove relation
InstallMethod(StzRemoveRelation,
"for an stz presentation and a pos int",
[IsStzPresentation, IsPosInt],
function(stz, index)
  local rels, pair, new_f, new_gens, new_s, free_fam, w1, w2, fp_fam, p1, p2;
  # <index> should be the index of the relation needing removing in the
  # overall list of relations.

  # argument check: valid index
  rels := ShallowCopy(stz!.RelationsOfStzPresentation);
  if index > Length(rels) then
    ErrorNoReturn("StzRemoveRelation: second argument <index> must be less\n",
                  "than or equal to the number of relations of the first\n",
                  "argument <stz>");
  fi;

  # create hypothetical fp semigroup that would be the result of removing
  # the requested pair
  pair := rels[index];
  Remove(rels, index);
  new_f    := FreeSemigroup(stz!.GeneratorsOfStzPresentation);
  new_gens := GeneratorsOfSemigroup(new_f);
  new_s    := new_f / List(rels,
                           x -> List(x,
                                     y -> Product(List(y,
                                                       z -> new_gens[z]))));

  # create two associative words
  free_fam := FamilyObj(new_f.1);
  w1       := AssocWordByLetterRep(free_fam, pair[1]);
  w2       := AssocWordByLetterRep(free_fam, pair[2]);

  # map these words to hypothetical fp semigroup words and check equality
  fp_fam := FamilyObj(new_s.1);
  p1     := ElementOfFpSemigroup(fp_fam, w1);
  p2     := ElementOfFpSemigroup(fp_fam, w2);

  # WARNING: may run forever if undecidable
  if p1 = p2 then
    SEMIGROUPS.TietzeTransformation2(stz, index);
    return;
  else
    ErrorNoReturn("StzRemoveRelation: second argument <index> must point to\n",
                  "a relation that is redundant in the presentation <stz>");
  fi;
end);

# Tietze Transformation 2: Remove relation (NO REDUNDANCY CHECK)
InstallMethod(StzRemoveRelationNC,
"for an stz presentation and a pos int",
[IsStzPresentation, IsPosInt],
function(stz, index)
  if index > Length(RelationsOfStzPresentation(stz)) then
    ErrorNoReturn("StzRemoveRelationNC: second argument <index> must be less\n",
                  "than or equal to the number of relations of the first\n",
                  "argument <stz>");
  fi;

  # WARNING: no checks are run to verify that the pair is redundant. This
  # may result in an output semigroup which is non-isomorphic to the
  # starting semigroup.
  SEMIGROUPS.TietzeTransformation2(stz, index);
  return;
end);

# Tietze Transformation 3: Add generator
InstallMethod(StzAddGenerator,
"for an stz presentation and a LetterRep word",
[IsStzPresentation, IsList],
function(stz, word)
  local n, letter;
  # argument checks
  if IsEmpty(word) then
    ErrorNoReturn("StzAddGenerator: cannot add generator equal to the empty\n",
                  "word");
  fi;
  n := Length(GeneratorsOfStzPresentation(stz));
  for letter in word do
    if not (IsPosInt(letter) and letter <= n) then
      # the argument has not been entered as a list of pos ints. Pass off to
      # potential future methods for Tietze3, but this is likely to be a
      # mistake.
      ErrorNoReturn("StzAddGenerator: second argument <word> is not a\n",
                    "list of pos ints at most equal to the number of\n",
                    "generators of the first argument <stz>");
    fi;
  od;

  # at this point all is good, so request new generator to be added with
  # a new generator name auto-created.
  SEMIGROUPS.TietzeTransformation3(stz, word, fail);
end);

InstallMethod(StzAddGenerator,
"for an stz presentation and a fp semigroup element",
[IsStzPresentation, IsElementOfFpSemigroup],
function(stz, word)
  local letterrepword;
  # argument check: word should be an element of the unreduced semigroup
  # (that way we can express it as a word on the current tietze generators)
  if not word in UnreducedFpSemigroup(stz) then
    TryNextMethod();
  fi;

  # if we do have an element of s, use the forward map to express it as a word
  # on the current generators, then run the original implementation of Tietze 3.
  letterrepword := SEMIGROUPS.StzExpandWord(
                     LetterRepAssocWord(UnderlyingElement(word)),
                     TietzeForwardMap(stz));
  StzAddGenerator(stz, letterrepword);
end);

# Tietze Transformation 3: Add generator (with specified new generator name)
InstallMethod(StzAddGenerator,
"for an stz presentation, a LetterRep word and a string",
[IsStzPresentation, IsList, IsString],
function(stz, word, name)
  local n, letter;
  # argument check 0: new word is non-empty
  if IsEmpty(word) then
    ErrorNoReturn("StzAddGenerator: cannot add generator equal to the empty\n",
                  "word");
  fi;
  # argument check 1: word is a valid word
  n := Length(GeneratorsOfStzPresentation(stz));
  for letter in word do
    if not (IsPosInt(letter) and letter <= n) then
      # the argument has not been entered as a list of pos ints. Pass off to
      # potential future methods for Tietze3, but this is likely to be a
      # mistake.
      ErrorNoReturn("StzAddGenerator: second argument <word> is not a\n",
                    "list of pos ints at most equal to the number of\n",
                    "generators of the first argument <stz>");
    fi;
  od;

  # argument check 2: requested generator name not yet used
  if name in GeneratorsOfStzPresentation(stz) then
    ErrorNoReturn("StzAddGenerator: third argument <name> should not be the\n",
                  "name of a pre-existing generator");
  fi;

  # otherwise we are all good
  SEMIGROUPS.TietzeTransformation3(stz, word, name);
end);

InstallMethod(StzAddGenerator,
"for an stz presentation, a fp semigroup element and a string",
[IsStzPresentation, IsElementOfFpSemigroup, IsString],
function(stz, word, name)
  local letterrepword;
  # argument check: word should be an element of the unreduced semigroup
  # (that way we can express it as a word on the current tietze generators)
  if not word in UnreducedFpSemigroup(stz) then
    TryNextMethod();
  fi;

  # if we do have an element of s, use the forward map to express it as a word
  # on the current generators, then run the original implementation of Tietze 3.
  letterrepword := SEMIGROUPS.StzExpandWord(
                     LetterRepAssocWord(UnderlyingElement(word)),
                     TietzeForwardMap(stz));
  StzAddGenerator(stz, letterrepword, name);
end);

# Tietze Transformation 4: Remove generator
InstallMethod(StzRemoveGenerator,
"for an stz presentation and a positive integer",
[IsStzPresentation, IsPosInt],
function(stz, gen)
  local found, index, i, rels;
  # argument check 1: requested removal is potentially possible
  if Length(GeneratorsOfStzPresentation(stz)) = 1 then
    ErrorNoReturn("StzRemoveGenerator: cannot remove only remaining\n",
                  "generator \"",
                  GeneratorsOfStzPresentation(stz)[1],
                  "\"");
  elif gen > Length(GeneratorsOfStzPresentation(stz)) then
    ErrorNoReturn("StzRemoveGenerator: second argument <gen> must be no\n",
                  "greater than the total number of generators");
  fi;

  # argument check 2: generator can be expressed as a product of others.
  # this check has to be repeated inside Tietze4, but we include it here to
  # ensure that an incorrect input is clearly reported to the user.
  found := false;
  rels  := RelationsOfStzPresentation(stz);
  for i in [1 .. Length(rels)] do
    if  (rels[i][1] = [gen] and not gen in rels[i][2]) or
        (rels[i][2] = [gen] and not gen in rels[i][1]) then
      found := true;
      index := i;
      continue;
    fi;
  od;
  if not found then
    ErrorNoReturn("StzRemoveGenerator: there is no relation in first\n",
                  "argument <stz> expressing second argument <gen> as a\n",
                  "product of other generators");
  fi;

  # otherwise all good; apply internal Tietze 4 function (it will check
  # whether any relation can actually express that generator as a combination
  # of others)
  SEMIGROUPS.TietzeTransformation4(stz, gen, index);
end);

InstallMethod(StzRemoveGenerator,
"for an stz presentation and a generator name",
[IsStzPresentation, IsString],
function(stz, genname)
  local gen;
  # find index of genname in stz gens
  gen := Position(GeneratorsOfStzPresentation(stz), genname);
  if gen = fail then
    ErrorNoReturn("StzRemoveGenerator: second argument <gen> does not\n",
                  "correspond to a generator name in first argument <stz>");
  else
    StzRemoveGenerator(stz, gen);
  fi;
end);

# Tietze transformation 4 with specified index (e.g. if there are several
# relations allowing the generator to be removed and the user wants a specific
# one)
InstallMethod(StzRemoveGenerator,
"for an stz presentation and two pos ints",
[IsStzPresentation, IsPosInt, IsPosInt],
function(stz, gen, index)
  # first argument check: requested generator exists
  if Length(GeneratorsOfStzPresentation(stz)) = 1 then
    ErrorNoReturn("StzRemoveGenerator: cannot remove only remaining\n",
                  "generator \"",
                  GeneratorsOfStzPresentation(stz)[1],
                  "\"");
  elif gen > Length(GeneratorsOfStzPresentation(stz)) then
    ErrorNoReturn("StzRemoveGenerator: second argument <gen> must be no\n",
                  "greater than the total number of generators");
  fi;

  # second argument check: index exists
  if index > Length(RelationsOfStzPresentation(stz)) then
    ErrorNoReturn("StzRemoveGenerator: third argument <index> must be no\n",
                  "greater than the total number of relations in first\n",
                  "argument <stz>");
  fi;

  # third argument check: a reasonable relation number has been supplied
  if (RelationsOfStzPresentation(stz)[index][1] <> [gen]
      or gen in RelationsOfStzPresentation(stz)[index][2])
      and (RelationsOfStzPresentation(stz)[index][2] <> [gen]
      or gen in RelationsOfStzPresentation(stz)[index][1]) then
    ErrorNoReturn("StzRemoveGenerator: third argument <index> does not point\n",
                  "to a relation expressing second argument <gen> as a\n",
                  "combination of other generators in first argument <stz>");
  fi;

  # if we made it this far then the substitution can be made
  SEMIGROUPS.TietzeTransformation4(stz, gen, index);
end);

InstallMethod(StzRemoveGenerator,
"for an stz presentation, a generator name and a pos int",
[IsStzPresentation, IsString, IsPosInt],
function(stz, genname, index)
  local gen;
  # find index of genname in stz gens
  gen := Position(GeneratorsOfStzPresentation(stz), genname);
  if gen = fail then
    ErrorNoReturn("StzRemoveGenerator: second argument <gen> does not\n",
                  "correspond to a generator name in first argument <stz>");
  else
    StzRemoveGenerator(stz, gen, index);
  fi;
end);

# Tietze Transformation 1/2: substitute relation
InstallMethod(StzSubstituteRelation,
"for an stz presentation and two positive integers",
[IsStzPresentation, IsPosInt, IsPosInt],
function(stz, index, side)
  local oldword, newword, new_rel, i;
  # argument check
  if index > Length(RelationsOfStzPresentation(stz)) then
    ErrorNoReturn("StzSubstituteRelation: second argument <index> must be no\n",
                  "greater than the number of relations in first argument\n",
                  "<stz>");
  fi;
  if not side in [1, 2] then
    ErrorNoReturn("StzSubstituteRelation: third argument <side> must be\n",
                  "either 1 or 2");
  fi;

  oldword := ShallowCopy(RelationsOfStzPresentation(stz)[index][side]);
  newword := ShallowCopy(RelationsOfStzPresentation(stz)[index][[2, 1][side]]);

  # Push relations onto the end of the stack of relations, each time replacing
  # the relation at the front by that relation, with oldword -> newword.
  # Each time, immediately delete the replaced relation at the front of the
  # stack, and do this as many times as there are relations, so that order
  # is preserved.
  # Special case for relation number <index> itself: it should not be changed.
  for i in [1 .. Length(RelationsOfStzPresentation(stz))] do
    if i = index then
      new_rel := [oldword, newword];
    else
      new_rel := List(stz!.RelationsOfStzPresentation[1],
                      x -> SEMIGROUPS.StzReplaceSubwordRel(x,
                                                           oldword,
                                                           newword));
    fi;
    SEMIGROUPS.TietzeTransformation1(stz, new_rel);
    SEMIGROUPS.TietzeTransformation2(stz, 1);
  od;
end);

########################################################################
# 5. Internal helper functions for word/relation manipulation etc.
########################################################################

SEMIGROUPS.StzReplaceSubword := function(rels, subword, newWord)
  local newRels, rel1, rel2, i;

  newRels := List([1 .. Length(rels)], x -> []);
  for i in [1 .. Length(rels)] do
    rel1 := SEMIGROUPS.StzReplaceSubwordRel(rels[i][1], subword, newWord);
    rel2 := SEMIGROUPS.StzReplaceSubwordRel(rels[i][2], subword, newWord);
    newRels[i] := [rel1, rel2];
  od;
  return newRels;
end;

SEMIGROUPS.StzReplaceSubwordRel := function(word, subword, newWord)
  local out, k, l, i;
  # Searches a single LetterRepAssocWord list and replaces instances of
  # subword with newWord.

  # build word up as we read through the old word.
  out := [];
  k   := Length(subword);
  l   := Length(word);
  i   := 1;  # current index that we are looking at when trying to see subword
  while i <= l do
    if word{[i .. Minimum(i + k - 1, l)]} = subword then
      # in this, case the word starting at i needs to be substituted out.
      # (the minimum is there to make sure we don't fall off the end of word)
      Append(out, newWord);
      # jump to end of occurrence
      i := i + k;
    else
      # move over by one and append the original letter, since the word is not
      # seen here.
      Add(out, word[i]);
      i := i + 1;
    fi;
  od;
  return out;
end;

# takes in a letterrep word and replaces every letter with its expression in
# dict.
# NOTE: does not check arguments. Assumes in good faith that every integer
# in word has an entry in the list <dict>.
SEMIGROUPS.StzExpandWord := function(word, dict)
  local out, letter;
  out := [];
  for letter in word do
    Append(out, dict[letter]);
  od;
  return out;
end;

SEMIGROUPS.NewGeneratorName := function(names_immut)
  local alph, Alph, na, nA, names_prefx, names_suffx, int_positions, prefixes,
  prefixes_collected, p, ints, i, name, names;
  names := [];
  for name in names_immut do
    Add(names, ShallowCopy(name));
  od;

  # useful helper variables
  alph := "abcdefghijklmnopqrstuvwxyz";
  Alph := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  # SPECIAL CASE 0: empty list
  if IsEmpty(names) then
    return "a";
  fi;

  # SPECIAL CASE 1: only one generator
  if Length(names) = 1 then
    if Length(names[1]) = 1 then
      if names[1][1] in Alph then
        return [First(Alph, x -> not [x] in names)];
      elif names[1][1] in alph then
        return [First(alph, x -> not [x] in names)];
      else
        return "a";
      fi;
    else
      return "a";
    fi;
  fi;

  # SPECIAL CASE 2: single letter names are present. Add an unused letter
  # with the most common capitalisation
  na := Length(Filtered(names, x -> Length(x) = 1 and x[1] in alph));
  nA := Length(Filtered(names, x -> Length(x) = 1 and x[1] in Alph));
  if 2 <= na and na < 26 then
    if na <= nA and nA < 26 then
      return [First(Alph, x -> not [x] in names)];
    else
      return [First(alph, x -> not [x] in names)];
    fi;
  elif 2 <= nA and nA < 26 then
    return [First(Alph, x -> not [x] in names)];
  fi;

  # SPECIAL CASE 3: there are names like s1, s3, s23, etc or x12, etc
  names_prefx := [];
  names_suffx := [];
  for name in names do
    Add(names_prefx, [name[1]]);
    Add(names_suffx, name{[2 .. Length(name)]});
  od;
  int_positions := PositionsProperty(names_suffx, x -> Int(x) <> fail
                                              and x <> ""
                                              and x[1] <> '-');
  if Length(int_positions) >= 2 then
    prefixes           := names_prefx{int_positions};
    prefixes_collected := Collected(prefixes);
    # look for highest frequency in collected list
    p := prefixes_collected[PositionMaximum(prefixes_collected, x -> x[2])][1];
    # find maximum suffix int, even amongst those with prefix not p
    ints := List(names_suffx{int_positions}, Int);
    i    := Maximum(ints) + 1;
    return Concatenation(p, String(i));
  fi;

  # if none of the special cases are covered, just try s1, s2,... until good
  for i in [1 .. Length(names) + 1] do
    if not Concatenation("s", String(i)) in names then
      return Concatenation("s", String(i));
    fi;
  od;
end;

# Counts the number of times a subword appears in the relations of an stz
# presentation, ensuring that the subwords do not overlap
# (Eg, for relations [[1,1,1,1],[1,1]], the subword [1,1] would return 3 and not
# 4)
SEMIGROUPS.StzCountRelationSubwords := function(stz, subWord)
  local count, relSide, rel, rels, pos, len, relSideCopy;
  rels := RelationsOfStzPresentation(stz);
  len := Length(subWord);
  count := 0;
  for rel in rels do
    for relSide in rel do
      pos := PositionSublist(relSide, subWord);
      relSideCopy := ShallowCopy(relSide);
      while pos <> fail do
        count := count + 1;
        relSideCopy := List([(pos + len) .. Length(relSideCopy)],
                            x -> relSideCopy[x]);
        pos := PositionSublist(relSideCopy, subWord);
      od;
    od;
  od;
  return count;
end;

# Converts an Stz presentation into an fp semigroup.
SEMIGROUPS.StzConvertObjToFpSemigroup := function(stz)
  local F, rels, gens;
  F    := FreeSemigroup(stz!.GeneratorsOfStzPresentation);
  rels := RelationsOfStzPresentation(stz);
  gens := GeneratorsOfSemigroup(F);
  return F / List(rels, x -> List(x, y -> Product(List(y, z -> gens[z]))));
end;

########################################################################
# 6.  Internal auto-checkers and appliers for presentation simplifying
########################################################################

## Format to add a new reduction check:
## StzCheck: function that takes an stz presentation as input and checks all
##           possible instances of the desired reduction, and outputs as a
##           record the least length of the possible reductions together with
##           the arguments required to achieve that length
## StzApply: function that takes an stz presentation and the above record as
##           input and applies the arguments from the record to achieve the
##           desired reduction
## Add the above two functions to the list results in StzSimplifyOnce as the
## pair [StzApply, StzCheck(stz)]

SEMIGROUPS.StzFrequentSubwordCheck := function(stz)
  local best_gain, best_word, flat, count_occurrences, n, c, gain, word,
        pair, i, j;
  # SUPER INEFFICIENT (~n^3), do something about this eventually (@Reinis?)
  # Look at every subword, count how many times it appears, and see whether
  # introducing a new generator equal to a given subword would make the
  # whole presentation shorter
  best_gain := 0;   # best reduction seen so far
  best_word := [];  # word currently holding record

  # flat list of words (don't care about which one is related to which)
  flat := [];
  for pair in stz!.RelationsOfStzPresentation do
    Append(flat, ShallowCopy(pair));  # TODO(later) might not need shallow copy
  od;

  # function to count occurrences of subword in list of lists
  count_occurrences := function(list, subword)
    local count, k, l, i, word;
    count := 0;
    k     := Length(subword);
    for word in list do
      l := Length(word);
      i := 1;  # index at which to start counting
      while i <= l - k + 1 do
        if word{[i .. i + k - 1]} = subword then
          count := count + 1;
          # jump to end of occurrence
          i := i + k;
        else
          # move over by one
          i := i + 1;
        fi;
      od;
    od;
    return count;
  end;

  # now check for every subword, how many times it appears
  for word in flat do
    # N.B. ASSUMES WORDS NON-EMPTY
    n := Length(word);
    for i in [1 .. n - 1] do
      for j in [i + 1 .. n] do
        c := count_occurrences(flat, word{[i .. j]});
        # now calculate what a gain that would be:
        # subbing out every instance of word{[i .. j]} for a word of length 1
        # makes us gain j - i characters per substitution
        # BUT, we also have a new generator cost of 1
        # AND a new relation cost of 3 + j - i
        gain := (c - 1) * (j - i) - 3;
        if gain > best_gain then
          best_gain := gain;
          best_word := word{[i .. j]};
        fi;
      od;
    od;
  od;
  return rec(reduction := Length(stz) - best_gain,
             word := best_word);
end;

SEMIGROUPS.StzFrequentSubwordApply := function(stz, metadata)
  local word, rels, n, gens, k, shortened_rels, new_rel, i, str, f, aWord;

  # have received instruction to sub out metadata.word for something shorter.
  word := metadata.word;
  rels := ShallowCopy(RelationsOfStzPresentation(stz));
  n    := Length(rels);
  gens := ShallowCopy(GeneratorsOfStzPresentation(stz));
  k    := Length(gens);

  # Build message
  str := "<Creating new generator to replace instances of word: ";
  f := FreeSemigroup(gens);
  aWord := AssocWordByLetterRep(FamilyObj(f.1), metadata.word);
  Append(str, PrintString(aWord));
  Append(str, ">");
  Info(InfoFpSemigroup, 2, PRINT_STRINGIFY(str));

  # first, add new generator.
  SEMIGROUPS.TietzeTransformation3(stz, word, fail);

  # then, go through and add loads of relations which are the old ones but
  # with the old word subbed out.
  shortened_rels := SEMIGROUPS.StzReplaceSubword(rels, word, [k + 1]);
  for new_rel in shortened_rels do
    SEMIGROUPS.TietzeTransformation1(stz, new_rel);
  od;

  # finally, remove the original relations.
  for i in [1 .. n] do
    SEMIGROUPS.TietzeTransformation2(stz, 1);
  od;
  return;
end;

# Checks each relation in the stz presentation in turn and determines if
# replacing the longer side by the shorter side reduces the length of the
# presentation
SEMIGROUPS.StzRelsSubCheck := function(stz)
  local rels, currentMin, currentRel, tempRel, len, relLenDiff, numInstances,
        newLen, i;
  rels := RelationsOfStzPresentation(stz);
  currentMin := Length(stz);
  currentRel := 0;
  for i in [1 .. Length(rels)] do
    tempRel := ShallowCopy(rels[i]);
    SortBy(tempRel, Length);
    len := Length(stz);
    relLenDiff := Length(tempRel[2]) - Length(tempRel[1]);
    numInstances := SEMIGROUPS.StzCountRelationSubwords(stz, tempRel[2]) - 1;
    newLen := len - numInstances * relLenDiff;
    if newLen < currentMin then
      currentMin := newLen;
      currentRel := i;
    fi;
  od;
  return rec(reduction := currentMin,
              argument := currentRel);
end;

# Checks each relation of the stz presentation to see if any are of the form
# a = w, where a is a generator and w is a word not containing a, then
# determines the length if the generator and relation were removed
SEMIGROUPS.StzRedundantGeneratorCheck := function(stz)
  local rel, rels, numInstances, relPos, tempPositions, r, out, redLen,
        genToRemove, wordToReplace, foundRedundant, currentMin, currentGen,
        currentRel;
  rels := RelationsOfStzPresentation(stz);
  out := [];
  currentMin := Length(stz);
  currentGen := 0;
  currentRel := 0;
  for rel in rels do
    foundRedundant := false;
    if Length(rel[1]) = 1 and Length(rel[2]) = 1 and rel[1] <> rel[2] then
      genToRemove := rel[1][1];
      wordToReplace := rel[2];
      Append(out, [Length(stz) - 3]);
      if Length(stz) - 3 < currentMin then
        currentMin := Length(stz) - 3;
        currentGen := genToRemove;
        currentRel := Position(rels, rel);
      fi;
      continue;
    elif Length(rel[1]) = 1 and Length(rel[2]) > 1 and
          (not (rel[1][1] in rel[2])) then
      genToRemove := rel[1][1];
      wordToReplace := rel[2];
      foundRedundant := true;
    elif Length(rel[2]) = 1 and Length(rel[1]) > 1 and
          (not (rel[2][1] in rel[1])) then
      genToRemove := rel[2][1];
      wordToReplace := rel[1];
      foundRedundant := true;
    fi;
    if foundRedundant then
      relPos := Position(rels, rel);
      numInstances := 0;
      for r in Concatenation([1 .. relPos - 1], [relPos + 1 .. Length(rels)]) do
        tempPositions := Length(Positions(Concatenation(rels[r][1], rels[r][2]),
                                          genToRemove));
        numInstances  := numInstances + tempPositions;
      od;
      redLen := Length(stz) + (numInstances * (Length(wordToReplace) - 1)) -
                2 - Length(wordToReplace);
      if redLen < currentMin then
        currentMin := redLen;
        currentGen := genToRemove;
        currentRel := Position(rels, rel);
      fi;
    fi;
  od;
  return rec(reduction := currentMin,
              argument := currentGen,
              infoRel := currentRel);
end;

# Checks each relation to determine if it is a direct duplicate of another
# (ie does not check equivalence in terms of other relations, just whether they
# are literally the same relation)
SEMIGROUPS.StzDuplicateRelsCheck := function(stz)
  local rel, rels, i, tempRel, j, currentMin, currentRel, len;
  rels := RelationsOfStzPresentation(stz);
  currentMin := Length(stz);
  currentRel := 0;
  if Length(rels) < 2 then
    return rec(reduction := Length(stz),
                argument := 1);
  else
    for j in [1 .. Length(rels)] do
      rel := rels[j];
      for i in Concatenation([1 .. j - 1],
                              [j + 1 .. Length(rels)]) do
        tempRel := rels[i];
        if (tempRel[1] = rel[1] and tempRel[2] = rel[2]) or
            (tempRel[1] = rel[2] and tempRel[2] = rel[1]) then
          len := Length(stz) - Length(rel[1]) - Length(rel[2]);
          if len < currentMin then
            currentMin := len;
            currentRel := j;
          fi;
        fi;
      od;
    od;
    return rec(reduction := currentMin,
                argument := currentRel);
  fi;
end;

# Checks each relation to determine if any are of the form w = w, where w is a
# word over the generators
SEMIGROUPS.StzTrivialRelationCheck := function(stz)
  local rels, len, i, currentMin, currentRel, newLen;
  rels := RelationsOfStzPresentation(stz);
  len := Length(stz);
  currentMin := len;
  currentRel := 0;
  for i in [1 .. Length(rels)] do
    if rels[i][1] = rels[i][2] then
      newLen := len - Length(rels[i][1]) - Length(rels[i][2]);
      if newLen < currentMin then
        currentMin := newLen;
        currentRel := i;
      fi;
    fi;
  od;
  return rec(reduction := currentMin,
              argument := currentRel);
end;

# Removes a trivial relation
SEMIGROUPS.StzTrivialRelationApply := function(stz, args)
  local str;
  str := "<Removing trivial relation: ";
  Append(str, SEMIGROUPS.StzRelationDisplayString(stz, args.argument));
  Append(str, ">");
  Info(InfoFpSemigroup, 2, PRINT_STRINGIFY(str));
  SEMIGROUPS.TietzeTransformation2(stz, args.argument);
end;

# Removes a duplicated relation
SEMIGROUPS.StzDuplicateRelsApply := function(stz, args)
  local str;
  str := "<Removing duplicate relation: ";
  Append(str, SEMIGROUPS.StzRelationDisplayString(stz, args.argument));
  Append(str, ">");
  Info(InfoFpSemigroup, 2, PRINT_STRINGIFY(str));
  SEMIGROUPS.TietzeTransformation2(stz, args.argument);
end;

# Removes a redundant generator
SEMIGROUPS.StzGensRedundantApply := function(stz, args)
  local str;
  str := "<Removing redundant generator ";
  Append(str, GeneratorsOfStzPresentation(stz)[args.argument]);
  Append(str, " using relation : ");
  Append(str, SEMIGROUPS.StzRelationDisplayString(stz, args.infoRel));
  Append(str, ">");
  Info(InfoFpSemigroup, 2, PRINT_STRINGIFY(str));
  SEMIGROUPS.TietzeTransformation4(stz, args.argument, args.infoRel);
end;

# Replaces all instances of one side of a relation with the other inside each
# other relation
SEMIGROUPS.StzRelsSubApply := function(stz, args)
  local str, relIndex, rels, rel, subword, replaceWord, containsRel, newRel, i,
        j;
  str := "<Replacing all instances in other relations of relation: ";
  Append(str, SEMIGROUPS.StzRelationDisplayString(stz, args.argument));
  Append(str, ">");
  Info(InfoFpSemigroup, 2, PRINT_STRINGIFY(str));

  relIndex := args.argument;
  rels := RelationsOfStzPresentation(stz);
  rel := ShallowCopy(rels[relIndex]);
  SortBy(rel, Length);
  # TODO(later) line above: potential edge cases where we are not substituting
  # the longer for the shorter, but rather two words of equal length? I guess
  # not, since the checker function would only suggest this applier function if
  # there was an actual reduction? In that case, careful to use correctly

  # record words to sub out (one we are searching for) and to replace with
  subword := rel[2];
  replaceWord := rel[1];

  # keep track of relation indices where we substituted
  containsRel := [];
  for i in [1 .. Length(rels)] do
    if  i <> relIndex and
        (PositionSublist(rels[i][1], subword) <> fail or
         PositionSublist(rels[i][2], subword) <> fail) then
      Add(containsRel, i);
    fi;
  od;

  for i in containsRel do
    newRel := List([1 .. 2], x -> ShallowCopy(rels[i][x]));
    newRel := List(newRel, x -> SEMIGROUPS.StzReplaceSubwordRel(x, subword,
                                                                replaceWord));
    SEMIGROUPS.TietzeTransformation1(stz, newRel);
  od;
  for j in [1 .. Length(containsRel)] do
    SEMIGROUPS.TietzeTransformation2(stz, containsRel[j]);
    containsRel := containsRel - 1;
  od;
end;

########################################################################
# 7. SimplifyPresentation etc.
########################################################################

InstallMethod(StzSimplifyOnce,
[IsStzPresentation],
function(stz)
  local rels, results, len, mins, result, func, args;
  rels := RelationsOfStzPresentation(stz);
  if IsEmpty(rels) then
    return false;
  else
    results := [[SEMIGROUPS.StzGensRedundantApply,
                  SEMIGROUPS.StzRedundantGeneratorCheck(stz)],
                [SEMIGROUPS.StzDuplicateRelsApply,
                  SEMIGROUPS.StzDuplicateRelsCheck(stz)],
                [SEMIGROUPS.StzRelsSubApply,
                  SEMIGROUPS.StzRelsSubCheck(stz)],
                [SEMIGROUPS.StzFrequentSubwordApply,
                  SEMIGROUPS.StzFrequentSubwordCheck(stz)],
                [SEMIGROUPS.StzTrivialRelationApply,
                  SEMIGROUPS.StzTrivialRelationCheck(stz)]];
    len := Length(stz);
    mins := List(results, x -> x[2].reduction);
    if Minimum(mins) < len then
      result := results[Position(mins, Minimum(mins))];
      func := result[1];
      args := result[2];
      func(stz, args);
      return true;
    fi;
    return false;
  fi;
end);

InstallMethod(StzSimplifyPresentation,
[IsStzPresentation],
function(stz)
  local transformApplied;
  transformApplied := true;
  Info(InfoFpSemigroup, 2, "Applying StzSimplifyPresentation...");
  Info(InfoFpSemigroup, 2, "StzSimplifyPresentation is verbose by default. ",
                           "Use SetInfoLevel(InfoFpSemigroup, 1) to hide");
  Info(InfoFpSemigroup, 2, "output while maintaining ability to use ",
                           "StzPrintRelations, StzPrintGenerators, etc.");
  Info(InfoFpSemigroup, 2, Concatenation("Current: ", ViewString(stz)));
  while transformApplied do
    transformApplied := StzSimplifyOnce(stz);
    if transformApplied then
      Info(InfoFpSemigroup, 2, Concatenation("Current: ", ViewString(stz)));
    fi;
  od;
end);

InstallMethod(SimplifiedFpSemigroup,
[IsFpSemigroup],
function(S)
  local T, map;
  map := SimplifyFpSemigroup(S);
  T := Range(map);
  SetUnreducedFpSemigroup(T, S);
  SetFpTietzeIsomorphism(T, map);
  return T;
end);

InstallMethod(SimplifyFpSemigroup, "for an f.p. semigroup", [IsFpSemigroup],
function(S)
  local stz;
  stz := StzPresentation(S);
  StzSimplifyPresentation(stz);
  return StzIsomorphism(stz);
end);
