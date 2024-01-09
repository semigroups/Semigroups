#############################################################################
##
##  fp/word.gi
##  Copyright (C) 2020-2022                                 Maria Tsalakou
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(WordToString, "for a string and a homogeneous list",
[IsString, IsHomogeneousList],
function(alphabet, word)
  if IsEmpty(word) then
    return "";
  elif not ForAll(word, IsPosInt) then
    ErrorNoReturn("expected list of positive integers as 2nd argument");
  elif Maximum(word) > Length(alphabet) then
    ErrorNoReturn("the 1st argument (a string) is too short, ",
                  "expected at least ", Maximum(word),
                  " but found ", Length(alphabet));
  fi;
  return List(word, i -> alphabet[i]);
end);

InstallMethod(RandomWord, "for two integers",
[IsInt, IsInt],
function(length, number_letters)
  if length < 0 then
    ErrorNoReturn("expected non-negative integer as 1st argument");
  elif number_letters < 0 then
    ErrorNoReturn("expected non-negative integer as 2nd argument");
  elif number_letters = 0 and length > 0 then
    ErrorNoReturn("the 1st argument (an integer) cannot be ",
                  "non-zero if the 2nd argument is 0");
  fi;
  return List([1 .. length], x -> Random([1 .. number_letters]));
end);

InstallMethod(StandardiseWord, "for a homogeneous list",
[IsHomogeneousList],
function(word)
  local distinct_chars, lookup, i;

  if IsEmpty(word) then
    return word;
  elif not ForAll(word, IsPosInt) then
    ErrorNoReturn("expected a list of positive integers as 2nd argument");
  fi;

  distinct_chars := 1;
  lookup         := [];

  for i in [1 .. Length(word)] do
    if IsBound(lookup[word[i]]) then
      word[i] := lookup[word[i]];
    else
      lookup[word[i]]   := distinct_chars;
      word[i]           := distinct_chars;
      distinct_chars    := distinct_chars + 1;
    fi;
  od;
  return word;
end);

InstallMethod(StringToWord, "for a string", [IsString],
w -> StandardiseWord(List(w, IntChar)));
