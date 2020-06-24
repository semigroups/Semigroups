#############################################################################
##
##  word.gi
##  Copyright (C) 2020                                      Maria Tsalakou
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(WordToString, "for a string and a list",
[IsString, IsList],
function(alphabet, word)
  local str, i;
   if Maximum(word) > Length(alphabet) then
     ErrorNoReturn("Semigroups: WordToString: usage,\n",
                   "there are not enough letters in the alphabet,");
   fi;
  str := "";
  for i in word do
     Add(str, alphabet[i]);
  od;
  return str;
end);

InstallMethod(RandomWord, "for two integers",
[IsInt, IsInt],
function(length, number_letters)
  local word, i;
  word := EmptyPlist(length);
  for i in [1 .. length] do
      Add(word, Random([1 .. number_letters]));
  od;
  return word;
end);
