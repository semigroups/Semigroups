###########################################################################
##
##  libsemigroups/words.gi
##  Copyright (C) 2023                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
###########################################################################
##

InstallMethod(Iterator, "for an IsWords object", [IsWords],
function(words)
  local iter;

  iter := rec();
  iter.WordsObj := words;
  iter.NextIterator := function(it)
    Next(it!.WordsObj);
    return Get(it!.WordsObj);
  end;
  iter.IsDoneIterator := it -> AtEnd(it!.WordsObj);

  iter.ShallowCopy := function(it)
    local copy;
    copy := rec();
    copy.NextIterator := it.NextIterator;
    copy.IsDoneIterator := it.IsDoneIterator;
    # TODO should be Words(it.WordsObj)
    copy.WordsObj := it.WordsObj;
  end;

  return IteratorByFunctions(iter);
end);
