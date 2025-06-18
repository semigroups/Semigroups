

BindGlobal("TYPES_TWISTED_BIPART", []);
BindGlobal("TYPE_TWISTED_BIPART",
function(n, d)
  local fam, type;

  if IsBound(TYPES_TWISTED_BIPART[n + 1]) then
    if IsBound(TYPES_TWISTED_BIPART[n + 1][d + 1]) then
      return TYPES_TWISTED_BIPART[n + 1][d + 1];
    fi;
  else
    TYPES_TWISTED_BIPART[n + 1] := [];
  fi;

  fam := NewFamily(Concatenation("TwistedBipartitionFamily", String(n), "_", String(d)),
                   IsTwistedBipartition,
                   CanEasilySortElements,
                   CanEasilySortElements);
  fam!.DegreeOfBipartition := n;
  fam!.MaxFloatingBlocks := d;

  type := NewType(fam, IsTwistedBipartition and IsPositionalObjectRep);
  TYPES_TWISTED_BIPART[n + 1][d + 1] := type;
  return type;
end);

InstallMethod(ZeroTwistedBipartition,
"for degree and max. floating blocks", [IsInt, IsInt],
function(n, d)
  local result;

    result := [];
    Objectify(TYPE_TWISTED_BIPART(n, d), result);
    return result;
end);

InstallMethod(TwistedBipartition,
"for number of floating blocks, bipartition and max. floating blocks (d)",
[IsInt, IsBipartition, IsInt],
function(i, bipart, d)
  local n, result;

# TODO check args: i and d are non-negative,
# and i <= d
n := DegreeOfBipartition(bipart);
result := [i, bipart];
Objectify(TYPE_TWISTED_BIPART(n, d), result);
return result;
end);

InstallMethod(MaxFloatingBlocks, "for a twisted bipartition",
[IsTwistedBipartition],
x -> FamilyObj(x)!.MaxFloatingBlocks);

InstallMethod(DegreeOfTwistedBipartition, "for a twisted bipartition",
[IsTwistedBipartition],
x -> FamilyObj(x)!.DegreeOfBipartition);

InstallMethod(IsZero, "for a twisted bipartition",
[IsTwistedBipartition], x -> not IsBound(x![1]));

# TODO special case for IsZero
InstallMethod(NrFloatingBlocks, "for a twisted bipartition",
[IsTwistedBipartition], x -> x![1]);

# TODO special case for IsZero
InstallMethod(UnderlyingBipartition, "for a twisted bipartition",
[IsTwistedBipartition], x -> x![2]);

# TODO add IsOne and One

InstallMethod(Zero, "for a twisted bipartition",
[IsTwistedBipartition],
function(x)
  local n, d, result;

    if IsZero(x) then
    return x;
    fi;

    n := DegreeOfBipartition(UnderlyingBipartition(x));
    d := MaxFloatingBlocks(x);
end);

InstallMethod(One, "for a twisted bipartition",
[IsTwistedBipartition],
function(x)
  local n, d;
    n := DegreeOfBipartition(UnderlyingBipartition(x));
    d := MaxFloatingBlocks(x);
    return TwistedBipartition(0, IdentityBipartition(n), d);
end);

# TODO IdentityTwistedBipartition

InstallMethod(ViewString, "for a twisted bipartition",
[IsTwistedBipartition],
function(x)
    if IsZero(x) then
        return StringFormatted("ZeroTwistedBipartition({}, {})",
        DegreeOfTwistedBipartition(x), MaxFloatingBlocks(x));
    fi;
   return StringFormatted("TwistedBipartition({}, {}, {})",
   NrFloatingBlocks(x), PrintString(UnderlyingBipartition(x)),
   MaxFloatingBlocks(x));
end);

InstallMethod(RandomNonZeroTwistedBipartition,
"for a degree and max. floating blocks", [IsInt, IsInt],
{n, d} -> TwistedBipartition(Random([0 .. d]), RandomBipartition(n), d));

InstallMethod(\*, "for twisted bipartition", IsIdenticalObj,
[IsTwistedBipartition, IsTwistedBipartition],
function(x, y)
  local xx, yy, floaters;

    if IsZero(x) then
        return x;
    elif IsZero(y) then
        return y;
    fi;
    xx := UnderlyingBipartition(x);
    yy := UnderlyingBipartition(y);

    floaters := NrFloatingBlocks(xx, yy) + NrFloatingBlocks(x) + NrFloatingBlocks(y);

    if floaters > MaxFloatingBlocks(x) then
        return ZeroTwistedBipartition(DegreeOfTwistedBipartition(x),
                                      MaxFloatingBlocks(x));
    fi;

    return TwistedBipartition(floaters, xx * yy, MaxFloatingBlocks(x));
end);

InstallMethod(\=, "for twisted bipartition", IsIdenticalObj,
[IsTwistedBipartition, IsTwistedBipartition],
function(x, y)
if IsZero(x) then
return IsZero(y);
elif IsZero(y) then
return false;
fi;
return NrFloatingBlocks(x) = NrFloatingBlocks(y) and UnderlyingBipartition(x) =
UnderlyingBipartition(y);
end);

InstallMethod(\<, "for twisted bipartition", IsIdenticalObj,
[IsTwistedBipartition, IsTwistedBipartition],
function(x, y)
if IsZero(x) then
return not IsZero(y);
elif IsZero(y) then
return false;
fi;
return NrFloatingBlocks(x) < NrFloatingBlocks(y) or NrFloatingBlocks(x) <
NrFloatingBlocks(y) and UnderlyingBipartition(x) < UnderlyingBipartition(y);
end);

