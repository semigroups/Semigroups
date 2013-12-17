#############################################################################
##
#W  display.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#

InstallMethod(DotDClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  return DotDClasses(s, rec(maximal:=false, number:=true));
end);

InstallMethod(DotDClasses, "for an acting semigroup and record",
[IsActingSemigroup, IsRecord],
function(s, opts)
  local str, i, gp, h, rel, j, k, d, l, x;

  # process the options
  if not IsBound(opts.maximal) then 
    opts.maximal:=false;
  fi;
  if not IsBound(opts.number) then 
    opts.number:=true;
  fi;

  str:="";
  Append(str, "digraph  DClasses {\n");
  Append(str, "node [shape=plaintext]\n");
  Append(str, "edge [color=red,arrowhead=none]\n");
  i:=0;

  for d in DClasses(s) do
    i:=i+1;
    Append(str, String(i));
    Append(str, " [shape=box style=dotted label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\"");
    Append(str, " CELLPADDING=\"10\" CELLSPACING=\"0\"");
    Append(str, Concatenation(" PORT=\"", String(i), "\">\n"));

    if opts.number then
      Append(str, "<TR BORDER=\"0\"><TD COLSPAN=\"");
      Append(str, String(NrRClasses(d)));
      Append(str, "\" BORDER=\"0\" >");
      Append(str, String(i));
      Append(str, "</TD></TR>");
    fi;

    if opts.maximal and IsRegularDClass(d) then
       gp:=StructureDescription(GroupHClass(d));
    fi;

    for l in LClasses(d) do
      Append(str, "<TR>");
      if not IsRegularClass(l) then
        for j in [1..NrRClasses(d)] do
          Append(str, "<TD CELLPADDING=\"10\"> </TD>");
        od;
      else
        h:=HClasses(l);
        for x in h do
          if IsGroupHClass(x) then
            if opts.maximal then
              Append(str, Concatenation("<TD BGCOLOR=\"grey\">", gp, "</TD>"));
            else
              Append(str, "<TD BGCOLOR=\"grey\">*</TD>");
            fi;
          else
            Append(str, "<TD></TD>");
          fi;
        od;
      fi;
      Append(str, "</TR>\n");
    od;
    Append(str, "</TABLE>>];\n");
  od;

  rel:=PartialOrderOfDClasses(s);
  rel:=List([1..Length(rel)], x-> Filtered(rel[x], y-> not x=y));

  for i in [1..Length(rel)] do
    j:=Difference(rel[i], Union(rel{rel[i]})); i:=String(i);
    for k in j do
      k:=String(k);
      Append(str, Concatenation(i, " -> ", k, "\n"));
    od;
  od;

  Append(str, " }");

  return str;
end);

#EOF
