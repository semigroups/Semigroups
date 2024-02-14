files_to_delete := ["testbugfix/2021-04-13-TryMaximals.tst",
                    "testbugfix/2023-10-18-SetDimension.tst"];
for f in files_to_delete do
  f := Filename(DirectoriesLibrary("tst"), f);
  if f <> fail then
    Exec(Concatenation("rm -f ", f));
  fi;
od;

if GAPInfo.BytesPerVariable < 8 then
  files_to_delete_32_bit := ["testbugfix/2024-01-25-MaxAbQuot.tst"]; # too much memory
  for f in files_to_delete_32_bit do
    f := Filename(DirectoriesLibrary("tst"), f);
    if f <> fail then
      Exec(Concatenation("rm -f ", f));
    fi;
  od;
fi;

Read(Filename(DirectoriesLibrary("tst"), "testbugfix.g"));
