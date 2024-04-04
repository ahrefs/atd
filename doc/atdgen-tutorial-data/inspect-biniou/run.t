  $ ./tree.exe
  raw value (saved as tree.dat):
  "\023\179\2276\"\020\003\023\179\2276\"\020\003\023\003\007\170m\017\002\023\003\007\170m\017\004\023\179\2276\"\020\003\023\179\2276\"\020\003\023\003\007\170m\017\006\023\003\007\170m\017\b\023\179\2276\"\020\003\023\003\007\170m\017\n\023\003\007\170m"
  length: 75
  pretty-printed value (without dictionary):
  <#33e33622:
     (<#33e33622: (<#0307aa6d>, 1, <#0307aa6d>)>,
      2,
      <#33e33622:
         (<#33e33622: (<#0307aa6d>, 3, <#0307aa6d>)>,
          4,
          <#33e33622: (<#0307aa6d>, 5, <#0307aa6d>)>)>)>
  pretty-printed value (with dictionary):
  <"Node":
     (<"Node": (<"Empty">, 1, <"Empty">)>,
      2,
      <"Node":
         (<"Node": (<"Empty">, 3, <"Empty">)>,
          4,
          <"Node": (<"Empty">, 5, <"Empty">)>)>)>

Running the tree executable has serialized the tree into a biniou file.

  $ ls tree.dat
  tree.dat

We make use of a custom dictionary for this test, start with an empty one.

  $ touch bdump.dict

Without entries in the directionary, the fields are printed as raw values.

  $ bdump -h bdump.dict tree.dat
  <#33e33622:
     (<#33e33622: (<#0307aa6d>, 1, <#0307aa6d>)>,
      2,
      <#33e33622:
         (<#33e33622: (<#0307aa6d>, 3, <#0307aa6d>)>,
          4,
          <#33e33622: (<#0307aa6d>, 5, <#0307aa6d>)>)>)>

Let's add the dictionary entries for the tree.

  $ bdump -h bdump.dict -w Empty,Node tree.dat
  <"Node":
     (<"Node": (<"Empty">, 1, <"Empty">)>,
      2,
      <"Node":
         (<"Node": (<"Empty">, 3, <"Empty">)>,
          4,
          <"Node": (<"Empty">, 5, <"Empty">)>)>)>

The dictionary entries have been saved into the dictionary for further use.

  $ bdump -h bdump.dict tree.dat
  <"Node":
     (<"Node": (<"Empty">, 1, <"Empty">)>,
      2,
      <"Node":
         (<"Node": (<"Empty">, 3, <"Empty">)>,
          4,
          <"Node": (<"Empty">, 5, <"Empty">)>)>)>
