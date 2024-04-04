Output a sample config

  $ ./config.exe -template
  {
    "title": "",
    "timeout": 10,
    "credentials": [ { "name": "foo", "key": "0123456789abcdef" } ]
  }

Print the original type definitions

  $ ./config.exe -format
  type config = {
    title : string;
    ?description : string option;
    ~timeout <ocaml default="10"> : int;
    ~credentials : param list
      <ocaml valid="fun l ->
                      l <> [] || failwith \"missing credentials\"">;
  }
  
  type param = {
    name : string
      <ocaml valid="fun s -> s <> \"\"">;
    key : string
      <ocaml valid="fun s -> String.length s = 16">;
  }

Fail to validate an invalid config file

  $ ./config.exe -validate bad-config1.json || :
  Error: File bad-config1.json, line 5, bytes 14-48:
  Expected '"' but found '0,
        "key": "db7c0877bdef3016'

Fail to validate another invalid config file (using custom validators)

  $ ./config.exe -validate bad-config2.json || :
  Error: Found unknown JSON field tiemout while expecting type defined at: File "config.atd", line 1, characters 14-228

Validate, inject missing defaults and pretty-print

  $ ./config.exe -validate sample-config.json
  {
    "title": "Example",
    "timeout": 10,
    "credentials": [
      { "name": "joeuser", "key": "db7c0877bdef3016" },
      { "name": "tester", "key": "09871ff387ac2b10" }
    ]
  }
