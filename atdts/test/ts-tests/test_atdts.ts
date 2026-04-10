// Test JSON reading and writing against expectations.

/* tslint:disable no-console  */

import * as API from "./everything"
import * as ImportAlias from "./import_alias"
import * as ext_types from "./ext_types"
import * as fs from "fs"

/* eslint-disable @typescript-eslint/no-unused-vars */
// Check that types are exported
const aaa: API.Option<string> = null
/* eslint-enable @typescript-eslint/no-unused-vars */

function assert(isTrue: boolean, errmsg: string) {
  if (!isTrue) {
    throw new Error(errmsg)
  }
}

function save(file: string, data: string) {
  console.log('Saving data to file ' + file)
  fs.writeFile(file, data, err => {
    if (err)
      throw new Error(String(err))
  })
}

function test_everything() {
  const aObj : API.Root = {
    id: "abc",
    this_: 100,
    items: [[], [1, 2]],
    // maybe?: 123,
    extras: [17, 53],
    answer: 42,
    aliased: [8, 9, 10],
    point: [3.3, -77.22],
    kinds: [
      { kind: 'WOW' },
      { kind: 'Thing', value: 99 },
      { kind: 'Amaze', value: ["a", "b"] },
      { kind: 'Root' }
    ],
    assoc1: [
      [1.1, 1],
      [2.2, 2],
    ],
    assoc2: [
      ["c", 3],
      ["d", 4],
    ],
    assoc3: new Map([
      [5.5, 5],
      [6.6, 6],
    ]),
    assoc4: new Map([
      ["g", 7],
      ["h", 8],
    ]),
    options: [{value:10}, null, {value:88}],
    nullables: [13, 71],
    untyped_things: [{}, [["hello"]], 123],
    foo: null,
    parametrized_record: {
      field_a: 42,
      field_b: [
        9.9,
        8.8
      ]
    },
    parametrized_tuple: [
      { kind: 'WOW' },
      { kind: 'WOW' },
      100
    ],
    anything: {
      any_a: 1234,
      any_b: [ [], [[[]]], true, {}, null ]
    },
    wrapped: ["a", "b"],
  ext_tag: "hello",
  }
  const aStr = JSON.stringify(API.writeRoot(aObj), null, 2)
  save('aStr', aStr)

  console.log(
    `----- aStr (converted from original TS object) -----
${aStr}`
  )

  // expected output (copy-pasted from an earlier run)
  const bStr =
`{
  "ID": "abc",
  "this": 100,
  "items": [
    [],
    [
      1,
      2
    ]
  ],
  "extras": [
    17,
    53
  ],
  "answer": 42,
  "aliased": [
    8,
    9,
    10
  ],
  "point": [
    3.3,
    -77.22
  ],
  "kinds": [
    "wow",
    [
      "Thing",
      99
    ],
    [
      "!!!",
      [
        "a",
        "b"
      ]
    ],
    "Root"
  ],
  "assoc1": [
    [
      1.1,
      1
    ],
    [
      2.2,
      2
    ]
  ],
  "assoc2": {
    "c": 3,
    "d": 4
  },
  "assoc3": [
    [
      5.5,
      5
    ],
    [
      6.6,
      6
    ]
  ],
  "assoc4": {
    "g": 7,
    "h": 8
  },
  "options": [
    [
      "Some",
      10
    ],
    "None",
    [
      "Some",
      88
    ]
  ],
  "nullables": [
    13,
    71
  ],
  "untyped_things": [
    {},
    [
      [
        "hello"
      ]
    ],
    123
  ],
  "foo": null,
  "parametrized_record": {
    "field_a": 42,
    "field_b": [
      9.9,
      8.8
    ]
  },
  "parametrized_tuple": [
    "wow",
    "wow",
    100
  ],
  "anything": {
    "any_a": 1234,
    "any_b": [
      [],
      [
        [
          []
        ]
      ],
      true,
      {},
      null
    ]
  },
  "wrapped": [
    "a",
    "b"
  ],
  "ext_tag": "hello"
}`
  save('bStr', bStr)
  const bObj = API.readRoot(JSON.parse(aStr))
  const bStr2 = JSON.stringify(API.writeRoot(bObj), null, 2)
  save('bStr2', bStr2)

  assert(
    bStr === bStr2,
    `JSON mismatch:
----- expected (bStr) -----
${bStr}
----- actual (bStr2) -----
${bStr2}
---------------------------`
  )
  assert(
    bStr2 === aStr,
    `JSON mismatch:
----- expected (bStr2) -----
${bStr2}
----- actual (aStr) -----
${aStr}
--------------------------`
  )
}

function test_import_alias() {
  const obj: ImportAlias.Item = { tag: ext_types.readTag(JSON.parse('"renamed"')) }
  const j = JSON.stringify(ImportAlias.writeItem(obj))
  assert(j === '{"tag":"renamed"}', `unexpected JSON: ${j}`)
  const obj2 = ImportAlias.readItem(JSON.parse(j))
  assert(ext_types.writeTag(obj2.tag) === "renamed", "tag round-trip failed")
}

function test_sum_repr_object() {
  // With <json repr="object">, tagged variants (those carrying a payload)
  // are encoded as single-key JSON objects {"Constructor": payload} instead
  // of the default two-element array ["Constructor", payload].
  // This matches the default Rust/Serde externally-tagged encoding and is
  // also natural YAML syntax (a single-key mapping per variant).
  // Unit variants (no payload) remain plain strings in all cases.

  // Encoding
  const circle: API.Shape = { kind: 'Circle', value: 3.14 }
  const square: API.Shape = { kind: 'Square', value: 2.0 }
  const point: API.Shape = { kind: 'Point' }

  assert(JSON.stringify(API.writeShape(circle)) === '{"Circle":3.14}',
    'Circle encoding failed')
  assert(JSON.stringify(API.writeShape(square)) === '{"Square":2}',
    'Square encoding failed')
  assert(JSON.stringify(API.writeShape(point)) === '"Point"',
    'Point (unit variant) should be a plain string')

  // Round-trip decoding
  const c2 = API.readShape(JSON.parse('{"Circle":1.0}'))
  assert(c2.kind === 'Circle', 'Circle decode: wrong kind')
  assert((c2 as {kind: 'Circle'; value: number}).value === 1.0, 'Circle decode: wrong value')

  const p2 = API.readShape(JSON.parse('"Point"'))
  assert(p2.kind === 'Point', 'Point decode: wrong kind')

  // Error on unknown constructor
  let threw = false
  try { API.readShape(JSON.parse('{"Triangle":3}')) }
  catch (_) { threw = true }
  assert(threw, 'Expected error for unknown constructor')
}

test_everything()
test_import_alias()
test_sum_repr_object()
