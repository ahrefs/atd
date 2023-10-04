// Test JSON reading and writing against expectations.

/* tslint:disable no-console  */

import * as API from "./everything"
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
  ]
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

test_everything()
