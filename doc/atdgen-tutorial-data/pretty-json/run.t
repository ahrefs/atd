  $ cat single.json
  [1234,"abcde",{"start_date":{"year":1970,"month":1,"day":1}, 
  "end_date":{"year":1980,"month":1,"day":1}}]

  $ ydump single.json
  [
    1234,
    "abcde",
    {
      "start_date": { "year": 1970, "month": 1, "day": 1 },
      "end_date": { "year": 1980, "month": 1, "day": 1 }
    }
  ]

  $ cat stream.json
  [1234,"abcde",{"start_date":{"year":1970,"month":1,"day":1}, 
  "end_date":{"year":1980,"month":1,"day":1}}]
  [1,"a",{}]

  $ ydump -s stream.json
  [
    1234,
    "abcde",
    {
      "start_date": { "year": 1970, "month": 1, "day": 1 },
      "end_date": { "year": 1980, "month": 1, "day": 1 }
    }
  ]
  [ 1, "a", {} ]

  $ ./prettify.exe
  [
    1234,
    "abcde",
    {
      "start_date": { "year": 1970, "month": 1, "day": 1 },
      "end_date": { "year": 1980, "month": 1, "day": 1 }
    }
  ]
