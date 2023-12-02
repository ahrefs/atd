Capturing the current output of the demo.

  $ ./resume.exe
  VALID:
  [
    {
      "company": "Acme Corp.",
      "title": "Tester",
      "start_date": { "year": 2005, "month": 8, "day": 1 },
      "end_date": { "year": 2006, "month": 3, "day": 22 }
    },
    {
      "company": "Acme Corp.",
      "title": "Tester",
      "start_date": { "year": 2000, "month": 2, "day": 29 },
      "end_date": { "year": 2006, "month": 3, "day": 22 }
    }
  ]
  INVALID:
  [
    {
      "company": "Acme Corp.",
      "title": "Tester",
      "start_date": { "year": 2005, "month": 8, "day": 1 },
      "end_date": { "year": 2006, "month": 3, "day": 22 }
    },
    {
      "company": "Acme Corp.",
      "title": "Tester",
      "start_date": { "year": 2005, "month": 8, "day": 1 },
      "end_date": { "year": 1900, "month": 0, "day": 0 }
    }
  ]
