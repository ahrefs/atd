// Handwritten code that serves as a model for generated code.

/*
export class Root_ {
  kind: 'Root';
  constructor() {
    this.kind = 'Root'
  }

  public toJSON() {
    return this.kind;
  }
}
*/

export type Root_ = {
  kind: 'Root';
}

export type Thing = {
  kind: 'Thing';
  value: Int;
}

export type WOW = {
  kind: 'WOW';
}

export type Amaze = {
  kind: 'Amaze';
  value: string[];
}

export type Kind = Root_ | Thing | WOW | Amaze

export type Root = {
  id: string;
  items: Int[][];
  maybe?: Int;
  extras: Int[];
  point: [number, number];
  kinds: Kind[];
}

export function KindFromJSON(x: any): Kind {
  switch (x) {
  case 'Root':
    return { kind: 'Root' };
  case 'wow':
    return { kind: 'WOW' };
  default:
    switch (x[0]) {
    case 'Thing':
      return { kind: 'Thing', value: x[1] };
    case '!!!':
      return { kind: 'Amaze', value: x[1] };
    default:
      throw new Error('bad JSON');
    }
  }
}

export function KindToJSON(x: Kind): any {
  switch (x.kind) {
  case 'Root':
    return 'Root';
  case 'WOW':
    return 'wow';
  case 'Thing':
    return ['Thing', x.value];
  case 'Amaze':
    return ['!!!', x.value];
  }
}

// Test the code above

function main(x: Kind) {
  switch (x.kind) {
  case 'Thing':
    console.log(x.value);
    break;
  case 'Root':
    console.log('root');
    break;
  default:
    console.log('other');
  }
}

main({ kind: 'Thing', value: 42 });
// main(new Root_());
main({ kind: 'Root' });
console.log(KindFromJSON(KindToJSON({ kind: 'Thing', value: 42 })))
_atd_missing_json_field('a', 'b')

////////////////////// Constant runtime library /////////////////////////////

// This type alias documents extra validation when converting from/to JSON
export type Int = number

function _atd_missing_json_field(type_name: string, json_field_name: string) {
  throw new Error(`missing field '${json_field_name}'` +
                  ` in JSON object of type '${type_name}'`)
}

function _atd_bad_json(expected_type: string, json_value: any) {
  let value_str = String(json_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible JSON value where` +
                  ` type '${expected_type}' was expected: '${value_str}'`)
}

function _atd_bad_ts(expected_type: string, json_value: any) {
  let value_str = String(json_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible TypeScript value where` +
                  ` type '${expected_type}' was expected: '${value_str}'`)
}

function _atd_read_unit(x: any) {
  if (x === null)
    return x;
  else
    _atd_bad_json('unit', x);
}

function _atd_read_bool(x: any): boolean {
  if (typeof x === 'boolean')
    return x;
  else
    _atd_bad_json('bool', x)
}

function _atd_read_int(x: any): Int {
  if (Number.isInteger(x))
    return x;
  else
    _atd_bad_json('integer', x)
}

function _atd_read_float(x: any): number {
  if (isFinite(x))
    return x;
  else
    _atd_bad_json('number', x)
}

function _atd_read_string(x: any): string {
  if (typeof x === 'string')
    return x;
  else
    _atd_bad_json('str', x)
}
