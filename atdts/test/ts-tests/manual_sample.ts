// Handwritten code that serves as a model for generated code.

// This type alias documents extra validation when converting from/to JSON
export type Int = number

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

export interface Root {
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
