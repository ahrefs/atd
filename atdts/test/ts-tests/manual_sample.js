"use strict";
// Handwritten code that serves as a model for generated code.
exports.__esModule = true;
exports.KindToJSON = exports.KindFromJSON = void 0;
function KindFromJSON(x) {
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
exports.KindFromJSON = KindFromJSON;
function KindToJSON(x) {
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
exports.KindToJSON = KindToJSON;
// Test the code above
function main(x) {
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
console.log(KindFromJSON(KindToJSON({ kind: 'Thing', value: 42 })));
