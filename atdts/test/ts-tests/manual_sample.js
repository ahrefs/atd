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
_atd_missing_json_field('a', 'b');
function _atd_missing_json_field(type_name, json_field_name) {
    throw new Error("missing field '".concat(json_field_name, "'") +
        " in JSON object of type '".concat(type_name, "'"));
}
function _atd_bad_json(expected_type, json_value) {
    var value_str = String(json_value);
    if (value_str.length > 200)
        value_str = value_str.substring(0, 200) + '…';
    throw new Error("incompatible JSON value where" +
        " type '".concat(expected_type, "' was expected: '").concat(value_str, "'"));
}
function _atd_bad_ts(expected_type, json_value) {
    var value_str = String(json_value);
    if (value_str.length > 200)
        value_str = value_str.substring(0, 200) + '…';
    throw new Error("incompatible TypeScript value where" +
        " type '".concat(expected_type, "' was expected: '").concat(value_str, "'"));
}
function _atd_read_unit(x) {
    if (x === null)
        return x;
    else
        _atd_bad_json('unit', x);
}
function _atd_read_bool(x) {
    if (typeof x === 'boolean')
        return x;
    else
        _atd_bad_json('bool', x);
}
function _atd_read_int(x) {
    if (Number.isInteger(x))
        return x;
    else
        _atd_bad_json('integer', x);
}
function _atd_read_float(x) {
    if (isFinite(x))
        return x;
    else
        _atd_bad_json('number', x);
}
function _atd_read_string(x) {
    if (typeof x === 'string')
        return x;
    else
        _atd_bad_json('str', x);
}
