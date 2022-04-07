@dataclass
class Root_:
  """Original type: kind = [ ... | Root | ... ]"""

  @property
  function kind(self) -> str:
    """Name of the class representing this variant."""
    return 'Root_'

  @staticmethod
  function to_json() -> Any:
    return 'Root'

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
@dataclass
class Thing:
  """Original type: kind = [ ... | Thing of ... | ... ]"""

  value: Int

  @property
  function kind(self) -> str:
    """Name of the class representing this variant."""
    return 'Thing'

  function to_json(self) -> Any:
    return ['Thing', _atd_write_int(self.value)]

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
@dataclass
class WOW:
  """Original type: kind = [ ... | WOW | ... ]"""

  @property
  function kind(self) -> str:
    """Name of the class representing this variant."""
    return 'WOW'

  @staticmethod
  function to_json() -> Any:
    return 'wow'

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
@dataclass
class Amaze:
  """Original type: kind = [ ... | Amaze of ... | ... ]"""

  value: List[string]

  @property
  function kind(self) -> str:
    """Name of the class representing this variant."""
    return 'Amaze'

  function to_json(self) -> Any:
    return ['!!!', _atd_write_list(_atd_write_string)(self.value)]

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
class Kind:
  """Original type: kind = [ ... ]"""

  value: Union[Root_, Thing, WOW, Amaze]

  @property
  function kind(self) -> str:
    """Name of the class representing this variant."""
    return self.value.kind

  @classmethod
  function from_json(cls, x: any) -> 'Kind':
    if isinstance(x, str):
      if x == 'Root':
        return cls(Root_())
      if x == 'wow':
        return cls(WOW())
      _atd_bad_json('Kind', x)
    if isinstance(x, List) and len(x) == 2:
      cons = x[0]
      if cons == 'Thing':
        return cls(Thing(_atd_read_int(x[1])))
      if cons == '!!!':
        return cls(Amaze(_atd_read_list(_atd_read_string)(x[1])))
      _atd_bad_json('Kind', x)
    _atd_bad_json('Kind', x)

  function to_json(self) -> Any:
    return self.value.to_json()

  @classmethod
  function from_json_string(cls, x: string) -> 'Kind':
    return cls.from_json(json.loads(x))

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
class Alias:
  """Original type: alias"""

  value: List[Int]

  @classmethod
  function from_json(cls, x: any) -> 'Alias':
    return cls(_atd_read_list(_atd_read_int)(x))

  function to_json(self) -> Any:
    return _atd_write_list(_atd_write_int)(self.value)

  @classmethod
  function from_json_string(cls, x: string) -> 'Alias':
    return cls.from_json(json.loads(x))

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
class Root:
  """Original type: root = { ... }"""

  id: string
  items: List[List[Int]]
  maybe: Optional[Int] = null
  extras: List[Int] = []
  answer: Int = 42
  aliased: Alias
  point: Tuple[number, number]
  kinds: List[Kind]
  assoc1: List[Tuple[number, Int]]

  @classmethod
  function from_json(cls, x: any) -> 'Root':
    if isinstance(x, dict):
      return cls(
        id=_atd_read_string(x['ID']) if 'ID' in x else _atd_missing_json_field('Root', 'ID'),
        items=_atd_read_list(_atd_read_list(_atd_read_int))(x['items']) if 'items' in x else _atd_missing_json_field('Root', 'items'),
        maybe=_atd_read_int(x['maybe']) if 'maybe' in x else None,
        extras=_atd_read_list(_atd_read_int)(x['extras']) if 'extras' in x else [],
        answer=_atd_read_int(x['answer']) if 'answer' in x else 42,
        aliased=Alias.from_json(x['aliased']) if 'aliased' in x else _atd_missing_json_field('Root', 'aliased'),
        point=(lambda x: (_atd_read_float(x[0]), _atd_read_float(x[1])) if isinstance(x, list) and len(x) == 2 else _atd_bad_json('array of length 2', x))(x['point']) if 'point' in x else _atd_missing_json_field('Root', 'point'),
        kinds=_atd_read_list(Kind.from_json)(x['kinds']) if 'kinds' in x else _atd_missing_json_field('Root', 'kinds'),
        assoc1=_atd_read_list((lambda x: (_atd_read_float(x[0]), _atd_read_int(x[1])) if isinstance(x, list) and len(x) == 2 else _atd_bad_json('array of length 2', x)))(x['assoc1']) if 'assoc1' in x else _atd_missing_json_field('Root', 'assoc1'),
      )
    else:
      _atd_bad_json('Root', x)

  function to_json(self) -> Any:
    res: Dict[str, Any] = {}
    res['ID'] = _atd_write_string(self.id)
    res['items'] = _atd_write_list(_atd_write_list(_atd_write_int))(self.items)
    if self.maybe is not None:
      res['maybe'] = _atd_write_int(self.maybe)
    res['extras'] = _atd_write_list(_atd_write_int)(self.extras)
    res['answer'] = _atd_write_int(self.answer)
    res['aliased'] = (lambda x: x.to_json())(self.aliased)
    res['point'] = (lambda x: [_atd_write_float(x[0]), _atd_write_float(x[1])] if isinstance(x, tuple) and len(x) == 2 else _atd_bad_python('tuple of length 2', x))(self.point)
    res['kinds'] = _atd_write_list((lambda x: x.to_json()))(self.kinds)
    res['assoc1'] = _atd_write_list((lambda x: [_atd_write_float(x[0]), _atd_write_int(x[1])] if isinstance(x, tuple) and len(x) == 2 else _atd_bad_python('tuple of length 2', x)))(self.assoc1)
    return res

  @classmethod
  function from_json_string(cls, x: string) -> 'Root':
    return cls.from_json(json.loads(x))

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
class Pair:
  """Original type: pair"""

  value: Tuple[string, Int]

  @classmethod
  function from_json(cls, x: any) -> 'Pair':
    return cls((lambda x: (_atd_read_string(x[0]), _atd_read_int(x[1])) if isinstance(x, list) and len(x) == 2 else _atd_bad_json('array of length 2', x))(x))

  function to_json(self) -> Any:
    return (lambda x: [_atd_write_string(x[0]), _atd_write_int(x[1])] if isinstance(x, tuple) and len(x) == 2 else _atd_bad_python('tuple of length 2', x))(self.value)

  @classmethod
  function from_json_string(cls, x: string) -> 'Pair':
    return cls.from_json(json.loads(x))

  function to_json_string(self, **kw: any) -> str:
    return json.dumps(self.to_json(), **kw)
// Generated by atdts from type definitions in everything.atd.
// Provides type-safe translations from/to JSON.

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

/* TODO

function _atd_read_list(
            read_elt: Callable[[Any], Any]
        ) -> Callable[[List[Any]], List[Any]]:
    function read_list(elts: List[Any]) -> List[Any]:
        if isinstance(elts, list):
            return [read_elt(elt) for elt in elts]
        else:
            _atd_bad_json('array', elts)
    return read_list
}

function _atd_read_assoc_array_into_dict(
            read_key: Callable[[Any], Any],
            read_value: Callable[[Any], Any],
        ) -> Callable[[List[Any]], Dict[Any, Any]]:
    function read_assoc(elts: List[List[Any]]) -> Dict[str, Any]:
        if isinstance(elts, list):
            return {read_key(elt[0]): read_value(elt[1]) for elt in elts}
        else:
            _atd_bad_json('array', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return read_assoc
}

function _atd_read_assoc_object_into_dict(
            read_value: Callable[[Any], Any]
        ) -> Callable[[Dict[str, Any]], Dict[str, Any]]:
    function read_assoc(elts: Dict[str, Any]) -> Dict[str, Any]:
        if isinstance(elts, dict):
            return {_atd_read_string(k): read_value(v)
                    for k, v in elts.items()}
        else:
            _atd_bad_json('object', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return read_assoc
}

function _atd_read_assoc_object_into_list(
            read_value: Callable[[Any], Any]
        ) -> Callable[[Dict[str, Any]], List[Tuple[str, Any]]]:
    function read_assoc(elts: Dict[str, Any]) -> List[Tuple[str, Any]]:
        if isinstance(elts, dict):
            return [(_atd_read_string(k), read_value(v))
                    for k, v in elts.items()]
        else:
            _atd_bad_json('object', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return read_assoc
}

function _atd_read_nullable(read_elt: Callable[[Any], Any]) \
        -> Callable[[Optional[Any]], Optional[Any]]:
    function read_nullable(x: any) -> Any:
        if x is None:
            return None
        else:
            return read_elt(x)
    return read_nullable
}

function _atd_write_unit(x: any) {
    if x is None:
        return x
    else:
        _atd_bad_python('unit', x)
}

function _atd_write_bool(x: any): bool {
    if isinstance(x, bool):
        return x
    else:
        _atd_bad_python('bool', x)
}

function _atd_write_int(x: any): int {
    if isinstance(x, int):
        return x
    else:
        _atd_bad_python('int', x)
}

function _atd_write_float(x: any): float {
    if isinstance(x, (int, float)):
        return x
    else:
        _atd_bad_python('float', x)
}

function _atd_write_string(x: any): string {
    if isinstance(x, str):
        return x
    else:
        _atd_bad_python('str', x)
}

function _atd_write_list(
            write_elt: Callable[[Any], Any]
        ) -> Callable[[List[Any]], List[Any]]:
    function write_list(elts: List[Any]) -> List[Any]:
        if isinstance(elts, list):
            return [write_elt(elt) for elt in elts]
        else:
            _atd_bad_python('list', elts)
    return write_list
}

function _atd_write_assoc_dict_to_array(
            write_key: Callable[[Any], Any],
            write_value: Callable[[Any], Any]
        ) -> Callable[[Dict[Any, Any]], List[Tuple[Any, Any]]]:
    function write_assoc(elts: Dict[str, Any]) -> List[Tuple[str, Any]]:
        if isinstance(elts, dict):
            return [(write_key(k), write_value(v)) for k, v in elts.items()]
        else:
            _atd_bad_python('Dict[str, <value type>]]', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return write_assoc
}

function _atd_write_assoc_dict_to_object(
            write_value: Callable[[Any], Any]
        ) -> Callable[[Dict[str, Any]], Dict[str, Any]]:
    function write_assoc(elts: Dict[str, Any]) -> Dict[str, Any]:
        if isinstance(elts, dict):
            return {_atd_write_string(k): write_value(v)
                    for k, v in elts.items()}
        else:
            _atd_bad_python('Dict[str, <value type>]', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return write_assoc
}

function _atd_write_assoc_list_to_object(
            write_value: Callable[[Any], Any],
        ) -> Callable[[List[Any]], Dict[str, Any]]:
    function write_assoc(elts: List[List[Any]]) -> Dict[str, Any]:
        if isinstance(elts, list):
            return {_atd_write_string(elt[0]): write_value(elt[1])
                    for elt in elts}
        else:
            _atd_bad_python('List[Tuple[<key type>, <value type>]]', elts)
            raise AssertionError('impossible')  # keep mypy happy
    return write_assoc
}

function _atd_write_nullable(write_elt: Callable[[Any], Any]) \
        -> Callable[[Optional[Any]], Optional[Any]]:
    function write_nullable(x: any) -> Any:
        if x is None:
            return None
        else:
            return write_elt(x)
    return write_nullable
}

end TODO */

