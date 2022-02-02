"""
Handwritten code that serves as a model for generated code.
"""

from typing import Any, Callable
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import json


def atd_missing_field(type_name: str, json_field_name: str):
    raise ValueError(f"missing field '{json_field_name}'"
                     f" in JSON object of type '{type_name}'")


def atd_type_mismatch(expected_type: str, json_value: Any):
    value_str = str(json_value)
    if len(value_str) > 200:
        value_str = value_str[:200] + '…'
    raise ValueError(f"incompatible JSON value where"
                     f" type '{expected_type}' was expected: '{value_str}'")


def atd_read_bool(x: Any) -> bool:
    if isinstance(x, bool):
        return x
    else:
        return atd_type_mismatch('bool', x)


def atd_read_int(x: Any) -> int:
    if isinstance(x, int):
        return x
    else:
        return atd_type_mismatch('int', x)


def atd_read_float(x: Any) -> float:
    if isinstance(x, (int, float)):
        return x
    else:
        return atd_type_mismatch('float', x)


def atd_read_string(x: Any) -> str:
    if isinstance(x, str):
        return x
    else:
        return atd_type_mismatch('str', x)


def atd_read_list(read_elt: Callable[[Any], Any]) \
        -> Callable[[List[Any]], List[Any]]:
    def read_list(elts: List[Any]) -> List[Any]:
        if isinstance(elts, list):
            return [read_elt(elt) for elt in elts]
        else:
            return atd_type_mismatch('list', elts)
    return read_list


def atd_read_nullable(read_elt: Callable[[Any], Any]) \
        -> Callable[[Optional[Any]], Optional[Any]]:
    def read_nullable(x: Any) -> Any:
        if x is None:
            return None
        else:
            return read_elt(x)
    return read_nullable


def atd_write_unit(x: Any) -> None:
    if x is None:
        return x
    else:
        return atd_type_mismatch('unit', x)


def atd_write_bool(x: Any) -> bool:
    if isinstance(x, bool):
        return x
    else:
        return atd_type_mismatch('bool', x)


def atd_write_int(x: Any) -> int:
    if isinstance(x, int):
        return x
    else:
        return atd_type_mismatch('int', x)


def atd_write_float(x: Any) -> float:
    if isinstance(x, (int, float)):
        return x
    else:
        return atd_type_mismatch('float', x)


def atd_write_string(x: Any) -> str:
    if isinstance(x, str):
        return x
    else:
        return atd_type_mismatch('str', x)


def atd_write_list(write_elt: Callable[[Any], Any]) \
        -> Callable[[List[Any]], List[Any]]:
    def write_list(elts: List[Any]) -> List[Any]:
        if isinstance(elts, list):
            return [write_elt(elt) for elt in elts]
        else:
            atd_type_mismatch('list', elts)
    return write_list


def atd_write_nullable(write_elt: Callable[[Any], Any]) \
        -> Callable[[Optional[Any]], Optional[Any]]:
    def write_nullable(x: Any) -> Any:
        if x is None:
            return None
        else:
            return write_elt(x)
    return write_nullable


class Root:
    def __init__(
            self,
            id: str,
            await_: bool,
            items: List[List[int]]
    ):
        self._id = id
        self._await = await_
        self._items = items

    def __repr__(self):
        return self.to_json_string()

    @property
    def id(self):
        return self._id

    @property
    def await_(self):
        return self._await

    @property
    def items(self):
        return self._items

    @classmethod
    def from_json(cls, x: Any):
        if isinstance(x, dict):
            if 'id' in x:
                id: str = atd_read_string(x['id'])
            else:
                atd_missing_field('Root', 'id')
            if 'await' in x:
                await_: bool = atd_read_bool(x['await'])
            else:
                atd_missing_field('Root', 'await')
            if 'items' in x:
                items: List[List[int]] = atd_read_list(atd_read_list(atd_read_int))(x['items'])
            else:
                atd_missing_field('Root', 'items')
        else:
            atd_type_mismatch('Root', x)
        return cls(id, await_, items)

    def to_json(self) -> Any:
        res: Dict[str, Any] = {}
        res['id'] = atd_write_string(self._id)
        res['await'] = atd_write_bool(self._await)
        res['items'] = atd_write_list(atd_write_list(atd_write_int))(self.items)
        return res

    @classmethod
    def from_json_string(cls, x: str):
        return cls.from_json(json.loads(x))

    def to_json_string(self) -> str:
        return json.dumps(self.to_json())
