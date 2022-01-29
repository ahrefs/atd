"""
Handwritten code that serves as a model for generated code.
"""

from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import json


def missing_field(type_name: str, json_field_name: str):
    raise ValueError(f"missing field '{json_field_name}'"
                     " in JSON object of type '{type_name}'")


def incorrect_type(expected_type: str, json_value: Any):
    value_str = str(json_value)
    if len(value_str) > 200:
        value_str = value_str[:200] + '…'

    raise ValueError(f"incompatible JSON value where"
                     " type '{expected_type}' was expected: '{value_str}'")


class Sample:
    def __init__(
            self,
            id_: str
    ):
        self._id = id_

    @property
    def id(self):
        return self._id

    @classmethod
    def from_json(cls, x: Any):
        if isinstance(x, dict):
            if 'id' in x:
                id_: str = x['id']
                return cls(id_)
            else:
                missing_field('Sample', 'id')
        else:
            incorrect_type('Sample', x)

    def to_json(self) -> Any:
        return {
            'id': self._id,
        }

    @classmethod
    def from_json_string(cls, x: str):
        return cls.from_json(json.loads(x))

    def to_json_string(self) -> str:
        return json.dumps(self.to_json())
