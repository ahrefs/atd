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


def type_mismatch(expected_type: str, json_value: Any):
    value_str = str(json_value)
    if len(value_str) > 200:
        value_str = value_str[:200] + '…'

    raise ValueError(f"incompatible JSON value where"
                     " type '{expected_type}' was expected: '{value_str}'")


class Sample:
    def __init__(
            self,
            id: str
    ):
        self._id = id

    @property
    def id(self):
        return self._id

    @classmethod
    def from_json(cls, x: Any):
        if isinstance(x, dict):
            if 'id' in x:
                id: str = x['id']
            else:
                missing_field('Sample', 'id')
        else:
            type_mismatch('Sample', x)
        return cls(id)

    def to_json(self) -> Any:
        return {
            'id': self._id,
        }

    @classmethod
    def from_json_string(cls, x: str):
        return cls.from_json(json.loads(x))

    def to_json_string(self) -> str:
        return json.dumps(self.to_json())
