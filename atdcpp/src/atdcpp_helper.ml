(* Helper classes *)

open Atd.Import
open Atdcpp_env

(* TODO: Extract to to a plain file? *)

let output_atds env =
  fprintf env.output "\
#pragma once

#include <cstddef>
#include <string>
#include <vector>
#include <optional>

"
