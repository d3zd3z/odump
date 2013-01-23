open Batteries

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)
