open Low_level_tree
open Utils

type cached_value =
  | ConstIncrease of int
  | ConstValue of int
  | IncreasedVariable of memory_location * int

let add x = function
  | ConstIncrease y -> ConstIncrease (x+y)
  | ConstValue y -> ConstValue (x+y)
  | IncreasedVariable (offset, y) -> IncreasedVariable (offset, x+y)

let get_value memory = function
  | Const x -> `Const x
  | Memory offset ->
    if IntMap.mem offset memory then
      match IntMap.find offset memory with
        | ConstValue x -> `Const x
        | IncreasedVariable (offset2, 0) -> `Variable offset2
        | _ -> `Variable offset
    else
      `Variable offset

let realize memory offset =
  if IntMap.mem offset memory then
    match IntMap.find offset memory with
      | ConstIncrease x -> [Add (offset, Const x)]
      | ConstValue x -> [Set (offset, Const x)]
      | IncreasedVariable (offset2, x) ->
        [Set (offset, Memory offset2); Add (offset, Const x)]
  else
    []

let realize_all memory = List.concat (List.map (realize memory) (IntMap.keys memory))

let rec optimize memory = function
  | [] -> realize_all memory
  | Add (offset, value) :: rest ->
    let memory = match get_value memory value with
      | `Const x ->
        let new_value =
          if IntMap.mem offset memory then
            add x (IntMap.find offset memory)
          else
            ConstIncrease x
        in
        IntMap.add offset new_value memory
      | `Variable offset2 ->
        if IntMap.mem offset memory then
          match IntMap.find offset memory with
            | ConstValue x ->
              IntMap.add offset (IncreasedVariable (offset2, x)) memory
            | _ -> memory
        else
          memory
    in
    optimize memory rest
  | Set (offset, Memory offset2) :: rest ->
    optimize (IntMap.add offset (IncreasedVariable (offset2, 0)) memory) rest
  | Set (offset, Const x) :: rest ->
    optimize (IntMap.add offset (ConstValue x) memory) rest
  | Input offset :: rest ->
    Input offset :: optimize (IntMap.remove offset memory) rest
  | Output (Memory offset) :: rest ->
      (match get_value memory (Memory offset) with
        | `Const x -> Output (Const x) :: optimize memory rest
        | `Variable offset2 ->
          realize memory offset2 @ Output (Memory offset2) :: optimize memory rest)
  | Store :: rest ->
    let instructions = realize_all memory in
    instructions @ Store :: optimize IntMap.empty rest
  | Loop body :: rest ->
    realize_all memory @ Loop (optimize IntMap.empty body) :: optimize IntMap.empty rest
  | BalancedLoop (offset, body) :: rest ->
    realize_all memory @ BalancedLoop (offset, optimize IntMap.empty body) :: optimize IntMap.empty rest
  | statement :: rest ->
    statement :: optimize memory rest

let optimize = optimize IntMap.empty
