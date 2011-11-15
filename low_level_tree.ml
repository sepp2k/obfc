open Utils

type memory_location = int

type value =
  | Memory of memory_location
  | Const of int

let string_of_memory offset = "ptr[" ^ string_of_int offset ^ "]"

type statement =
  | Set of memory_location * value
  | Add of memory_location * value
  | Move of memory_location
  | Output of value
  | Input of memory_location
  | Loop of statement list
  | BalancedLoop of memory_location * statement list
  | Load of memory_location
  | Store

let rec transform n move_at_end = function
  | [] ->
    if move_at_end then 
      if n <> 0 then [Store; Move n; Load 0] else [Store; Load 0]
    else []
  | Ast.MoveLeft :: rest -> transform (n-1) move_at_end rest
  | Ast.MoveRight :: rest -> transform (n+1) move_at_end rest
  | Ast.Loop body :: rest ->
    if Ast.is_balanced body then
      BalancedLoop (n, transform n false body) :: transform n move_at_end rest
    else
      let rest = Loop (transform 0 true body) :: transform 0 move_at_end rest in
      if n = 0 then Store :: rest else Store :: Move n :: rest
  | Ast.Increase :: rest -> Add (n, Const 1) :: transform n move_at_end rest
  | Ast.Decrease :: rest -> Add (n, Const (-1)) :: transform n move_at_end rest
  | Ast.Output :: rest -> Output (Memory n) :: transform n move_at_end rest
  | Ast.Input :: rest -> Input n :: transform n move_at_end rest

let rec insert_loads zeroed offsets previous statements =
  let rec find_offsets offsets = function
    | [] -> offsets
    | (Set (offset, Memory offset2) | Add (offset, Memory offset2)) :: rest ->
      find_offsets (IntSet.add offset2 (IntSet.add offset offsets)) rest 
    | (Set (offset, Const _) | Add (offset, Const _) | Output (Memory offset) | Input offset) :: rest ->
      find_offsets (IntSet.add offset offsets) rest
    | Output (Const _) :: rest -> find_offsets offsets rest
    | BalancedLoop (offset, body) :: rest ->
      let offsets = find_offsets (IntSet.add offset offsets) body in
      find_offsets offsets rest
    | _ -> failwith "Unexpected Move, Store, Load or Loop in BalancedLoop"
  in
  let make_loads offsets =
    let make_load offset =
      if zeroed then
        Set (offset, (Const 0))
      else
        Load offset
    in
    List.map make_load (IntSet.elements offsets)
  in
  match statements with
  | [] -> make_loads offsets @ List.rev previous
  | Move offset :: rest ->
    let rest = Move offset :: insert_loads false IntSet.empty [] rest in
    make_loads offsets @ List.rev previous @ rest
  | (Set (offset, Memory offset2) | Add (offset, Memory offset2)) as statement :: rest ->
    insert_loads zeroed (IntSet.add offset2 (IntSet.add offset offsets)) (statement::previous) rest
  | (Set (offset, Const _) | Add (offset, Const _) | Output (Memory offset) | Input offset) as statement :: rest ->
    insert_loads zeroed (IntSet.add offset offsets) (statement::previous) rest
  | (Output (Const _) | Load _ | Store) as statement :: rest ->
    insert_loads zeroed offsets (statement::previous) rest
  | BalancedLoop (offset, body) as statement :: rest ->
    let offsets = find_offsets (IntSet.add offset offsets) body in
    insert_loads zeroed offsets (statement :: previous) rest
  | Loop body :: rest ->
    let loop = Loop (insert_loads false IntSet.empty [] body) in
    let rest = loop :: insert_loads false IntSet.empty [] rest in
    make_loads (IntSet.add 0 offsets) @ List.rev previous @ rest

let transform statements = insert_loads true IntSet.empty [] (transform 0 false statements)
