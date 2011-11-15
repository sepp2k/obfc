open Low_level_tree

let rec optimize app = List.map optimize_statement app

and optimize_statement = function
  | BalancedLoop (offset, [Add (offset2, Const (-1))]) as statement ->
    if offset = offset2 then
      Set (offset, Const 0)
    else
      begin
        prerr_endline "Warning: Infinite loop detected";
        statement
      end
  | BalancedLoop (offset, body) -> BalancedLoop (offset, optimize body)
  | Loop body -> Loop (optimize body)
  | statement -> statement
