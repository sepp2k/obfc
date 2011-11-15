open Low_level_tree

let string_of_value = function
  | Memory offset -> string_of_memory offset
  | Const x -> string_of_int x

let rec string_of_statement = function
  | Set (offset, value) -> string_of_memory offset ^ " = " ^ string_of_value value ^ ";\n"
  | Add (offset, value) -> string_of_memory offset ^ " += " ^ string_of_value value ^ ";\n"
  | Move offset -> "ptr += " ^ string_of_int offset ^ ";\n"
  | Output value -> "putchar( " ^ string_of_value value ^ " );\n"
  | Input offset -> string_of_memory offset ^ " = getchar();\n"
  | Loop body -> "while(*ptr) {\n" ^ string_of_statements body ^ "}\n"
  | BalancedLoop (offset, body) -> "while(" ^ string_of_memory offset ^ ") {\n" ^ string_of_statements body ^ "}\n"
  | Load offset -> "load(" ^ string_of_memory offset ^ ");\n"
  | Store -> "store();\n"

and string_of_statements statements =
  String.concat "" (List.map string_of_statement statements)

let print statements = print_string (string_of_statements statements)
