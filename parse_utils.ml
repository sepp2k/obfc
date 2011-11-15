type position = {line_number: int; column_number: int; index: int}

let zero_position = {line_number = 0; column_number = 0; index = 0}

let pos_to_string pos = "line " ^ string_of_int (pos.line_number + 1) ^
                        ", column " ^ string_of_int (pos.column_number + 1)

let tab_width = 8

let add_tab idx = idx + tab_width - (idx mod tab_width)

let increment_position pos char =
  let new_pos = {
    index = pos.index + 1;
    column_number = pos.column_number + 1;
    line_number = pos.line_number
  } in
  match char with
  | '\n' -> {new_pos with line_number = pos.line_number + 1; column_number = 0}
  | '\t' -> {new_pos with column_number = add_tab pos.column_number}
  | _ ->  new_pos

let syntax_error error pos =
  failwith ("Syntax Error: " ^ error ^ " on " ^ pos_to_string pos)
