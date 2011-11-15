open Parse_utils
open Ast

let parse str =
  let simple_statement char =
    match char with
    | '<' -> Some MoveLeft
    | '>' -> Some MoveRight
    | '+' -> Some Increase
    | '-' -> Some Decrease
    | '.' -> Some Output
    | ',' -> Some Input
    | _ -> None
  in
  let rec parse' pos result =
    if pos.index == String.length str
    then
      result, None
    else
      let char = str.[pos.index] in
      let next_pos = increment_position pos char in
      match simple_statement char with
      | Some statement ->
        parse' next_pos (statement :: result)
      | None ->
        match char with
        | '[' ->
          begin
            match parse' next_pos [] with
            | inner, Some pos_of_closing_bracket ->
              let pos_after_loop = increment_position pos_of_closing_bracket ']' in
              parse' pos_after_loop (Loop (List.rev inner) :: result)
            | _, None ->
              syntax_error "Unclosed '['" pos
          end
        | ']' -> result, Some pos
        | _ -> parse' next_pos result
  in
  match parse' zero_position [] with
  | statements, None -> List.rev statements
  | _, Some pos -> syntax_error "Unexpected ']'" pos
