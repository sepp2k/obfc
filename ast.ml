type statement =
  | MoveLeft
  | MoveRight
  | Increase
  | Decrease
  | Output
  | Input
  | Loop of statement list

let rec is_balanced n = function
  | [] -> n = 0
  | MoveLeft :: rest -> is_balanced (n-1) rest
  | MoveRight :: rest -> is_balanced (n+1) rest
  | Loop body :: rest -> is_balanced n body && is_balanced n rest
  | (Increase | Decrease | Output | Input ) :: rest -> is_balanced n rest

let is_balanced = is_balanced 0
