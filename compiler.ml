open Llvm
open Low_level_tree
open Utils

(* If true, prints values as numbers rather than characters for debugging purposes *)
let numeric_output = false

let compile app =
  let context = global_context () in
  let the_module = create_module context "bf_program" in
  let i32 = i32_type context in
  let zero32 = const_null i32 in

  let main_fun = define_function "main" (function_type i32 [||]) the_module in
  let main_block = entry_block main_fun in
  let main_begin_builder = builder_at_end context main_block in

  let value_type = i32 in
  let tape_size = 1048576 in
  let tape_array = const_null (array_type value_type tape_size) in
  let tape_var = define_global "tape" tape_array the_module in
  let initial_tape_ptr = const_gep tape_var [|zero32; zero32|] in

  let putchar = declare_function "putchar" (function_type i32 [|i32|]) the_module in
  let printf = declare_function "printf" (function_type i32 [|pointer_type (i8_type context); i32|]) the_module in
  let printf_number_format = define_global "number_format" (const_stringz context "%d\n") the_module in
  let getchar = declare_function "getchar" (function_type i32 [||]) the_module in

  let print_value value builder =
    if numeric_output then
      let number_format = build_gep printf_number_format [|zero32; zero32|] "fmt" builder in
      build_call printf [|number_format; value|] "" builder
    else
      build_call putchar [|value|] "" builder
  in

  let read_value builder = build_call getchar [||] "read_char" builder in
  
  let compile_value value values = match value with
    | Const x -> const_int value_type x
    | Memory offset -> IntMap.find offset values
  in

  let rec compile_statement (values, tape_ptr, builder, bb) = function
    | Add (offset, x) ->
      let current_value = IntMap.find offset values in
      let summand = compile_value x values in
      let new_value = build_add current_value summand "new_value" builder in
      IntMap.add offset new_value values, tape_ptr, builder, bb
    | Set (offset, x) ->
      let new_value = compile_value x values in
      IntMap.add offset new_value values, tape_ptr, builder, bb
    | Move offset ->
      let new_tape_ptr = build_gep tape_ptr [|const_int i32 offset|] "new_tape_ptr" builder in
      IntMap.empty, new_tape_ptr, builder, bb
    | Store ->
      let store_value offset value =
        let ptr = build_gep tape_ptr [|const_int i32 offset|] "ptr" builder in
        ignore (build_store value ptr builder)
      in
      IntMap.iter store_value values;
      values, tape_ptr, builder, bb
    | Output value ->
      let value = compile_value value values in
      ignore (print_value value builder);
      values, tape_ptr, builder, bb
    | Input offset ->
      IntMap.add offset (read_value builder) values, tape_ptr, builder, bb
    | Loop body ->
      let _, tape_ptr, builder, bb = compile_loop false body 0 values tape_ptr builder bb in
      IntMap.empty, tape_ptr, builder, bb
    | BalancedLoop (offset, body) ->
      compile_loop true body offset values tape_ptr builder bb
    | Load offset ->
      let value_ptr = build_gep tape_ptr [|const_int i32 offset|] "value_ptr" builder in
      let value = build_load value_ptr "value" builder in
      IntMap.add offset value values, tape_ptr, builder, bb

  and compile_loop balanced body offset values tape_ptr builder bb =
      let loop_block = append_block context "loop_body" main_fun in
      let loop_builder = builder_at_end context loop_block in
      let after_loop_block = append_block context "after_loop" main_fun in
      let after_loop_builder = builder_at_end context after_loop_block in

      let check_loop_condition values builder =
        let current_value = IntMap.find offset values in
        let loop_condition = build_is_not_null current_value "loop_condition" builder in
        ignore (build_cond_br loop_condition loop_block after_loop_block builder)
      in
      check_loop_condition values builder;

      let make_phi value = build_phi [value, bb] "value_in_loop" loop_builder in
      let loop_values = if balanced then IntMap.map make_phi values else IntMap.empty in
      let loop_ptr = build_phi [tape_ptr, bb] "ptr_in_loop" loop_builder in

      let new_values, new_ptr, end_loop_builder, end_loop_block =
        compile_statements loop_values loop_ptr loop_builder loop_block body
      in

      IntMap.iter2 (fun _ loop_value new_value -> add_incoming (new_value, end_loop_block) loop_value) loop_values new_values;
      add_incoming (new_ptr, end_loop_block) loop_ptr;
      check_loop_condition new_values end_loop_builder;

      let make_phi current_value new_value =
        build_phi [current_value, bb; new_value, end_loop_block] "value_after_loop" after_loop_builder
      in
      let after_loop_values = if balanced then IntMap.map2 make_phi values new_values else IntMap.empty in
      let after_loop_ptr =
        build_phi [tape_ptr, bb; new_ptr, end_loop_block] "ptr_after_loop" after_loop_builder
      in
      after_loop_values, after_loop_ptr, after_loop_builder, after_loop_block

  and compile_statements current_value tape_ptr builder bb =
    List.fold_left compile_statement (current_value, tape_ptr, builder, bb)
  in
  let _, _, main_end_builder, _ =
    compile_statements IntMap.empty initial_tape_ptr main_begin_builder main_block app
  in
  ignore (build_ret zero32 main_end_builder);
  the_module, main_fun

let compile_file file =
  let code = Utils.read_file file in
  let app = Parser.parse code in
  let app = Low_level_tree.transform app in
  let app = Zero_optimizer.optimize app in
  let app = Constant_folding.optimize app in
  compile app
