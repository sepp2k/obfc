let read_file file =
  let chan = open_in_bin file in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  buf

let print_ints ints =
  List.iter (Printf.printf "%d,") ints;
  print_newline ()

(** Given the path of a Brainfuck file and the extension (including the '.') for
    the result, returns the path of the result file that will be generated.
    If the source file has an extension, the extension will be replaced by
    the result extension. Otherwise the result extension will be simply appended
    to the file name.
*)
let result_path source_path result_extension =
  (* Separate base- and dirname, so that a '.' in a directory name does not
     mess up the result (i.e. something like "a/b.c/d" would be "a/b.bc" instead
     of "a/b.c/d.bc" if we didn't do this) *)
  let dirname = Filename.dirname source_path in
  let source_name = Filename.basename source_path in
  let name =
    if String.contains source_name '.'
    then Filename.chop_extension source_name
    else source_name
  in
  String.concat "" [dirname; Filename.dir_sep; name; result_extension]

module IntCompare = struct
  let compare = Pervasives.compare
  type t = int
end

module IntSet = Set.Make(IntCompare)

module IntMap = struct
  include Map.Make(IntCompare)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let iter2 f m1 m2 = iter (fun k v -> f k v (find k m2)) m1
  let mapi2 f m1 m2 = mapi (fun k v -> f k v (find k m2)) m1
  let map2 f m1 m2 = mapi2 (fun _ v1 v2 -> f v1 v2) m1 m2
end
