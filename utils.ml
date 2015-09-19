let read_file file =
  let chan = open_in_bin file in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  buf

let print_ints ints =
  List.iter (Printf.printf "%d,") ints;
  print_newline ()

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
