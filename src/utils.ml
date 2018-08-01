let product l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)
              (* Zip two lists (possibly unequal lengths) into a tuple *)

let rec zip lst1 lst2 = match lst1,lst2 with
  | [],[] -> []
  | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)
  | _ -> failwith "Error 002: Mismatching list lengths in zip"

let rec dummyl size =
  match size with
  | 0 ->
     []
  | _ ->
     () :: dummyl (size - 1)

let open_file_in_dir dir file =
  if not (Sys.file_exists dir)
  then
    Unix.mkdir dir 0o777
  else 
    if not (Sys.is_directory dir)
    then
      failwith ("Error 036: Failed trying to create directory " ^ dir ^ " file exists instead.");
  open_out (Printf.sprintf "%s/%s" dir file)

let rec cartesian_int ll acc = 
  match ll with 
  | [] -> acc
  | l :: ll_rest ->
     let acc_new =
       List.map 
         (fun elem -> 
           List.map 
             (fun acc_l -> elem :: acc_l)
             acc
         )
         l 
       |> List.flatten
     in
     cartesian_int ll_rest acc_new

let cartesian (l: 'a list list) =   
  cartesian_int l [[]]
  |> List.map List.rev

