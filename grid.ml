
type 'a list2d = 'a list list

type t = {
  grid: int list2d;
  n: int;
  score: int;
  move_id: int;
  game_mode: int;
}

(**
 * [col_to_row] Returns the nth entry of every list inside a [list2d]
*)
let rec col_to_row (grd: int list2d) nth (acc : int list) = match grd with
  | h::t -> col_to_row t nth (acc@[List.nth h nth])
  | [] -> acc

(** [get_nonzero] returns a list containing the non-zero elements of
    [lst]*)
let rec get_nonzero lst = List.filter (fun a -> a <> 0) lst

(** [count_zeros] counts the number of zeroes in the matrix [grd]*)
let rec count_zeros grd acc = 
  match grd with 
  | [] -> acc
  | h::t -> let amount = ((List.length h) - List.length (get_nonzero h)) in
    count_zeros t (acc+amount)

(** [choose_kth] returns a random integer from 0 (inclusive) to [count] 
    (exclusive)*)
let choose_kth count = Random.int count

(** [pick_rand_tile] returns an integer and is a helper function for pop_helper.
    It takes in a number and based on that value determines whether to return
    2 or 4. *)
let pick_rand_tile probability = 
  if probability <= 55 then 2
  else 4

(** [pick_rand_tile_hard] returns an integer and is a helper function for 
    pop_helper. It takes in a number and based on that value determines whether 
    to return 1, 2 or 4. *)
let pick_rand_tile_hard probability = 
  if probability <= 10 then 1
  else if probability <= 65 then 2 
  else 4

(** [pop_helper] is a helper function for [populate]. It takes list [row] and
    changes its [c]th zero to either a 2 or a 4. When [row] doesn't have
    a [c]th zero or [c] is negative, i.e. it contains less than [c] zeroes, 
    [pop_helper] returns [row]*)
let rec pop_helper row c num_zero mode =
  match row with 
  | [] -> []
  | h1::t1 -> 
    if c < 0 then h1::(pop_helper t1 c num_zero mode)
    else
      begin
        if h1 = 0 then 
          if ((num_zero+1) = c) then 
            if mode = 0 then (pick_rand_tile (Random.int 100))::t1
            else (pick_rand_tile_hard (Random.int 100))::t1
          else h1::(pop_helper t1 c (num_zero + 1) mode)
        else h1::(pop_helper t1 c num_zero mode)
      end

(** [populate] takes a matrix [grd] and replaces its [kth] zero
    with a 2 or a 4. This recursive function does this row-by-row,
    subtracting the number of zeroes from the next [kth] parameter.
    Obviously, [kth] might be negative, a case that [pop_helper] catches*)
let rec populate grd kth mode = 
  match grd with 
  | [] -> grd
  | h::t -> (pop_helper h (kth+1) 0 mode)::
            (populate t (kth- (List.length h 
                               - List.length (get_nonzero h))) mode)

(** [transpose] takes a matrix [grd] and returns its transpose, or
    a new matrix with columns equal to the rows of [grd] *)
let rec transpose grd acc n count = 
  if count = n then acc else 
    let column = col_to_row grd count [] in
    transpose grd (acc@[column]) n (count+1)

(** [smash_nums] takes a list [lst] of non-zero integers and returns 
    the same list except with adjacent, disjoint, equal pairs of numbers 
    removed and replaced by a single number, the sum of the pair. By disjoint, 
    we mean [2;2;2] only has one pair, not two, since the first two elements
    and the last two elements share the middle element. For example,
    [smash_nums [2;2;4;] []] returns [4;4]. Note that the resulting 4 and
    the original 4 do not get smashed together. To do this you have to call
    [smash_nums] again. *)
let rec smash_nums lst acc =
  match lst with 
  | [] -> acc 
  | [h] -> acc@[h]
  | h1::h2::t -> 
    if h1 = h2 then 
      begin
        if h1 = 1 then smash_nums (h2::t) (acc@[h1])
        else smash_nums t (acc@[h1*2])
      end
    else smash_nums (h2::t) (acc@[h1])

(** [reduce_to_nonzero] takes a matrix [grd] and returns a list of lists,
    comprised of the rows of [grd] with zeroes removed*)
let reduce_to_nonzero grd = List.map (fun x -> get_nonzero x) grd


(** [calculate_score] takes a list [lst] and accumulates the values of
    all equal, disjoint, adjacent integer pairs. For example, [2;2;2] is counted
    as one pair, not two. The score of this list is 4, not 8. [2;4;2] 
    is scored 0 since no pairs have equal elements*)
let rec calculate_score lst score = 
  match lst with 
  | [] -> score
  | [h] -> score
  | h1::h2::t -> 
    if h1 = h2 then calculate_score t (score + h1*2)
    else calculate_score (h2::t) score

(** [fill_list_left_up] pads a list of integers [nums] with enough zeroes to the
    right to return an [n]-length list*)
let rec fill_list_left_up nums n = 
  let length = List.length nums in
  if n = length then nums else fill_list_left_up (nums@[0]) n 

(** [fill_list_right_down] pads a list of integers [nums] with enough zeroes to 
    the left to return an [n]-length list*)
let rec fill_list_right_down nums n =  
  let length = List.length nums in
  if n = length then nums else fill_list_right_down (0::nums) n

(** [shift_grid_lr] takes each row of an [n] by [n] matrix [grd], takes its
    non-zero entries, "smashes" them (see smash_nums), and pads the list with
    zeroes to the left or right depending on [dir]. When [dir = "right"] we
    actually reverse the list of non-zero values before smashing and reverse
    it again after smashing, so the right-most elements get smashed first. For
    example, shifting [2;2;2] left should return [4;2;0], and shifting the same
    input right should return [0;2;4]. *)
let rec shift_grid_lr grd dir acc n = 
  match grd with 
  | [] -> acc 
  | h::t -> 
    let nonzero = h |> get_nonzero in 
    let padded = 
      begin
        match dir with 
        | "left" -> 
          (smash_nums nonzero [] |> 
           fill_list_left_up) n
        | "right" ->
          ((List.rev nonzero |> smash_nums) [] |> List.rev |>
           fill_list_right_down) n
        | _ -> failwith "Invalid direction"
      end
    in shift_grid_lr t dir (acc@[padded]) n

(** [shift_grid_ud] essentially takes the transpose of matrix [grd] and
    calls [shift_grid_lr] on it based on direction [dir]. Then we return the
    transpose of the result. This is based on the idea that shifting up or down 
    is the same as shifting columns "left" or "right" respectively.*)
let shift_grid_ud grd dir n =
  let trans = transpose grd [] n 0 in 
  let shifted =
    match dir with 
    | "up" -> shift_grid_lr trans "left" [] n
    | "down" -> shift_grid_lr trans "right" [] n
    | _ -> failwith "Invalid direction"
  in transpose shifted [] n 0

(** [shift] calls [shift_grid_lr] or [shift_grid_ud] on [grd] based on
    direction [dir]*)
let shift grd dir =
  let n =
    match grd with 
    | [] -> failwith "empty grid"
    | h::t -> List.length h
  in 
  match dir with 
  | "up"|"down" -> shift_grid_ud grd dir n
  | "left"|"right" -> shift_grid_lr grd dir [] n 
  | _ -> failwith "Invalid direction"

let transition state dir = 
  let trans = transpose state.grid [] state.n 0 in
  let new_grid = shift state.grid dir in 
  let non_zeros = count_zeros new_grid 0 in
  let kth = if non_zeros = 0 then 0 else Random.int non_zeros in
  let new_move_id = state.move_id + 1 in
  match dir with 
  | "up" | "down" -> 
    let score =
      List.fold_right 
        (fun x y -> (calculate_score x 0) + y) (reduce_to_nonzero trans) 0 in 
    {
      grid = (populate new_grid kth state.game_mode); 
      n = state.n; 
      score = state.score + score; 
      move_id = new_move_id;
      game_mode = state.game_mode
    }
  | "left" | "right" ->
    let score = 
      List.fold_right 
        (fun x y -> calculate_score x 0 + y) (reduce_to_nonzero state.grid) 0 in 
    {grid = (populate new_grid kth state.game_mode); 
     n = state.n; 
     score = state.score + score; 
     move_id = new_move_id;
     game_mode = state.game_mode}
  | _ -> failwith "Invalid input"

(** [print_line] recursively prints the dashed string n amount of times to form
    the grid *)
let rec print_line n =
  print_string "--------";
  match n with
  | 1 -> ()
  | _ -> print_line (n-1)

(** [print_color] prints each specified number in the grid in different colors*)
let print_color x= match x with  
  | 1 -> ANSITerminal.(print_string [red;Bold] "X")
  | 2 -> ANSITerminal.(print_string [blue;] "2")
  | 4 -> ANSITerminal.(print_string [yellow;] "4")
  | 8 -> ANSITerminal.(print_string [white;] "8")
  | 16 -> ANSITerminal.(print_string [magenta;] "16")
  | 32 -> ANSITerminal.(print_string [green] "32")
  | 64 -> ANSITerminal.(print_string [yellow;Bold] "64")
  | 128 -> ANSITerminal.(print_string [blue;Bold] "128")
  | 256 -> ANSITerminal.(print_string [cyan;Bold] "256")
  | 512 -> ANSITerminal.(print_string [green;Bold] "512")
  | 1024 -> ANSITerminal.(print_string [cyan;Bold;on_white] "1024")
  | 2048 -> ANSITerminal.(print_string [white;Bold;on_red] "2048")
  | 4096 -> ANSITerminal.(print_string [white;Bold;on_green] "4096")
  | 8192 -> ANSITerminal.(print_string [cyan;Bold;on_black] "8192")
  | x -> ANSITerminal.(print_string [cyan;Bold] (string_of_int x))

(** [print_int_line] prints integer [x] and a space *)
let print_int_line x =
  if x = 0 then print_char ' ' else print_color x;
  print_string "\t|"

(** [print_list] takes an int list [list] and prints all of
    its entries separated by spaces *)
let print_list lst = 
  print_newline ();
  print_string "|";
  List.iter (print_int_line) lst;
  print_newline ();
  print_string "-";
  print_line (List.length lst)

let print_grid (state:t) =
  print_string "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
  \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
  \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nScore: ";
  print_int state.score;
  print_string "\t\t moves: ";
  print_int state.move_id;
  print_newline ();
  print_line state.n;
  print_string "-";
  List.iter print_list state.grid;
  print_newline ()

(** [create_list] returns an [n]-length list filled with zeroes*)
let rec create_list n acc =
  if n <= 0 then acc
  else create_list (n-1) (0::acc)

(** [create_grid] returns an [n]x[n] 2-dimensional list filled with zeroes*)
let rec create_grid x n (acc : int list2d) =
  if x <= 0 then acc
  else  create_grid (x-1) n ((create_list n [])::acc)

let create_game n_ mode = 
  transition {
    grid = create_grid n_ n_ [];
    n = n_;
    score = 0;
    move_id = 0;
    game_mode = mode
  } "up"

let get_grid (state:t) : (int list2d) = state.grid

let get_score (state:t) : int = state.score

let get_size (state:t) : int = state.n

let get_move_id (state:t) : int = state.move_id

let game_over state =
  get_grid state = get_grid (transition state "up") &&
  get_grid state = get_grid (transition state "down")&&
  get_grid state = get_grid (transition state "right")&&
  get_grid state = get_grid (transition state "left")

let get_state gr size s mid mode = 
  {grid = gr; n = size; score = s; move_id = mid; 
   game_mode = mode}

let get_mode (state:t) : int = state.game_mode
