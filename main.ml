open Printf

(** [move] represents the different player inputs: valid ([Move]), 
    invalid ([BadInput]), or a quit request ([Quit]). *)
type move = Move of string | Quit | BadInput | Help | Undo 

(** [parse_move] categorizes an input [str] into a type of [move]. The
    only valid game moves are WASD commands or a quit.*)
let parse_move str = 
  match str with
  | "w" | "W" -> Move "up"
  | "a" | "A" -> Move "left"
  | "s" | "S" -> Move "down"
  | "d" | "D" -> Move "right"
  | "quit" | "Quit" -> Quit
  | "help" | "Help" -> Help
  | "undo" | "Undo" -> Undo
  | _ -> BadInput

(** [play_again] takes the user input to determine whether to start a new game 
    or not. The only valid inputs are y,Y, n, or N. *)
let rec play_again () = 
  print_endline "Do you want to play again? (y/n)";
  print_string "> ";
  match (read_line ()) with 
  | "y" | "Y" -> true
  | "n" | "N" -> false
  | _ -> play_again ()

(** [read_dim] takes in the grid dimension the user enter. The valid input
    will be a positive integer. *)
let rec read_dim () = 
  match read_line () with 
  | "2" -> 2 | "3" -> 3 | "4" -> 4 | "5" -> 5
  | _ -> print_endline "Please enter a value between 2 and 5.";
    print_string "> ";
    read_dim () 

(** [read_mode] takes in the difficulty level the user enters. The valid inputs
    are the strings normal and hard. *)
let rec read_mode () = 
  match read_line () with 
  | "normal" | "Normal" -> 0
  | "hard" | "Hard" -> 1
  | _ -> print_endline "Sorry, that's an invalid game mode!"; 
    read_mode ()

(** [init_game] initializes the game by reading the inputs the user writes 
    and providing the instructions*)
let init_game () = 
  print_endline ("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
  \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
  \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n ══════════════ Welcome to 2048 ☺"^
                 "  ══════════════ 
  \n\nPlease specify the tile dimensions of the board (between 2 and 5.)");
  print_string "> ";
  let n = read_dim () in
  print_endline "Which mode? (normal/hard) Normal mode is classic 2048. 
Hard mode introduces an uncombinable block every once in a while.";
  print_string "> ";
  let mode = read_mode () in
  let init_state = Grid.create_game n mode in 
  print_endline "Please enter a move (<w> to move up, 
<a> to move left, <s> to move down, or <d> to move right). 
To see the help menu, enter <help>. To quit, enter <quit>.
Undo a move by typing <undo>.
 \n";
  Grid.print_grid init_state;
  init_state

(** [init_game_user] takes in the user input to be used when displaying the
    scoreboard *)
let rec init_game_user () = 
  print_endline "What's your username?";
  print_string "> ";
  let user = read_line () in 
  if user = "" then 
    let p = print_endline ("You entered a blank username!"^
                           " Enter something else.") in
    init_game_user ()
  else if String.contains user '_' then 
    let p = print_endline ("You can't use a name with underscores!"^
                           " Enter something else."); in
    init_game_user ()
  else user

(** [lose] prints the signal that the game ended, along with the current
    score [x]. *)
let lose x = 
  print_string "You don't have any moves left! Here's your score: ";
  print_int x;
  print_newline (); 
  play_again

(** [update_scores] updates scores based on game mode chosen and saves the
    that particular game info into a text file *)
let update_scores user score dim mode = 
  let dimension = string_of_int dim in 
  let game_mode = if mode = 0 then "Normal" else "Hard" in 
  let new_score = string_of_int score in 
  let string =  (dimension^"_"^game_mode^"_"^new_score^"_"^user) in 
  let open_channel = 
    open_out_gen [Open_append; Open_creat] 0o666 "hiscores.txt" in 
  fprintf open_channel "%s\n" string;
  close_out open_channel

(** [list_scores] appends the score to the list of scores *)
let rec list_scores acc ic = 
  match (input_line ic) with 
  | exception (End_of_file) -> (close_in ic); acc
  | x -> list_scores (x::acc) ic 

(** [round] is a record containing the statistics of a finished game *)
type round = {
  dim: string;
  mode: string;
  score: string;
  user: string;
}

(** [split_round] splits the string on the underscore *)
let rec split_round str = String.split_on_char '_' str

(** [record_round] takes the information, splits it, and sets game state values
    to these strings *)
let record_round lst = 
  match lst with 
  | h1::h2::h3::h4::t -> {dim = h1; mode = h2; score = h3; user = h4}
  | _ -> failwith "Parsing failed-- bad username input?"

(** [record_list] makes a list with all these records for each games consisting 
    of all its state values *)
let rec record_list lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> 
    let split = split_round h in 
    let round = record_round split in 
    record_list t (round::acc)

(** [print_record] formats how the game record information will be displayed *)
let print_record round = 
  print_endline ("Game type: "^round.dim^" by "^round.dim^" "^round.mode);
  print_endline ("Score: "^round.score);
  print_endline ("User: "^round.user^"\n")

(** [show_scores] displays the scores saved in the text file *)
let show_scores = 
  let ic = open_in "hiscores.txt" in 
  let lst = list_scores [] ic in 
  print_endline "\n =========SCORE HISTORY========= \n";
  List.iter print_record (record_list lst []);
  print_endline " =============================== \n"


(**[get_move] recursively prompts user for input until a valid input
   is entered (either w, a, s, or d). Also prints current grid and score.
   Valid inputs incur transitions in the game (shifts). Loops until
   player cannot make any more moves or the player signals to quit. *)
let rec get_move state prev user () =
  print_string  "> ";
  match (parse_move (read_line ())) with 
  | BadInput -> print_endline "Please enter a valid move.";
    get_move state prev user ()
  | Move a -> 
    let new_state = Grid.transition state a in 
    if (Grid.game_over state) then 
      let score = Grid.get_score state in 
      let dim = Grid.get_size state in 
      let mode = Grid.get_mode state in 
      update_scores user score dim mode;
      if (lose score ()) then
        let newgame = (init_game ()) in
        get_move newgame newgame user ()
      else exit 0
    else
      if Grid.get_grid new_state = Grid.get_grid state then
        (Grid.print_grid state;
        print_endline "Please enter a valid move.";
        get_move state prev user ())
      else
        (Grid.print_grid new_state;
        get_move new_state state user ())
  | Help -> let str = "Please enter a move (<w> to move up, 
<a> to move left, <s> to move down, or <d> to move right). 
To see the help menu, enter <help>. To quit, enter <quit>.
Undo a move by typing <undo>.";
    in print_endline str;
    get_move state prev user ()
  | Undo -> Grid.print_grid prev;
    if Grid.get_move_id state = Grid.get_move_id prev then
      print_endline "Can not undo more than once in a row.";
    get_move prev prev user () (** this prevents undoing more than once **)
  | Quit -> exit 0

(** [main] initializes a 4 by 4 game of 2048 and prints out what options
    the user has. *)
let main () = 
  let user = init_game_user () in
  let newgame = (init_game ()) in
  get_move newgame newgame user ()

let () = main ()