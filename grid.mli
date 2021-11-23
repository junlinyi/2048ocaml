(** [t] is a record that represents the state of a game, including 
    the grid and score*)
type t

(** [create_game] initializes the game state, which is an [n_] by [n_] 
    zero matrix and a starting score of 0*)
val create_game: int -> int -> t

(** [transition] takes a game state and a shift direction [dir] and 
    returns a new state with a new, shifted matrix and an updated score *)
val transition: t -> string -> t

(** [print_grid] takes a state and prints the current game matrix
    along with the player's current score*)
val print_grid: t -> unit

(** [get_grid] returns a two-dimensional list of the current state *)
val get_grid: t -> int list list

(** [get_score] returns the score of the current state *)
val get_score: t -> int

(** [get_size] returns the dimensino of the current state *)
val get_size: t -> int

(** [get_move_id] returns the move id of the current state *)
val get_move_id: t -> int

(** [game_over] returns whether the player has any possible moves left
    by checking whether any transitions will make a difference to 
    [state] *)
val game_over: t -> bool

(** [get_mode] returns the game mode of the current state*)
val get_mode: t -> int

(** [get_state] returns a state of the grid based on the inputs provided,
    namely [gr] for the grid, [size] for n, [s] for score, [mid] for 
    move id, [mode] for game mode. *)
val get_state: int list list -> int -> int -> int -> int -> t