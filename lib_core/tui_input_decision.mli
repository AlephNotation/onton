(* @archlint.module interface
   @archlint.domain tui-input *)

(** Pure keyboard input -> TUI command translation.

    Maps {!Term_key.t} values to semantic TUI commands. Contains no I/O; the
    reading of keys from stdin is the caller's responsibility. *)

type command =
  | Quit
  | Help
  | Move_up
  | Move_down
  | Page_up
  | Page_down
  | Scroll_top
  | Scroll_bottom
  | Select
  | Back
  | Noop
  | Timeline
  | Send_message of string
  | Add_worktree of string
  | Open_in_browser
[@@deriving show, eq]

(** Input mode for the TUI prompt. *)
type input_mode =
  | Normal
  | Prompt_worktree  (** Buffer holds path string *)
  | Prompt_message  (** Buffer holds message text, detail view only *)
  | Prompt_broadcast  (** Buffer holds message to send to all active patches *)
  | Manage_patch  (** Menu of break-glass patch actions, detail view only *)
[@@deriving show, eq]

val prompt_prefix : input_mode -> string
(** Returns the prompt prefix for each mode. *)

val of_key : Term_key.t -> command
(** Translate a key press into a TUI command. *)

val apply_move : count:int -> selected:int -> command -> int
(** Apply a navigation command to the selected index.

    Returns [-1] when no patch should be selected. Page moves clamp to
    boundaries instead of deselecting. Valid range: [-1, count-1]. *)
