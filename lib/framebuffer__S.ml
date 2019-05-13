module Keycodes = Framebuffer__Keycodes

type backend_event =
  | Clipboard_paste of string (** When the user pastes [string] in the window*)
  | Clipboard_request (** When the backend requests our window's clipboard *)
  | Keypress of { pressed : bool ; (** pressed (true) || released (false) *)
                  scancode : int ; (** The scancode exposed by the backend *)
                  mask : int ; (** The modifier mask exposed by the backend *)
                  mods : Keycodes.kmod list ; (** The parsed modifier mask *)
                  keysym : Keycodes.keysym option ; (** The parsed keysym *)
                }(** When a keyboard key is pressed or released *)
  | Mouse_button of { x : int; y : int}
    (** When a mouse button is pressed or released. y = 0 at the top.
        TODO Need to expose which button, and the state (pressed/released) etc*)
  | Mouse_motion of { x: int ; y : int}
    (** When the mouse is moved (not necessarily within window) *)
  | Window_close
  | Window_focus
  (* | Window_focus *)
  | Resize of int * int

module type Backend_S =
sig
  type backend
  type color
  type line
  type 'a ret
  type init_handle

  module Compile :
  sig
    val rgb : ?r:char -> ?g:char -> ?b:char -> backend -> color
    val line : color list -> backend -> line
    val lineiter : (int -> color) -> int -> backend ->line
  end

  val init_backend : init_handle -> unit Lwt.t
  val window : init_handle -> width:int -> height:int -> backend Lwt.t
  val resize : width:int -> height:int -> backend -> unit Lwt.t
  val recv_event : backend -> backend_event Lwt.t
  val redraw : backend -> unit Lwt.t
  val set_title : backend -> string -> unit ret
  val pixel : backend -> x:int -> y:int -> color -> unit
  val horizontal : backend -> x:int -> y:int -> x_end:int -> color -> unit
  val draw_line : backend -> x:int -> y:int -> line -> unit
  val rect : backend -> x:int -> y:int -> x_end:int -> y_end:int -> color
    -> unit
  val rect_lineiter : backend -> x:int -> y:int -> y_end:int ->
          (int -> line) -> unit

  val keysym_of_scancode : int -> Framebuffer__Keycodes.keysym option
  val kmod_of_constant : int -> Framebuffer__Keycodes.kmod
end

type uchar = int

module type Framebuffer_S =
sig
  type color
  type line
  type t

  val compile_rgb : ?r:char -> ?g:char -> ?b:char -> t -> color
  val compile_line : color list -> t -> line

  val redraw : t -> unit Lwt.t
  val letter : t -> uchar -> x:int -> y:int -> unit
  val letters : t -> string -> x:int -> y:int -> unit
  val output_tty : t -> int * int -> string -> unit Lwt.t
  val pixel : t -> x:int -> y:int -> color -> unit
  val rect : t -> x:int -> y:int -> x_end:int -> y_end:int -> color -> unit
  val rect_lineiter : t -> x:int -> y:int -> y_end:int -> (int -> line) -> unit

  val term_size : t -> int * int
  (** [term_size t] returns the (width,height) dimensions currently available
      for a text console using the builtin font *)

  val window : width:int -> height:int -> t Lwt.t
  (** [window ~width ~height] opens a new window [t]*)

  val resize : width:int -> height:int -> t -> unit Lwt.t
  (** [resize ~width ~height t] attempts to resize the window [t]
      TODO error handling? *)

  val recv_event : t -> backend_event Lwt.t
  val keysym_of_scancode : int -> Keycodes.keysym option
  val kmod_of_constant : int -> Keycodes.kmod
end

module type Framebuffer_M = functor (B : Backend_S) ->
sig
  val init : B.init_handle -> (module Framebuffer_S) Lwt.t
end
