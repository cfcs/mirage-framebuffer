type backend_event =
  | Clipboard_paste of string (** When the user pastes [string] in the window*)
  | Clipboard_request (** When the backend requests our window's clipboard *)
  | Keypress (** When a keyboard key is pressed or released *)
  | Mouse_button (** When a mouse button is pressed or released *)
  | Mouse_motion (** When the mouse is moved (not necessarily within window)*)
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
  val recv_event : backend -> backend_event Lwt.t
  val redraw : backend -> unit Lwt.t
  val set_title : backend -> string -> unit ret
  val pixel : backend -> x:int -> y:int -> color -> unit Lwt.t
  val horizontal : backend -> x:int -> y:int -> x_end:int -> color -> unit Lwt.t
  val draw_line : backend -> x:int -> y:int -> line -> unit Lwt.t
  val rect : backend -> x:int -> y:int -> x_end:int -> y_end:int -> color
    -> unit Lwt.t
  val rect_lineiter : backend -> x:int -> y:int -> y_end:int ->
          (int -> line) -> unit Lwt.t
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
  val letter : t -> uchar -> x:int -> y:int -> unit Lwt.t
  val letters : t -> string -> x:int -> y:int -> unit Lwt.t
  val pixel : t -> x:int -> y:int -> color -> unit Lwt.t
  val rect : t -> x:int -> y:int -> x_end:int -> y_end:int -> color ->
    unit Lwt.t
  val rect_lineiter : t -> x:int -> y:int -> y_end:int -> (int -> line) ->
    unit Lwt.t
  val term_size : t -> int * int
  val readable : t -> string -> unit Lwt.t
  val window : width:int -> height:int -> t Lwt.t
  val recv_event : t -> backend_event Lwt.t
end

module type Framebuffer_M = functor (B : Backend_S) ->
sig
  val init : B.init_handle -> (module Framebuffer_S) Lwt.t
end
