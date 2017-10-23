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

  val init : width:int -> height:int -> init_handle -> backend Lwt.t
  val redraw : backend -> unit Lwt.t
  val set_title : backend -> string -> unit ret
  val pixel : backend -> x:int -> y:int -> color -> unit Lwt.t
  val horizontal : backend -> x:int -> y:int -> x_end:int -> color -> unit Lwt.t
  val draw_line : backend -> x:int -> y:int -> line -> unit Lwt.t
  val rect : backend -> x:int -> y:int -> x_end:int -> y_end:int -> color
    -> unit Lwt.t
end

type uchar = int

module type Framebuffer_S =
sig
  type init_handle
  type color
  type t

  val compile_rgb : ?r:char -> ?g:char -> ?b:char -> t -> color
  val redraw : t -> unit Lwt.t
  val letter : t -> uchar -> x:int -> y:int -> unit Lwt.t
  val letters : t -> string -> x:int -> y:int -> unit Lwt.t
  val pixel : t -> x:int -> y:int -> color -> unit Lwt.t
  val term_size : t -> int * int
  val readable : t -> string -> unit Lwt.t
  val init : width:int -> height:int -> init_handle -> t Lwt.t
end

module type Framebuffer_M = functor (B : Backend_S) ->
sig
  include Framebuffer_S with
        type init_handle = B.init_handle
    and type color = B.color
end
