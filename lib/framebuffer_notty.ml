open Notty

module type Pqwy_term =
sig
  (** Adapted from Notty_unix since there doesn't seem to be a general module
      type defined anywhere. *)

  type t (** terminal type*)

  type fdescr (* Unix: Unix.file_descr*)

  type ret (** return type. unit or unit Lwt.t etc *)

  val create : ?dispose:bool ->
    ?nosig:bool ->
    ?mouse:bool ->
    ?input:fdescr ->
    ?output:fdescr ->
    unit -> t

  val release : t -> ret
  val image : t -> image -> ret
  val cursor : t -> (int * int) option -> ret

  val event : t -> [Unescape.event | `Resize of (int*int) | `End ]
  val pending : t -> bool

  val size : t -> (int*int)
  val fds : t -> fdescr * fdescr
end

module type Pqwy_unix =
sig
  type fdescr (* Unix.file_descr *)
  type ret
  val winsize : fdescr -> (int * int) option
  val output_image : ?cap:Cap.t -> ?clear:bool -> ?chan:out_channel -> image
    -> ret
  val output_image_size : ?cap:Cap.t -> ?clear:bool -> ?chan:out_channel ->
    (int * int -> image) -> ret
  val output_image_endline : ?cap:Cap.t -> ?clear:bool -> ?chan:out_channel
    -> image -> ret
end

open Lwt.Infix

module Make(FB:S.Framebuffer_S) = struct
  type t = {
    trm : Tmachine.t;
    fb : FB.t;
  }

  let set_size t dim = Tmachine.set_size t.trm dim

  let create ?(dispose=true) ?(nosig=true) ?(mouse=true)
             ?(input=`Stdin) fb =
    let t = { trm = Tmachine.create ~mouse Notty.Cap.dumb;
              fb
    } in
    set_size t (FB.term_size t.fb) ;
    t

  let rec write t =
    match Tmachine.output t.trm with
    | `Output s -> FB.readable t.fb s >>= fun () -> write t
    | `Await    -> FB.redraw t.fb

  let refresh t = Tmachine.refresh t.trm; write t
  let image t image = Tmachine.image t.trm image; write t
  let cursor t (pos:(int*int) option) = Tmachine.cursor t.trm pos; write t
  let size t = Tmachine.size t.trm

  let release t =
    (*if Tmachine.release t.trm then
      ( t.trm.stop (); write t)
    else *) Lwt.return_unit


  let event t : [Unescape.event | `Resize of (int*int) | `End] = `End
  let pending t : bool = true
  let output_image ~(cap:Notty.Cap.t) ?clear ?chan image = ()
end
