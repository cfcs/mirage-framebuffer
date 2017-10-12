open Tsdl
open Rresult

(* consider using cairo2 / `opam install cairo` and xlib bindings instead
   https://www.cypherpunk.at/2014/11/cairo-graphics-and-x11xlib/
   https://www.cairographics.org/Xlib/ *)


(* tutorial on pixel bitmaps using SDL:
   http://friedspace.com/cprogramming/sdlpixels.php *)

(*
 val Lwt_io.make : 
  ?buffer:Lwt_bytes.t ->
  ?close:(unit -> unit Lwt.t) ->
  ?seek:(int64 -> Unix.seek_command -> int64 Lwt.t) ->
  mode:'mode mode ->
  (Lwt_bytes.t -> int -> int -> int Lwt.t) -> 'mode channel
*)

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf (fmt ^^ "@.")

let sdl_ALPHA_OPAQUE = 0xff

let event_loop backend : unit =
  let e = Sdl.Event.create () in
  let rec loop () = match Sdl.wait_event (Some e) with
  | Error (`Msg e) -> log_err " Could not wait event: %s" e; ()
  | Ok () ->
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> backend
      | `Drop_file -> Sdl.Event.drop_file_free e; loop ()
      | _ -> loop ()
  in
  Sdl.start_text_input ();
  loop ()

type init_handle = unit

type backend = {
    window : Sdl.window ;
    renderer : Sdl.renderer
  }

(* TODO use Sdl.lock_surface / Sdl.lock_texture *)

let redraw (b:backend) = Sdl.render_present b.renderer ; Lwt.return_unit

let init ~width ~height (xyz:init_handle) =
  let w, h = width, height in
  let _ = Sdl.init Sdl.Init.(timer + video + events) in
  let window, renderer = Sdl.create_window_and_renderer ~w ~h
      Sdl.Window.(windowed + opengl + resizable) |> R.get_ok in
  (*event_loop () ;*)
  (* Sdl.destroy_window w ; Sdl.quit () *)
  Lwt.return {window ; renderer}

type 'a ret = 'a
type line = Sdl.texture (*(int32, Bigarray.int32_elt) bigarray*)
type color = Sdl.color

let my_pixel_format_enum = Sdl.Pixel.format_rgb888
(* consider (get_current_display_mode |> R.get_ok).dm_format
   OR Sdl.get_surface_format_enum *)

let pixel_format = Sdl.alloc_format my_pixel_format_enum |> R.get_ok

let set_title (b:backend) title = Sdl.set_window_title b.window title

let rect (b:backend) ~(x:int) ~(y:int) ~(x_end:int) ~(y_end:int) (c:color) =
  Lwt.return ()
  (* https://github.com/dbuenzli/tsdl/blob/master/test/test.ml#L122 *)

let horizontal (b:backend) ~(x:int) ~(y:int) ~(x_end:int)
    (c:color) = Lwt.return ()
let draw_line (b:backend) ~(x:int) ~(y:int) (l:line) =
  ignore @@ Sdl.render_draw_points b.renderer [] ;
  redraw b

let pixel (b:backend) ~(x:int) ~(y:int) (c:color) : unit Lwt.t=
  Sdl.render_draw_point b.renderer x y |> R.get_ok ; redraw b

module Compile =
struct
  let rgb ?(r='\x00') ?(g='\x00') ?(b='\x00') (backend:backend) =
    Sdl.Color.create ~r:(Char.code r) ~g:(Char.code g) ~b:(Char.code b)
      ~a:sdl_ALPHA_OPAQUE
  let line lst b =
    Sdl.create_texture b.renderer my_pixel_format_enum
      Sdl.Texture.access_target
      ~w:(List.length lst) ~h:1 |> R.get_ok
  let lineiter f i b = line [f 0] b (*TODO*)
end
