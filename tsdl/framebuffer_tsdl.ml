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

(* modifying pixels:
      Sdl.create_rgb_surface ~w:256 ~h:256 ~depth:32
0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
      assert (Sdl.lock_surface s0 = Ok ());
      let ba = Sdl.get_surface_pixels s0 Bigarray.int32 in
      assert (Bigarray.Array1.dim ba = width * height);
      ba.{0} <- 0xFF0000FFl;
      assert (ba.{0} = 0xFF0000FFl);
      Sdl.unlock_surface s0;
 *)
type window_id = int
type backend =
  { no: window_id;
    event_mvar: Framebuffer__S.backend_event Lwt_mvar.t ;
    window : Sdl.window ;
    renderer : Sdl.renderer ;
  }
type global_state_t =
  { mutable windows : (window_id * backend) list ; }

let global_state : global_state_t = { windows = [] ; }
let no_window = 0 (* 0 is returned for errors on calls to SDL_GetWindowID(win),
                     see https://wiki.libsdl.org/SDL_GetWindowID*)

let sdl_ALPHA_OPAQUE = 0xff_l

let event_loop () : unit Lwt.t =
  let open Framebuffer__S in
  let ev = Sdl.Event.create () in
  let get_field = Sdl.Event.get ev in
  (*Sdl.Event.(get ev keyboard_window_id), mouse_button_window_id, etc*)
  let parse_event () : (unit Lwt.t, [`Msg of string]) result=
    let open Sdl.Event in
    match Sdl.poll_event (Some ev) with
    | false -> Error (`Msg "Sdl.poll_event")
    | true ->
      let ev_type = Sdl.Event.(get ev typ) in
      Logs.debug (fun m -> m "got an event");
      begin match Sdl.Event.enum ev_type with
        | `Clipboard_update ->
          begin match Sdl.get_clipboard_text () with
            | Error (`Msg err) ->
              Logs.err (fun m -> m "Sdl.get_clipboard_text: %s" err);
              Error (`Msg err)
            | Ok str -> Ok (0, Clipboard_paste str)
          end
        | `Drop_file ->
          Sdl.Event.drop_file_free ev ;
          Error (`Msg "Sdl.Event.`Drop_file")
        | `Key_up | `Key_down -> Ok (get_field keyboard_window_id, Keypress)
        | `Mouse_button_down ->
          Ok (get_field mouse_button_window_id, Mouse_button)
        | `Window_event -> Error (`Msg "Event.(get ev window_event_focus) etc")
        | `Quit -> Ok (0, Window_close) (*TODO *)
        | _ -> Error (`Msg "unhandled event")
      end >>= fun (window_id, parsed_ev) ->
      begin match List.assoc window_id global_state.windows with
        | exception Not_found ->
          Error (`Msg "TODO ignore global events?")
        | window ->
          Ok (Lwt_mvar.put window.event_mvar parsed_ev)
      end
  in
  let rec loop () =
    let open Lwt.Infix in
    begin match parse_event () with
    | Error (`Msg _) -> Lwt.return_unit
    | Ok promise -> promise
    end >>= fun () ->
    Lwt_unix.sleep 0.01 >>= loop (* tick every 1/100 second*)
  in
  loop ()
  (*Sdl.start_text_input ();*)

(* TODO use Sdl.lock_surface / Sdl.lock_texture *)

let redraw (b:backend) = Sdl.render_present b.renderer ; Lwt.return_unit

let recv_event (b:backend) : Framebuffer__S.backend_event Lwt.t =
  Lwt_mvar.take b.event_mvar

type init_handle = unit

let window (() :init_handle) ~width ~height =
  let w, h = width, height in
  let window, renderer = Sdl.create_window_and_renderer ~w ~h
      Sdl.Window.(windowed + resizable) |> R.get_ok in
  let backend = {window ; renderer; no = Sdl.get_window_id window;
                 event_mvar = Lwt_mvar.create_empty () }
  in
  assert (backend.no <> no_window);
  global_state.windows <- (backend.no, backend) :: global_state.windows ;
  (*event_loop () ;*)
  (* Sdl.destroy_window w ; Sdl.quit () *)
  Lwt.return backend

let init_backend ((): init_handle) =
  let _ = Sdl.init Sdl.Init.(timer + video + events) in
  Lwt.async event_loop ;
  Lwt.return_unit

type 'a ret = 'a
type line = { w : int; (*width, in pixels *)
              texture: Sdl.texture} (*(int32, Bigarray.int32_elt) bigarray*)
type color = int32 (*Sdl.color*)

(*let my_pixel_format_enum = Sdl.Pixel.format_rgb888*)
(* consider (get_current_display_mode |> R.get_ok).dm_format
   OR Sdl.get_surface_format_enum *)

(*let pixel_format = Sdl.alloc_format my_pixel_format_enum |> R.get_ok*)

let set_title (b:backend) title = Sdl.set_window_title b.window title

module Compile =
struct
  let rgb ?(r='\x00') ?(g='\x00') ?(b='\x00') (_:backend) =
    (*Sdl.Color.create ~r:(Char.code r) ~g:(Char.code g) ~b:(Char.code b)
      ~a:sdl_ALPHA_OPAQUE*)
    let open Int32 in
       add (shift_left (of_int @@ int_of_char r) 24)
           (shift_left (of_int @@ int_of_char g) 16)
    |> add (shift_left (of_int @@ int_of_char b) 8)
    |> add sdl_ALPHA_OPAQUE

  let line (lst:color list) b : line =
    let w = List.length lst and depth = 32 in
    let rmask = 0xff_00_00_00_l
    and gmask = 0xff_00_00_l
    and bmask = 0xff_00l in
    let open Tsdl.Sdl in
    let surface : surface =
      Sdl.create_rgb_surface ~w ~h:1 ~depth
        rmask gmask bmask sdl_ALPHA_OPAQUE
      |> R.get_ok in
    assert(Sdl.lock_surface surface = Ok ());
    let ba = Sdl.get_surface_pixels surface Bigarray.int32 in
    List.iteri (fun i (p:int32) -> Bigarray.Array1.set ba i p) lst;
    Sdl.unlock_surface surface ;
    let texture =
      Sdl.create_texture_from_surface b.renderer surface |> R.get_ok in
    let()=Sdl.free_surface surface in {w ; texture}

  let lineiter f i b = line (Array.init i f |> Array.to_list) b
end

let draw_line (b:backend) ~(x:int) ~(y:int) ({w;texture}:line) =
  (*log "drawing line x:%d y:%d w:%d" x y w;*)
  ignore @@ Sdl.render_copy ~dst:(Sdl.Rect.create ~x ~y ~w ~h:1) b.renderer texture ;
  Lwt.return_unit
  (*Sdl.render_fill_rects_ba b.renderer l ;*)

let rect_lineiter (b:backend) ~x ~y ~y_end f =
  let open Framebuffer.Utils in
  lwt_for (y_end - y)
    (fun off -> draw_line b ~x ~y:(y+off) (f off))

let horizontal (b:backend) ~(x:int) ~(y:int) ~(x_end:int) (c:color) =
  draw_line b ~x ~y (Compile.lineiter (fun _ -> c) (max (x_end-x) 0) b)

open Framebuffer.Utils

let rect (b:backend) ~(x:int) ~(y:int) ~(x_end:int) ~(y_end:int) (c:color) =
  lwt_for ~start:(max y 0) y_end
    (fun y -> horizontal b ~x ~y ~x_end c)
  (* https://github.com/dbuenzli/tsdl/blob/master/test/test.ml#L122 *)


let pixel (b:backend) ~(x:int) ~(y:int) (c:color) : unit Lwt.t=
  horizontal b ~x ~y ~x_end:(x+1) c
  (* Sdl.set_render_draw_color b.renderer r g b 0xFF;
     Sdl.render_draw_point b.renderer x y |> R.get_ok ;
     redraw b
   * *)
