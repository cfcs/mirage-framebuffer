open Tsdl
open Rresult

(* TODO this is currently a big mess, with lots of memory leaks.
        It seems there are two main directions to go:
     - using Textures, which may be optimized(?)
        - Sdl.update_texture for blitting to the texture,
        - Sdl.render_copy to update the current view?
        - redraw: Sdl.render_present
        - TODO look into Sdl.get_render_target
     - using Surfaces, which are basically pixel framebuffer arrays
        - https://wiki.libsdl.org/SDL_BlitSurface
          - copying to current window surface:
            https://wiki.libsdl.org/SDL_GetWindowSurface
        - redraw: https://wiki.libsdl.org/SDL_UpdateWindowSurface
                  https://wiki.libsdl.org/SDL_UpdateWindowSurfaceRects

*)

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
    mutable width : int ;
    mutable height : int ;
    renderer : Sdl.renderer ;
    mutable surface : (int32, Bigarray.int32_elt) Sdl.bigarray ;
  }
type global_state_t =
  { mutable windows : (window_id * backend) list ; }

module Log : Logs.LOG = (val Logs.src_log (Logs.Src.create "framebuffer.tsdl"
                                             ~doc:"Mirage.Framebuffer.TSDL"))

let global_state : global_state_t = { windows = [] ; }
let no_window = 0 (* 0 is returned for errors on calls to SDL_GetWindowID(win),
                     see https://wiki.libsdl.org/SDL_GetWindowID*)

let sdl_ALPHA_OPAQUE = 0xff_l

let keysym_of_scancode code =
  Framebuffer__Keycodes.find_keysym (fun (x, _, _) -> code = x)

let kmod_of_constant = let open Framebuffer__Keycodes in function
  | 1 | 2 -> Shift (* _L / _R *)
  | 64 | 128 -> Ctrl (* _L / _R *)
  | 256 | 512 -> Alt (* _L / _R*)
  | 1024 -> Mod1 (* _L *)
  | 8192 -> Caps_lock
  | 0 | _ -> None (* TODO *)


let event_loop () : unit Lwt.t =
  let open Framebuffer__S in
  let ev = Sdl.Event.create () in
  let get_field f = Sdl.Event.get ev f in
  (*Sdl.Event.(get ev keyboard_window_id), mouse_button_window_id, etc*)
  let parse_event () : (unit Lwt.t, [`Poll | `Msg of string]) result=
    let open Sdl.Event in
    match Sdl.poll_event (Some ev) with
    | false -> Error `Poll
    | true ->
      let ev_type = Sdl.Event.(get ev typ) in
      Log.debug (fun m -> m "got an event");
      begin match Sdl.Event.enum ev_type with
        | `Clipboard_update -> (* TODO handle ctrl-c also *)
          begin match Sdl.get_clipboard_text () with
            | Error (`Msg err) ->
              Log.err (fun m -> m "Sdl.get_clipboard_text: %s" err);
              Error (`Msg err)
            | Ok str ->
              (* TODO get currently focused window *)
              Ok (List.length global_state.windows, Clipboard_paste str)
          end
        | `Drop_file ->
          Sdl.Event.drop_file_free ev ;
          Error (`Msg "Sdl.Event.`Drop_file")
        | `Key_up | `Key_down ->
          let pressed =
            let state = get_field keyboard_state in
            state = Sdl.pressed
          in
          let repeat = get_field keyboard_repeat in
          let scancode = get_field keyboard_scancode in
          let keycode = get_field keyboard_keycode in
          let keymod = get_field keyboard_keymod in
          (* 1 = left-shift
             2 = right-shift
             64 = left-ctrl
             128 = right-ctrl
             256 = left-alt
             512 = right-alt
             1024 = left-mod
          *)
          Log.warn (fun m -> m "SDL key: down: %b repeat: %d scancode: %d \
                                  keycode: %d keymod :%d"
                         pressed repeat scancode keycode keymod);
          Ok (get_field keyboard_window_id,
              Keypress {pressed; scancode; mask = keymod;
                        mods = Framebuffer__Keycodes.kmods_of_mask
                                  kmod_of_constant keymod ;
                        keysym = keysym_of_scancode scancode})
        | `Mouse_button_down ->
          let x, y = get_field mouse_button_x, get_field mouse_button_y in
          Ok (get_field mouse_button_window_id, Mouse_button {x; y})
        | `Mouse_motion ->
          let x, y = get_field mouse_motion_x, get_field mouse_motion_y in
          Ok (get_field mouse_motion_window_id, Mouse_motion {x; y})
        | `Window_event ->
          let w_id = get_field window_window_id
          and w_ev = get_field window_event_id in
          begin match window_event_enum w_ev with
            | `Close -> Ok (w_id, Window_close)
            | `Size_changed (* TODO see http://sdl.5483.n7.nabble.com/resize-td35916.html *)
            | `Resized ->
              (* TODO should invalidate window surface and redraw, unlike
                 the Qubes target where a resize just means the painted view
                 size changed, and that the application may want to re-scale.*)
              Ok (w_id, Resize (get_field window_data1 |> Int32.to_int,
                                get_field window_data2 |> Int32.to_int))
            | _ ->  Error (`Msg "Event.(get ev window_event_focus) etc")
          end
        | `Quit -> Ok (0, Window_close) (*TODO *)
        | `App_terminating
        | `Finger_down | `Finger_up | `Finger_motion
        | `Mouse_wheel | `Multi_gesture | `Sys_wm_event
        | `Text_editing | `Text_input
        | _ -> Error (`Msg "unhandled event")
      end >>= fun (window_id, parsed_ev) ->
      begin match List.assoc window_id global_state.windows with
        | exception Not_found ->
          Log.debug (fun m -> m "event for window id %d not handled" window_id);
          Error (`Msg "TODO ignore global events?")
        | window ->
          Ok (Lwt_mvar.put window.event_mvar parsed_ev)
      end
  in
  let rec loop () =
    let open Lwt.Infix in
    begin match parse_event () with
      | Error `Poll -> Lwt.return_unit
      | Error (`Msg msg) ->
        Log.err (fun m -> m "parse_event: %s" msg);
        Lwt.return_unit
      | Ok promise -> promise
    end >>= fun () ->
    Lwt_unix.sleep 0.01 >>= loop (* tick every 1/100 second*)
  in
  loop ()
  (*Sdl.start_text_input ();*)

(* TODO use Sdl.lock_surface / Sdl.lock_texture *)

let redraw (b:backend) =
  (*Sdl.render_present b.renderer *)
  match Sdl.update_window_surface b.window with
  | Ok () -> Lwt.return_unit
  | Error `Msg x ->
    Log.err (fun m -> m "Sdl.update_window_surface %s" x) ;
    Lwt.return_unit

let recv_event (b:backend) : Framebuffer__S.backend_event Lwt.t =
  Lwt_mvar.take b.event_mvar

type init_handle = unit

let window (() :init_handle) ~width ~height =
  let w, h = width, height in
  let window, renderer = Sdl.create_window_and_renderer ~w ~h
      Sdl.Window.(windowed + resizable) |> R.get_ok in
  let backend =
    match Sdl.get_window_surface window with
    | Error _ -> failwith "get_window_surface"
    | Ok surf ->
    let width, height = Sdl.get_surface_size surf in
    {window ; renderer; no = Sdl.get_window_id window;
     event_mvar = Lwt_mvar.create_empty () ;
     surface = Sdl.get_surface_pixels surf Bigarray.Int32 ;
     width ; height;
    }
  in
  assert (backend.no <> no_window);
  global_state.windows <- (backend.no, backend) :: global_state.windows ;
  (*event_loop () ;*)
  (* Sdl.destroy_window w ; Sdl.quit () *)
  Lwt.return backend

let resize ~width ~height t =
  Sdl.set_window_size t.window ~w:width ~h:height ;
  let mode = match Sdl.get_window_display_mode t.window with
    | Ok m -> m
    | _ -> failwith "should not happen: Tsdl.Sdl.get_window_display" in
  ignore @@ Sdl.set_window_display_mode t.window
    {mode with dm_w = width ; dm_h = height } ;
  begin match (Sdl.get_window_surface t.window) with
    | Ok surf -> let width, height = Sdl.get_surface_size surf in
      t.width <- width ;
      t.height <- height ;
                  t.surface <- Sdl.get_surface_pixels surf Bigarray.Int32
    | Error _ -> failwith "woops getting surface"
  end ;
  Logs.debug (fun m -> m "resize: %d %d %a" t.width t.height
                Fmt.(pair int int) (Sdl.get_window_size t.window));
  Lwt.return_unit

let init_backend ((): init_handle) =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter () ;
  Logs.(set_level @@ Some Debug); (* TODO *)
  let _ = Sdl.init Sdl.Init.(timer + video + events) in
  Lwt.async event_loop ;
  Lwt.return_unit

type 'a ret = 'a
type color = Sdl.uint32 (*Sdl.color*)
type line = { w : int; (*width, in pixels *)
              ba: (color, Bigarray.int32_elt, Bigarray.c_layout)
                    Bigarray.Array1.t}

(*let my_pixel_format_enum = Sdl.Pixel.format_rgb888*)
(* consider (get_current_display_mode |> R.get_ok).dm_format
   OR Sdl.get_surface_format_enum *)

(*let pixel_format = Sdl.alloc_format my_pixel_format_enum |> R.get_ok*)

let set_title (b:backend) title = Sdl.set_window_title b.window title

module Compile =
struct
  let [@inline always] rgb ?(r='\x00') ?(g='\x00') ?(b='\x00') (_:backend) =
    (*Sdl.Color.create ~r:(Char.code r) ~g:(Char.code g) ~b:(Char.code b)
      ~a:sdl_ALPHA_OPAQUE*)
    (*let open Int32 in -- for textures:
       add (shift_left (of_int @@ int_of_char r) 24)
           (shift_left (of_int @@ int_of_char g) 16)
    |> add (shift_left (of_int @@ int_of_char b) 8)
      |> add sdl_ALPHA_OPAQUE*)
    let open Int32 in (* for surfaces: *)
       add (shift_left (of_int @@ int_of_char r) 16)
           (shift_left (of_int @@ int_of_char g) 8)
    |> add (shift_left (of_int @@ int_of_char b) 0)
    |> add (shift_left sdl_ALPHA_OPAQUE 24)

  let [@inline always] line (lst:color list) _backend : line =
    let w = List.length lst in
    let ba = Bigarray.Array1.create Bigarray.int32 Bigarray.C_layout w in
    List.iteri (fun i (p:int32) -> Bigarray.Array1.unsafe_set ba i p) lst;
    {w ; ba}

  let lineiter f i b = line (Array.init i f |> Array.to_list) b
end

let draw_line (b:backend) ~(x:int) ~(y:int) ({w;ba}:line) =
  (*log "drawing line x:%d y:%d w:%d" x y w;*)
  let wss = Sdl.get_window_surface b.window |> R.get_ok in
  (*Sdl.lock_surface wss |> R.get_ok;*)
  let surface_ba = (wss |> Sdl.get_surface_pixels) Bigarray.Int32 in
  let ws_w,ws_h = Sdl.get_surface_size wss in
  let sub_surface = Bigarray.Array1.(sub surface_ba (ws_w*y+x)
                                       (min (ws_w*ws_h-w) w)) in
  Bigarray.Array1.blit ba sub_surface
  (*ignore @@ Sdl.render_copy ~dst:(Sdl.Rect.create ~x ~y ~w ~h:1) b.renderer texture*)
  (*Sdl.render_fill_rects_ba b.renderer l ;*)

let rect_lineiter (b:backend) ~x ~y ~y_end f =
  for off = 0 to (y_end - y) -1 do
    draw_line b ~x ~y:(y+off) (f off)
  done

let horizontal (b:backend) ~(x:int) ~(y:int) ~(x_end:int) (c:color) =
  draw_line b ~x ~y (Compile.lineiter (fun _ -> c) (max (x_end-x) 0) b)

let rect (b:backend) ~(x:int) ~(y:int) ~(x_end:int) ~(y_end:int) (c:color) =
  for y = max y 0 to y_end do (* TODO bounds checking *)
    horizontal b ~x ~y ~x_end c
  done
  (* https://github.com/dbuenzli/tsdl/blob/master/test/test.ml#L122 *)


let pixel (b:backend) ~(x:int) ~(y:int) (c:color) : unit =

  (*Sdl.lock_surface wss |> R.get_ok;*)
  (*Bigarray.Array1.unsafe_set b.surface (y * (b.width) + x) c*)
  b.surface.{y * (b.width) + x} <- c
  (*Sdl.unlock_surface wss;*)
(*horizontal b ~x ~y ~x_end:(x+1) c*)
  (* Sdl.set_render_draw_color b.renderer r g b 0xFF;
     Sdl.render_draw_point b.renderer x y |> R.get_ok ;
     redraw b
   * *)

(*
The key you just pressed is not recognized by SDL. To help get this fixed, please report this to the SDL forums/mailing list <https://discourse.libsdl.org/> X11 KeyCode 151 (143), X11 KeySym 0x1008FF2B (XF86WakeUp).
--> fn (+shift+insert)
*)
