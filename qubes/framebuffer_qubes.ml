open Lwt.Infix
open Qubes

type 'ok ret = 'ok Qubes.S.or_eof Lwt.t

type t = {
  gntrefs : Gnt.gntref list ;
  io_page  : Io_page.t ; (* spans several pages, allows consecutive blitting *)
  io_page_arr : Io_page.t array ; (* point to each individual page in io_page *)
  touched_pages : bool array ; (* bitmap tracking which io_page_arr pages have
                                  been modified since last MFNDUMP *)
  mutable width : int;
  mutable height: int;
  qubes : Qubes.GUI.t ;
  window : Qubes.GUI.window ;
}

type init_handle = Qubes.GUI.t
type backend = t

type color = string
let color_w = 4
type line = string


module Log : Logs.LOG = (val Logs.src_log (Logs.Src.create "framebuffer.qubes"
                                             ~doc:"Mirage.Framebuffer.Qubes"))

let error s = failwith ("fqubes-error: " ^ s)

let resolve_ret (thread : 'a S.or_eof Lwt.t) : 'a Lwt.t =
  thread >>= function | `Ok v -> Lwt.return v
                      | `Eof -> error "EOF"

let set_title t title = GUI.set_title t.window title

let pixel (t:backend) ~x ~y (color:color) =
  let y_offset = t.width * y * 4 in
  assert (color_w = String.length color) ;
  try
    Io_page.string_blit
    color 0
    t.io_page (y_offset + x*color_w)
    4
   with | _ -> failwith "pixel blitting failed"

module Compile = struct
  let rgb ?(r='\x00') ?(g='\x00') ?(b='\x00') _ : color =
    String.init 4 (function
        | 0 -> b
        | 1 -> g
        | 2 -> r
        | _ -> '\x7f' (* little-endian MSB; ignored by Qubes*)
      )

  let lineiter (f: int -> color) len _ : line =
    (* For some reason Bytes.blit doesn't seem to work *)
    let rec loop acc = function
      | i when i = len -> acc
      | i -> loop (String.concat "" [acc;f i]) (succ i)
    in loop "" 0

  let line (lst : color list) _ =
    let arr = Array.of_list lst in
    lineiter Array.(get arr) (Array.length arr) ()
end

(* TODO
     Io_page.string_blit String.(make Io_page.(length mapping) '\x33') 0
                         mapping 0
                         Io_page.(length mapping);

  let () =
    let p_sz = Io_page.page_size in
    if offset mod p_sz <> 0
    then t.touched_pages.(offset mod p_sz)
    for i = 0 to ((x2-x)*4+Io_page.page_size-1)/Io_page.page_size
  in
*)

let line_length line = (String.length line) / color_w

type err = [`Msg of string]

let sub_color_line line offset : (color, [> err]) result =
  if offset < (line_length line) && offset >= 0 then
    try
      Ok (String.sub line (offset * color_w) 4)
        with _ -> failwith "sub_color_line"
  else
    Error (`Msg (Fmt.strf "sub_color_line: [%d]: %d" (line_length line) offset))

let horizontal t ~x ~y ~x_end color =
  for x = max x 0  to min x_end (t.width - 1) do
    pixel t ~y:(max 0 (min (t.height-1) y)) ~x color
  done

let draw_line t ~x ~y line : unit =
  for l_off = 0 to line_length line -1 do
      match sub_color_line line l_off with (* TODO *)
      | Ok color -> pixel t ~x:(x+l_off) ~y color
      | Error _ -> failwith "TODO handle / implement"
      | exception _ -> failwith "sub_color_line failed"
  done

let rect_lineiter t ~x ~y ~y_end f : unit =
  for off = 0 to min (t.height - 1) (y_end-y-1) do
    draw_line t ~x ~y:(y+off) (f off)
  done

let rect t ~x ~y ~x_end ~y_end color =
  for y = max 0 y to min (t.height-1) y_end do
    horizontal t ~x ~y ~x_end color
  done

let mfndump t =
  (* returns a list of MFNs touched since last mfndump,
     and resets the page touch tracker *)
  (* TODO mark t.touched_pages.(..) <- false after dumping *)
  (* TODO replace this mess, consider just making a (Io_page.t * true) array*)
  Log.debug (fun f -> f "entering mfndump t");
  let mfn_of_iopage p =
    Io_page.get_addr p |> OS.Xen.virt_to_mfn |> Nativeint.to_int32 in
  Array.mapi (fun i -> function
  | false -> None
  | true -> Some (mfn_of_iopage t.io_page_arr.(i))
    ) t.touched_pages |> Array.to_list
  |> List.filter (function None -> false | _ -> true)
  |> List.map (function Some x -> x | None -> failwith "TODO implement")

let keysym_of_scancode code =
  Framebuffer__Keycodes.find_keysym
    (function
      | (_, Some x,_) when x <= 0x53 ->
        (* subtract 8 to go from X11 keycode to "PS/2 Set 1" scancode: *)
        code - 8 = x

      (* handle cases where X.org differs from IBM XT 1: *)
      | (_, _, sym) ->
        begin match code - 8, sym with
          | 0x57, `F11
          | 0x58, `F12
          | 0x60, `Keypad_Enter
          | 0x61, `Right_Control
          | 0x62, `Keypad_Slash
          (*`Print_sysrq -> code = 0x63 *)
          | 0x64, `Right_Alt
          (* 0x65: Linefeed *)
          | 0x66, `Home
          | 0x67, `Up_Arrow
          | 0x68, `Page_Up
          | 0x69, `Left_Arrow
          | 0x6A, `Right_Arrow
          | 0x6B, `End
          | 0x6C, `Down_Arrow
          | 0x6D, `Page_Down (* aka Next *)
          | 0x6E, `Insert
          | 0x6F, `Delete
          | 0x75, `Keypad_Equals
          (*| 0x76, plus/minus*)
          | 0x77, `Pause
          | 0x79, `Keypad_Comma (* aka KP_Decimal*)
          | 0x7D, `Left_Meta (* aka Super_L*)
          (* 0x7E, `Right_Meta (* aka Super_R *) *)
          | 0x7F, `Keyboard_Menu
          | 0x80, `Keyboard_Cancel
          | 0x81, `Keyboard_Again (* aka Redo *)
          | 0x83, `Keyboard_Undo
          | 0x88, `Keyboard_Find
          | 0x8A, `Keyboard_Help
          | 0x8C, `Calculator
          (*| 0x8F or 0x97, `Lenovo_Fn, see keycodes.ml  *)
          | 0xA3, `Scan_Next_Track
          | 0xA4, `Play_slash__Pause (* XF86AudioPlay / XF86AudioPause *)
          | 0xA5, `Scan_Previous_Track
          | 0xBE, `Mute
            (*| 0xC3, `Mode_switch aka AltGr ?? TODO *)
            -> true

          | _ -> false
        end
    )

let kmod_of_constant = let open Framebuffer__Keycodes in function
  | 0 -> None
  | 1 -> Shift
  | 2 -> Caps_lock
  | 4 -> Ctrl
  | 8 -> Alt (* left alt *)
  | 64 -> Mod1 (* left meta *)
  | _ -> None (* TODO *)

let rec recv_event (t:backend) : Framebuffer__S.backend_event Lwt.t =
  let open Framebuffer__S in
  GUI.recv_event t.window >>= function
  | Button {x;y;_} ->
    Lwt.return (Mouse_button {x = Int32.to_int x ; y = Int32.to_int y})
  | Keypress {state;keycode;ty; _} ->
    Log.info (fun m -> m "keypress state: %ld \
                          keycode %ld ty %ld" state keycode ty);
    (* XKeyEvent->type KeyPressMask / KeyReleaseMask*)
    (* see handle_keypress in qubes-gui-agent-linux/gui-agent/vmside.c *)
    (* keycode/scancode handling: see
       qubes-gui-agent-windows/gui-agent/xorg-keymap.c
    *)
    let scancode = Int32.to_int keycode in
    let mask = Int32.to_int state in
    Lwt.return
      ( Keypress {pressed = Int32.(logand ty 1_l = 0_l); (* 1 = up*)
                  scancode; mask;
                  mods = Framebuffer__Keycodes.kmods_of_mask
                           kmod_of_constant mask;
                  keysym = keysym_of_scancode scancode ;
                 } : Framebuffer__S.backend_event)
  | Focus _ -> Lwt.return Window_focus
  | Motion {x;y;_} -> Lwt.return (Mouse_motion {x; y})
  | Clipboard_data (cstruct:Cstruct.t) ->
    Lwt.return (Clipboard_paste (Cstruct.to_string cstruct))
  | Clipboard_request -> Lwt.return Clipboard_request
  | Window_crossing _ -> recv_event t
  | Window_destroy -> Lwt.return Window_close (* TODO actually means "it disappeared" *)
  | Window_close -> Lwt.return Window_close
  | UNIT () -> recv_event t (* TODO ignore*)

let window qubes_t ~width ~height : backend Lwt.t =
  (* 32 bits per pixel, rounded up: *)
  let _pixels = height * width in (* TODO handle resizing instead *)
  let page_count = ((1300 * 1000 * (32/8)) |> Io_page.round_to_page_size)
                   / Io_page.page_size in
  let open Gnt.Gntshr in
  Gnt.Gntshr.with_gntshr
  (fun interface ->
    begin match share_pages interface 0 page_count
                  false (*TODO: false? this is the read/write bit*) with
    | None -> failwith ("what we can't share "^(string_of_int page_count)
                        ^" pages with dom0")
  | Some { refs ; mapping } ->
     Log.info (fun f -> f "Set up %d shared pages with dom0" page_count);
     (*TODO assert (page_count = Io_page.length mapping / 4096);*)
     (let width = Int32.of_int width and height = Int32.of_int height in
       (resolve_ret @@ Qubes.GUI.create_window ~x:0l ~y:0l ~title:"my qubes title"
       ~width ~height qubes_t)
     ) >>= fun window ->
     let t =
       { height; width;
         io_page = mapping ;
         io_page_arr = Io_page.to_pages mapping |> Array.of_list ;
         touched_pages = Array.make page_count true ; (* send all first time *)
         gntrefs = refs;
         qubes = qubes_t;
         window;
      }
     in
     Lwt.return t
  end
  )

let init_backend _qubes_t = Lwt.return_unit

let resize ~width ~height t =
  (* TODO allocate more pixels and copy over (+ scale) old io-page if need be *)
  t.width <- width ;
  t.height <- height ;
  let width, height = Int32.of_int width, Int32.of_int height in
  GUI.send t.qubes
    [Formats.GUI.make_msg_configure ~width ~height
       ~x:0l ~y:0l (* try to give current (x,y) ? *)
       ~window:(GUI.int32_of_window t.window)] |> resolve_ret
  (* TODO technically speaking we are required to parse the response from
          CONFIGURE and cannot just assume that it changed the window size *)

let redraw t =
    let mfns = mfndump t in
    Log.debug (fun f -> f "MFNS: %d" List.(length mfns)) ;

    let width = Int32.of_int t.width and height = Int32.of_int t.height in
    (* http://xenbits.xen.org/gitweb/?p=xen.git;a=blob;f=xen/include/xen/mm.h;h=88de3c1fa6bb64bde8867ec4b53a18844b099be4;hb=HEAD *)
    let window = GUI.int32_of_window t.window in
    let x, y = 0_l , 0_l in
    GUI.send t.qubes
      [ (* Resize the window: *)
        Formats.GUI.make_msg_configure ~window ~x ~y ~width ~height ;
        (* signal the touched MFNs: *)
        Formats.GUI.make_msg_mfndump ~width ~height ~mfns ~window ;
        (* force X.org redraw: *)
        Formats.GUI.make_msg_shmimage ~window ~x ~y ~height ~width ;
      ] >>= function
    | `Ok () -> Lwt.return ()
    | `Eof -> error "EOF during redraw"

    (*send_wmhints()*)
(*    let our_wmhints = Formats.GUI.make_msg_window_hints ~height ~width in
    QV.send gui [our_wmhints] >>= function
    | `Eof -> Lwt.fail (error "EOF during MSG_WINDOW_HINTS")
    | `Ok () ->
*)
  (* ENDOF process_pv_resize() *)


(*
[qubes.gui] Unexpected data with unknown type: [91 00 00 00 01 00 00 00 08 00 00 00 ] 00 00 00 00 04 00 00 00 a
INF [qubes.gui] debug_window [1]: UNIT
WRN [qubes.gui] Unexpected data with unknown type: [91 00 00 00 01 00 00 00 08 00 00 00 ] 04 00 00 00 00 00 00 00 a

*)

(* keypress:
  x/y: coordinate of the mouse, starting at 0,0: UPPER left corner

  keycodes:
  a     38
  shift 50
  right-shift 62
  alt   64
  meta  133
  altgr 108
*)
