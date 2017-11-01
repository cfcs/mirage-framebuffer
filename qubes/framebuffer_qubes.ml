open Lwt.Infix
open Qubes

type 'ok ret = 'ok Qubes.S.or_eof Lwt.t

type t = {
  gntrefs : Gnt.gntref list ;
  io_page  : Io_page.t ; (* spans several pages, allows consecutive blitting *)
  io_page_arr : Io_page.t array ; (* point to each individual page in io_page *)
  touched_pages : bool array ; (* bitmap tracking which io_page_arr pages have
                                  been modified since last MFNDUMP *)
  width : int;
  height: int;
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
  Io_page.string_blit
    color 0
    t.io_page (y_offset + x*color_w)
    4 ;
  Lwt.return_unit (* TODO for now*)

module Compile = struct
  let rgb ?(r='\x00') ?(g='\x00') ?(b='\x00') _ : color =
    let s = Bytes.create 4 in
    Bytes.unsafe_set s 3 '\x7f' ; (* ignored by Qubes *)
    Bytes.unsafe_set s 2 r ;
    Bytes.unsafe_set s 1 g ;
    Bytes.unsafe_set s 0 b ;
    Bytes.to_string s

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
    Ok (String.sub line (offset * color_w) 4)
  else
    Error (`Msg (Fmt.strf "sub_color_line: [%d]: %d" (line_length line) offset))

open Framebuffer.Utils

let horizontal t ~x ~y ~x_end color =
  lwt_for ~start:(min x 0) (min x_end t.width)
    (fun x -> pixel t ~y ~x color)

let draw_line t ~x ~y line : unit Lwt.t =
  let len = line_length line in
  lwt_for len
    (fun l_off ->
      match sub_color_line line l_off with (* TODO *)
      | Ok color -> pixel t ~x:(x+l_off) ~y color
      | Error _ -> failwith "TODO handle / implement")

let rect_lineiter t ~x ~y ~y_end f : unit Lwt.t =
  lwt_for (y_end-y)
    (fun off -> draw_line t ~x ~y:(y+off) (f off))

let rect t ~x ~y ~x_end ~y_end color =
  lwt_for ~start:(min 0 y) (min t.height y_end)
    (fun y -> horizontal t ~x ~y ~x_end color)

let mfndump t =
  (* returns a list of MFNs touched since last mfndump,
     and resets the page touch tracker *)
  (* TODO mark t.touched_pages.(..) <- false after dumping *)
  (* TODO replace this mess, consider just making a (Io_page.t * true) array*)
  Log.warn (fun f -> f "entering mfndump t");
  let mfn_of_iopage p =
    Io_page.get_addr p |> OS.MM.virt_to_mfn |> Nativeint.to_int32 in
  Array.mapi (fun i -> function
  | false -> None
  | true -> Some (mfn_of_iopage t.io_page_arr.(i))
    ) t.touched_pages |> Array.to_list
  |> List.filter (function None -> false | _ -> true)
  |> List.map (function Some x -> x | None -> failwith "TODO implement")

let rec recv_event (t:backend) : Framebuffer__S.backend_event Lwt.t =
  let open Framebuffer__S in
  GUI.recv_event t.window >>= function
  | Button _ -> Lwt.return Mouse_button
  | Keypress _ -> Lwt.return (Keypress : Framebuffer__S.backend_event)
  | Focus _ -> Lwt.return Window_focus
  | Motion _ -> Lwt.return Mouse_motion
  | Clipboard_data (cstruct:Cstruct.t) ->
    Lwt.return (Clipboard_paste (Cstruct.to_string cstruct))
  | Clipboard_request -> Lwt.return Clipboard_request
  | Window_crossing _ -> recv_event t
  | Window_destroy -> Lwt.return Window_close (* TODO actually means "it disappeared" *)
  | Window_close -> Lwt.return Window_close
  | UNIT () -> recv_event t (* TODO ignore*)

let window qubes_t ~width ~height : backend Lwt.t =
  (* 32 bits per pixel, rounded up: *)
  let page_count = ((height * width * (32/8)) |> Io_page.round_to_page_size)
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
       (resolve_ret @@ Qubes.GUI.create_window ~width ~height qubes_t)
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

let redraw t =
    let mfns = mfndump t in
    Log.warn (fun f -> f "MFNS: %d" List.(length mfns)) ;

    let width = Int32.of_int t.width and height = Int32.of_int t.height in
    (* http://xenbits.xen.org/gitweb/?p=xen.git;a=blob;f=xen/include/xen/mm.h;h=88de3c1fa6bb64bde8867ec4b53a18844b099be4;hb=HEAD *)
    let our_mfndump = Formats.GUI.make_msg_mfndump ~domid:0 ~width ~height ~mfns ~window:(GUI.int32_of_window t.window) in
    GUI.send t.qubes [our_mfndump] >>= function
    | `Ok () -> Lwt.return ()
    | `Eof -> error "EOF during MSG_MFNDUMP"

    (*send_wmhints()*)
(*    let our_wmhints = Formats.GUI.make_msg_window_hints ~height ~width in
    QV.send gui [our_wmhints] >>= function
    | `Eof -> Lwt.fail (error "EOF during MSG_WINDOW_HINTS")
    | `Ok () ->
*)
  (* ENDOF process_pv_resize() *)


(*
2017-10-15 14:08:51 -00:00: WRN [qubes.gui] Unexpected data with unknown type: [91 00 00 00 01 00 00 00 08 00 00 00 ] 00 00 00 00 04 00 00 00 a
2017-10-15 14:08:51 -00:00: INF [qubes.gui] debug_window [1]: UNIT

*)

(* keypress:
  x/y: coordinate of the mouse, starting at 0,0: UPPER left corner
  shift: state land 1
  caps-lock: state land 2
  ctrl: state land 4
  left alt: state land 8
  left meta: state land 64

  keycodes:
  a     38
  shift 50
  right-shift 62
  alt   64
  meta  133
  altgr 108
*)
