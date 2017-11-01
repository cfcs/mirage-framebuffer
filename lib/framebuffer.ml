open Lwt.Infix

type uchar = int
(* let screenshot t = TODO *)

module type Backend_S = S.Backend_S
module type S = S.Framebuffer_S

module Utils = struct
  let lwt_for ?(start=0) stop f =
    let rec loop = function
      | i when i = stop -> Lwt.return_unit
      | i -> f i >>= fun () -> loop (succ i)
    in loop start
end

let pp_backend_event (fmt:Format.formatter) : S.backend_event -> unit = function
  | Clipboard_paste str -> Fmt.pf fmt "Clipboard_paste: %S" str
  | Clipboard_request -> Fmt.pf fmt "Clipboard_request"
  | Keypress -> Fmt.pf fmt "Keypress"
  | Mouse_button -> Fmt.pf fmt "Mouse_button"
  | Mouse_motion -> Fmt.pf fmt "Mouse_motion"
  | Window_close -> Fmt.pf fmt "Window_close"
  | Window_focus -> Fmt.pf fmt "Window_focus"
  | Resize (w,h) -> Fmt.pf fmt "Resize: %d x %d" w h

module Make : S.Framebuffer_M = functor (Backend : S.Backend_S) ->
struct
  let init init_t =
    Backend.init_backend init_t >>= fun () ->
    let module Frontend : S.Framebuffer_S with
      type color = Backend.color and type line = Backend.line
    = struct
  type color = Backend.color
  type line = Backend.line
  type font_table = (uchar, Backend.line array) Hashtbl.t
  type t = {
    mutable height : int ;
    mutable width  : int ;
    b : Backend.backend;
    mutable text_lines : string list;
    font : font_table
  }

module Log : Logs.LOG = (val Logs.src_log (Logs.Src.create "framebuffer"
                                             ~doc:"Mirage.Framebuffer"))

let compile_rgb ?r ?g ?b t = Backend.Compile.rgb ?r ?g ?b t.b
let compile_line colors t = Backend.Compile.line colors t.b

(* load bitmap font *)
let font_w = 8
and font_h = 16

let compile_font backend : font_table =
  let color = Backend.Compile.rgb ~r:'\x10' ~g:'\x10' ~b:'\x10' backend
  and blank = Backend.Compile.rgb ~r:'\xff' ~g:'\xff' ~b:'\xff' backend
  and font : (uchar, Backend.line array) Hashtbl.t = Hashtbl.create 256
  and codepoint_w = 2 + font_h in
  let () =(
    let str = match Bitmapfont.read "single-chars.bitmap" with
      | Some s -> s | _ -> failwith "unable to load bitmap content" in
    let pixels_of_byte byte =
      backend |> (font_w |> Backend.Compile.lineiter
        (fun i -> if byte land (1 lsl (font_w-i-1)) > 0 then color else blank))
    in
    for chari = 0 to (String.(length str)-1) / codepoint_w do
      let t = Cstruct.create codepoint_w in
      let () = Cstruct.blit_from_string str (chari*codepoint_w)
                                        t  0  codepoint_w in
      Hashtbl.add font Cstruct.BE.(get_uint16 t 0) (*uchar*)
        Array.(init font_h (fun i -> pixels_of_byte @@
            Cstruct.(get_uint8 t (2+i)))) (* 2 = codepoint uint16 *)
    done
  ) in font

let t_iteri t t_len t_get f =
  let stop = t_len t in
  let open Lwt.Infix in
  let rec loop = function
  | i when i = stop -> Lwt.return_unit
  | i -> f i (t_get i) >>= fun () -> loop (succ i)
  in loop 0

(*let arrayiteri arr f = t_iteri arr Array.length (Array.get arr) f*)

let stringiteri str f = t_iteri str String.length (String.get str) f

let listiteri lst f =
  let rec loop i = function
    | [] -> Lwt.return_unit
    | hd::tl -> f i hd >>= fun () -> loop (succ i) tl
  in loop 0 lst

let redraw t = Backend.redraw t.b

let pixel t ~x ~y = Backend.pixel t.b ~x ~y

let rect t ~x ~y ~x_end ~y_end color = Backend.rect t.b ~x ~y ~x_end ~y_end color
let rect_lineiter t ~x ~y ~y_end f = Backend.rect_lineiter t.b ~x ~y ~y_end f

let letter (t:t) codepoint ~x ~y =
  (* draws a letter occupying pixels x -> x+8 ; y -> y+16*) (*
  assert (0 < x);
  assert (x <= t.width);
  assert (0 <= y);
  assert (x <= t.height);*)

  let letter = Hashtbl.(find t.font codepoint) in
  let y_end = min t.height (y+font_h) in
  rect_lineiter t ~x ~y ~y_end (fun i -> letter.(i))

let letters t str ~x ~y =
    stringiteri str
      (fun i -> fun ch ->
         let x2 = (min (t.width -8) (x+(8*i))) in
         let y2 = (min y (t.height - 16)) in
   (*Log.warn (fun m -> m "i: %d x: (%d) %d y: (%d) %d c: %C" i x2 x y2 y ch);*)
      letter t (int_of_char ch) ~x:x2
                                ~y:y2
    )

  let term_size t = (t.width / 8) , (t.height /16)

  let readable t str =
    let (width,height) = term_size t in
    let lines =
    let take_last n lst =
      let rec loop acc i = function
        | [] -> acc | _ when i <= 0 -> acc
        | hd::tl -> loop (hd::acc) (pred i) tl
      in loop [] n (List.rev lst)
    in
    let lines = take_last height @@
      ((String.split_on_char '\n' str) |> List.rev) @ t.text_lines
    in
    let rec fill acc = function
      | [] -> acc
      | hd::tl when String.length hd > width ->
        let hdl = String.length hd in
        fill ((String.sub hd (hdl-width) width)::acc)
             ((String.sub hd 0 (hdl-width))::tl)
      | hd::tl -> fill (hd::acc) tl
    in fill [] lines |> take_last height |> List.rev_map
         (fun s -> let len = String.length s in if len < width
           then String.concat "" [s;String.make (width - len) ' ']
           else s) in
    let() = Log.info (fun m -> m "Strs: %a" Fmt.(list ~sep:(unit "~")string) lines) in
    let() = t.text_lines <- lines in
    listiteri t.text_lines (fun y line -> letters t line ~x:0 ~y:(y*16))
      >>= fun () -> redraw t

  let internal_resize t ~width ~height =
    t.width <- width ;
    t.height <- height ;
    Lwt.return_unit

  let window ~width ~height : t Lwt.t =
    let _ = internal_resize (* TODO *) in
    Backend.window init_t ~height:height ~width:width
    >>= fun backend ->
    let t : t = {height; width; b = backend ;
                 text_lines = [] ; font = compile_font backend } in
    Lwt.return t

  let recv_event t = Backend.recv_event t.b

  (* destroy_window: SDL.destroy_window t.b / ...*)

end in
    Lwt.return (module Frontend : S)
end
