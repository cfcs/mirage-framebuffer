open Lwt.Infix

type uchar = int
(* let screenshot t = TODO *)

module S = Framebuffer__S
module Keycodes = Framebuffer__Keycodes

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
  | Keypress {pressed; scancode; mask; mods; keysym;} ->
    Fmt.pf fmt "Keypress {pressed: %b scancode: %d mask: %d; \
                          mods: [%a] ; keysym: %a}"
      pressed scancode mask
      Fmt.(list ~sep:(unit "; ") Keycodes.pp_kmod) mods
      Fmt.(option Keycodes.pp_keysym) keysym
  | Mouse_button {x;y} -> Fmt.pf fmt "Mouse_button (x: %d, y: %d)" x y
  | Mouse_motion {x;y} -> Fmt.pf fmt "Mouse_motion (x: %d, y: %d)" x y
  | Window_close -> Fmt.pf fmt "Window_close"
  | Window_focus -> Fmt.pf fmt "Window_focus"
  | Resize (w,h) -> Fmt.pf fmt "Resize: %d x %d" w h

module Make : S.Framebuffer_M = functor (Backend : S.Backend_S) ->
struct
  let init init_t =
    Backend.init_backend init_t >>= fun () ->
    let module Frontend : S.Framebuffer_S with
      type color = Backend.color and type line = Backend.line
    =
    struct
      type color = Backend.color
      type line = Backend.line
      type font_table = (uchar, Backend.line array) Hashtbl.t

      type t = {
        mutable height : int ;
        mutable width  : int ;
        b : Backend.backend;
        mutable text_lines : (int * string array);
        font : font_table
      }

      module Log : Logs.LOG = (val Logs.src_log (Logs.Src.create "framebuffer"
                                                   ~doc:"Mirage.Framebuffer"))

      let [@inline always] compile_rgb ?r ?g ?b t = Backend.Compile.rgb ?r ?g ?b t.b
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
                          (fun i -> if byte land (1 lsl (font_w-i-1)) > 0
                                    then color else blank))
          in
          for chari = 0 to (String.(length str)-1) / codepoint_w do
            let t = Cstruct.create codepoint_w in
            let () = Cstruct.blit_from_string str (chari*codepoint_w)
                t  0  codepoint_w in
            Hashtbl.add font Cstruct.BE.(get_uint16 t 0) (*uchar*)
              Array.(init font_h (fun i -> pixels_of_byte @@
                                   Cstruct.(get_uint8 t (2+i))))
              (* 2 = codepoint uint16 *)
          done
        ) in font

      let t_iteri t t_len t_get f =
        let stop = t_len t in
        let rec loop = function
          | i when i = stop -> ()
          | i -> f i (t_get i) ; loop (succ i)
        in loop 0

      (*let arrayiteri arr f = t_iteri arr Array.length (Array.get arr) f*)

      let stringiteri str f = t_iteri str String.length (String.get str) f

      let keysym_of_scancode = Backend.keysym_of_scancode

      let kmod_of_constant = Backend.kmod_of_constant

      let redraw t = Backend.redraw t.b

      let [@inline] pixel t ~x ~y = Backend.pixel t.b ~x ~y

      let line_bresenham ?(cb=pixel) t ~x ~y ~x_end ~y_end color =
        let x1 = x_end and y1 = y_end in
        let dx = abs (x1-x) in
        let sx = if x < x1 then 1 else ~-1 in
        let dy = ~-(abs(y1-y)) and sy = if y < y1 then 1 else ~-1 in
        let _ = sx,sy in
        let rec for_loop x ~err ~y ~d =
          cb t ~x ~y color ;
          if x = x_end && y = y_end then ()
          else begin
            let e2 = 2 * err in
            let err, x = if e2 >= dy
              then err + dy, x + sx
              else err, x in
            let err, y = if e2 <= dx
              then err + dx, y + sy
              else err, y in
            for_loop x ~err ~y ~d:(d+2*dy)
          end
        in
        let err = dx+dy in
        for_loop x ~err ~y ~d:(2*dy - dx)

      let line_bresenham_antialias t ~x ~y ~x_end ~y_end width _color =
        (* adapted from C-code at
           http://members.chello.at/~easyfilter/bresenham.html *)
        assert (width > 0);
        let x1 = x_end and y1 = y_end in
        let dx = abs (x1-x)
        and sx = if x < x1 then 1 else ~-1 in

        let dy = abs (y1-y)
        and sy = if y < y1 then 1 else ~-1 in

        let ed : float = if dx+dy = 0 then 1. else
            let dx' = float dx and dy' = float dy in
            sqrt @@ dx' *. dx' +. (dy' *. dy')
        in
        let wd = ((float width +. 1.) /. 2.) in

        let exception Break in
        let err = ref (dx-dy) in
        let e2 = ref 0 and x2 = ref 0 and y2 = ref 0 in
        let y0 = ref y and x0 = ref x in

        let [@inline]plot_pixel x y =
          let r = 255 - max 0 (
              (int_of_float (255. *. (
                   ( (float @@ abs !e2) /. ed) -. wd +. 1.)))) in
          let color =
            Backend.Compile.rgb ~r:(char_of_int r) t.b in
          pixel t ~x ~y color ;
        in

        try
          while true do
            pixel t ~x:!x0 ~y:!y0
              (t.b |> Backend.Compile.rgb ~r:(
                  char_of_int @@ 255 - (
                      max 0 @@ 255 *
                               (int_of_float (
                                   (float @@ abs (!err-dx+dy)) /. ed
                                   -. wd +. 1.))
                    ))) ;
            e2 := !err ; x2 := !x0 ;

            if 2 * !e2 >= ~-dx then begin
              (* handle x-axis progression*)
              e2 := !e2 + dy ;
              y2 := !y0 ;
              while float !e2 < ed *. wd
                    && (y1 <> !y2 || dx > dy) do
                y2 := !y2 + sy ;
                plot_pixel !x0 !y2 ;
                e2 := !e2 + dx ; (* loop terminator*)
              done ;
              if !x0 = x1 then raise_notrace Break ;
              e2 := !err ; err := !err - dy ; x0 := !x0 + sx ;
            end ;

            if 2 * !e2 <= dy then begin
              (* handle y-axis progression *)
              e2 := dx - !e2 ;
              while float !e2 < ed *. wd && (x1 <> !x2 || dx < dy) do
                x2 := !x2 + sx ;
                plot_pixel !x2 !y0 ;
                e2 := !e2 + dy ;
              done ;
              if !y0 = y1 then raise_notrace Break ;
              err := dx ; y0 := !y0 + sy;
            end
          done
        with Break -> ()

      let rect t ~x ~y ~x_end ~y_end color =
        Backend.rect t.b ~x ~y ~x_end ~y_end color

      let rect_lineiter t ~x ~y ~y_end f =
        Backend.rect_lineiter t.b ~x ~y ~y_end f

      let letter (t:t) codepoint ~x ~y =
        (* draws a letter occupying pixels x -> x+8 ; y -> y+16*)

        assert (0 <= x);
        assert (x <= t.width);
        assert (0 <= y);
        assert (x <= t.height);

        let letter = try Hashtbl.(find t.font codepoint) with
        _ -> failwith ("letter lookup failed for "^string_of_int codepoint)  in
        let y_end = min t.height (y+font_h) in
          rect_lineiter t ~x ~y ~y_end (fun i -> letter.(i) )

      let letters t str ~x ~y =
        stringiteri str
          (fun i -> fun ch ->
             let x2 = (min (t.width -8) (x+(8*i))) in
             let y2 = (min y (t.height - 16)) in
             (*Log.warn (fun m -> m "i: %d x: (%d) %d y: (%d) %d c: %C"
                                     i    x2    x    y2    y  ch);*)

             letter t (int_of_char ch) ~x:x2 ~y:y2
          )

      let dim t = t.width, t.height

      let term_size t = (t.width / font_w) , (t.height / font_h)

      let resize ~width ~height t =
        t.width <- width ;
        t.height <- height ;
        Backend.resize ~width ~height t.b

      let window ~width ~height : t Lwt.t =
        Backend.window init_t ~height:height ~width:width
        >>= fun backend ->
        let tty_backlog_size = 1000 in
        let t : t = {height; width; b = backend ;
                     text_lines = tty_backlog_size -
                                  height / font_h,
                                  Array.make tty_backlog_size "" ;
                     font = compile_font backend } in
        Lwt.return t

      let recv_event t = Backend.recv_event t.b

      (* destroy_window: SDL.destroy_window t.b / ...*)

      let output_tty (t:t) (width,height) str =
        let line_wrap (width:int) str =
          assert (width <> 0);
          let rec wrap acc = function
            | [] -> acc
            | hd::tl when String.length hd > width ->
              let hdl = String.length hd in
              wrap ((String.sub hd (hdl-width) width)::acc)
                ((String.sub hd 0 (hdl-width))::tl)
            | hd::tl -> wrap (hd::acc) tl
          in wrap [] @@ String.split_on_char '\n' str
        in
        let take_last n lst =
          let rec loop acc i = function
            | [] -> acc | _ when i <= 0 -> acc
            | hd::tl -> loop (hd::acc) (pred i) tl
          in loop [] n (List.rev lst)
        in
        let offset , lines = t.text_lines in
        Logs.debug (fun m -> m "width: %d height: %d offset: %d"
                                     width    height     offset);
        let append_to_current, line_list =
          let offset_line_len = String.length lines.(offset) in
          match offset_line_len < width,
                List.rev (line_wrap width str |> take_last height) with
          | true, ( ""::tl (* can append, but input is empty *)
                  | ([] as tl) (* can append, but we're appending a newline *)
                  )
          | false, tl -> "", tl (* can NOT append; we're on a new line *)
          | true , first::tl when
              (offset_line_len + String.length first) <= width ->
            first, tl
          | true, first::tl -> (* first is too long*)
            let take_len = width - offset_line_len in
            let first_line = String.sub first 0 take_len in
            let first_tl =
              String.sub first take_len (String.length first - take_len) in
            first_line, first_tl::tl
        in
        let() = Logs.info (fun m -> m "appending to current line: %S\nStrs: %a"
                              append_to_current
                           Fmt.(list ~sep:(unit "~") string) line_list) in
        let dim = Array.length lines in
        let lst_dim = List.length line_list in
        let old = max 0 (dim - lst_dim) in (* number of old lines to retain *)
        let dropped = dim - old in
        Logs.debug (fun m -> m "adim: %d lst_dim %d old %d dropped %d"
                                     dim    lst_dim    old    dropped);
        Logs.debug (fun m -> m "Current array: %a"
                       Fmt.(array ~sep:(unit ";") string) lines);
        for i = dropped to dim-1 do (* 0 -> (10-1)-2 = 8*)
            (* shift previous line *)
          lines.(i-dropped) <-
            (if i = dim-1 then lines.(i) ^ append_to_current
               else lines.(i))
        done ;
        Logs.debug (fun m -> m "Current array shifted: %a"
                       Fmt.(array ~sep:(unit ";") string) lines);

        let new_offset = min (dim-1) (offset + lst_dim) in
        for i = old to dim-1 do
          lines.(i) <- List.nth line_list (i-old)
        done ;
        Logs.debug (fun m -> m "New array: %a"
                       Fmt.(array ~sep:(unit ";") string) lines);

        (* only draw what fits into (size t): *)
        let offset = dim - height in
        let rpad_spaces s =
          let len = String.length s in if len < width
          then String.concat "" [s;String.make (width - len) ' '] else s
        in
        for y = height-1 downto 0 do
          letters t (rpad_spaces lines.(y+offset))
            ~x:0 ~y:(y*16) (*TODO 16 = font_h*)
        done ;
        t.text_lines <- (new_offset, lines) ;
        redraw t

    end
    in (* pack the module and return from [Make.init] *)
    Lwt.return (module Frontend : S)
end
