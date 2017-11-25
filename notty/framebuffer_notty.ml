open Notty

let mods_of_kmods lst : Notty.Unescape.mods =
  let open Framebuffer__Keycodes in
  List.fold_left (fun acc -> function
      | Ctrl -> `Ctrl::acc
      | Mod1 -> `Meta::acc
      | Shift -> `Shift::acc
      | _ -> acc ) [] lst

let event_of_backend_event
  : Framebuffer__S.backend_event -> Notty.Unescape.event list
  = function
    | Keypress {keysym; mods; pressed = false; _ } -> (* when key is released *)
    let notty_mods = mods_of_kmods mods in
    begin match keysym with
      | Some `Left_Arrow -> [`Key (`Arrow `Left, notty_mods)]
      | Some `Right_Arrow -> [`Key (`Arrow `Right, notty_mods)]
      | Some `Up_Arrow -> [`Key (`Arrow `Up, notty_mods)]
      | Some `Down_Arrow -> [`Key (`Arrow `Down, notty_mods)]
      | Some ( `Backspace
             | `Delete
             | `End
             | `Escape
             | `Home
             | `Insert
             | `Tab as sym) -> [`Key (sym, notty_mods)]
      | Some `Page_Down -> [`Key (`Page `Down, notty_mods)]
      | Some `Page_Up -> [`Key (`Page `Up, notty_mods)]
      | Some `Return -> [`Key (`Enter, notty_mods)]
      | Some keysym ->
        Framebuffer__Keycodes.US_keyboard.to_unicode mods keysym
        |> List.map (fun uc -> `Key (`Uchar uc, notty_mods))
      | None -> []
    end |> fun lst ->
    Logs.debug (fun m -> m "keysym len: %d" @@ List.length lst);
    lst
 | Clipboard_paste _data -> (* TODO Notty assumes we get utf-8 pastes? *)
      [`Paste `Start;
       (*`Key (`Uchar ... data ...);*)
       `Paste `End ]

 | Mouse_motion _ -> []
 | Mouse_button _ -> [] (* TODO ignored for now *)
 | _ -> []

(*Notty.Render.to_string cap (80,25) cyana*)

module type Pqwy_term =
sig
  (** Adapted from Notty_unix since there doesn't seem to be a general module
      type defined anywhere. *)

  type t (** terminal type*)

  type fdescr (* Unix: Unix.file_descr*)

  type ret (** return type. unit or unit Lwt.t etc *)

  val create : ?dispose:bool ->
    (* ?nosig:bool -> *)
    ?mouse:bool ->
    window:fdescr ->
 (* ?input:fdescr ->
    ?output:fdescr -> *)
    unit -> t

  val release : t -> ret
  val refresh : t -> ret
  val image : t -> image -> unit
  val cursor : t -> (int * int) option -> ret

  val event : t -> [Unescape.event | `Resize of (int*int) | `End ]
  val pending : t -> bool

  val set_size : t -> (int*int) -> unit
  val size : t -> (int*int)
  val fds : t -> fdescr * fdescr

  val debug_events : t -> unit -> unit Lwt.t
end

module Make : functor (FB : Framebuffer__S.Framebuffer_S) ->
sig
  type fdescr = FB.t (* Unix.file_descr *)
  type ret = unit Lwt.t
  val winsize : fdescr -> (int * int) option
  val output_image : ?cap:Cap.t -> ?clear:bool -> image -> ret
  val output_image_endline : ?cap:Cap.t -> ?clear:bool -> image -> ret
  val output_image_size : ?cap:Cap.t -> ?clear:bool ->
    (int * int -> image) -> ret
  (*val output_image_endline : ?cap:Cap.t -> ?clear:bool -> image -> ret*)
  module Term : Pqwy_term with type fdescr = fdescr and type ret = ret
end
  = functor (FB : Framebuffer__S.Framebuffer_S) ->
  struct

    module Term = struct
      type ret = unit Lwt.t
      type fdescr = FB.t

      type t = {
        trm : Tmachine.t ;
        window : FB.t ;
        mutable text_lines : (int * string array) ;
        mutable image : Notty.image ;
      }

      let set_size t dim = Tmachine.set_size t.trm dim
      let size t = FB.term_size t.window (* TODO Tmachine.size t.trm *)
      let image t image = t.image <- image; Tmachine.image t.trm image

      let output_tty t str =
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
        let (width,height) = size t in
        let offset , lines = t.text_lines in
        Logs.debug (fun m -> m "width: %d height: %d offset: %d" width height offset);
        let append_to_current, line_list =
          let offset_line_len = String.length lines.(offset) in
          match offset_line_len < width, line_wrap width str |> take_last height with
          | true,
            ( ""::tl (* can append, but input is empty *)
            | ([] as tl) (* can append, but we're appending a newline *)
            )
          | false, tl -> "", List.rev tl (* can NOT append; we're on a new line *)
          | true , first::tl ->
            begin match line_wrap (width - offset_line_len)
                          first with
            | first_line::first_tl ->
              Logs.debug (fun m -> m "appending: %S truncating %a new lines: %a"
                             first_line
                             Fmt.(list ~sep:(unit ";") string) first_tl
                             Fmt.(list ~sep:(unit ";") string) tl
                         ); (*TODO the truncating is not good *)
              first_line, List.rev tl
              | [] -> "", List.rev tl
            end
        in
        let() = Logs.info (fun m -> m "appending to current line: %S\nStrs: %a"
                              append_to_current
                           Fmt.(list ~sep:(unit "~") string) line_list) in
        let dim = Array.length lines in
        let lst_dim = List.length line_list in
        let old = max 0 (dim - lst_dim) in (* number of old lines to retain *)
        let dropped = dim - old in
        Logs.debug (fun m -> m "adim: %d lst_dim %d old %d dropped %d" dim lst_dim old dropped);
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
        (* only draw what fits into (size t): *)
        let offset = dim - height in
        let rpad_spaces s =
          let len = String.length s in if len < width
          then String.concat "" [s;String.make (width - len) ' '] else s
        in
        for y = height-1 downto 0 do
          FB.letters t.window (rpad_spaces lines.(y+offset))
            ~x:0 ~y:(y*16) (*TODO 16 = font_h*)
        done ;
        t.text_lines <- (new_offset, lines) ;
        FB.redraw t.window

      let create ?(dispose=true) ?(mouse=true) ~window () =
        let _ = dispose in (* TODO *)
        let _, height = FB.term_size window in
        let t = { trm = Tmachine.create ~mouse ~bpaste:true Notty.Cap.dumb ;
                  window ;
                  text_lines = height-1,
                               Array.init height (* TODO this is debug output:*)
                                 (fun i ->
                                   String.make 3 (Char.chr ((i mod 10)+0x30))) ;
                  image = Notty.I.empty
                } in
        set_size t (size t) ;
        t

      let write t =
        let b = Buffer.create 1000 in
        Tmachine.output t.trm b ;
        let str = Buffer.contents b in
        Logs.debug (fun m -> m "notty: writing %S" str);
        let open Lwt.Infix in
        output_tty t str >>= fun () -> FB.redraw t.window
      let refresh t = Tmachine.refresh t.trm; write t
      let release _t = Lwt.return_unit
      let cursor t (pos:(int*int) option) = Tmachine.cursor t.trm pos; write t
      let event _t : [Unescape.event | `Resize of (int*int) | `End] = `End
      let fds t = t.window, t.window
      let pending _t = true
      let debug_events t () =
        let rec loop () =
          let open Lwt.Infix in
          FB.recv_event t.window >>= function
          | Window_close -> failwith "window closed"
          (* TODO handle Resize *)
          | b_ev ->
            (*Logs.debug (fun m -> m "b_ev: %a" Framebuffer.pp_backend_event b_ev); TODO move out from Framebuffer *)
          Lwt.return (event_of_backend_event b_ev)
          >>= Lwt_list.iter_s (begin function
              | `Key (`Escape, _)-> output_tty t "abcdef\n1234\nyolo567"
              | `Key (`Uchar x, _) ->
                let s = String.make 1 @@ Uchar.to_char x in
                Logs.debug (fun m -> m "outputting %S" s);
                output_tty t s
              | `Key (`Enter, _)->output_tty t "\n"
              (*
              | `Key (`Enter, _) ->
                Logs.debug (fun m -> m "got enter");
                image t Notty.I.(t.image <-> string A.empty "h"); refresh t
              | `Key (`Uchar x, _) ->
                let s = String.make 1 @@ Uchar.to_char x in
                image t Notty.I.(t.image <->
                                 string A.empty (s)); refresh t*)
              | _ -> Lwt.return_unit
            end) >>= loop
          (* TODO *)
        in loop ()
    end

    type fdescr = FB.t
    type ret = unit Lwt.t

  let winsize t = Some (FB.term_size t)
  let output_image ?(cap:Cap.t option) ?(clear=true) image =
    let _ = cap , clear, image in (* TODO *)
    Lwt.return_unit
  let output_image_endline ?cap ?clear img =
    (* TODO Notty seems to have two interfaces for this; see Notty_unix.eol ? *)
    output_image ?cap ?clear img
  let output_image_size ?cap ?clear f =
    let _ = cap, clear, f in (* TODO *)
    Lwt.return_unit
end
