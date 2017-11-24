open Notty

let mods_of_kmods lst : Notty.Unescape.mods =
  let open Framebuffer__Keycodes in
  List.fold_left (fun acc -> function
      | Ctrl -> `Ctrl::acc
      | Mod1 -> `Meta::acc
      | Shift -> `Shift::acc
      | _ -> acc ) [] lst

let event_of_backend_event : Framebuffer__S.backend_event -> Notty.Unescape.event list
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
        text_lines : string array ;
        mutable image : Notty.image ;
      }

      let set_size t dim = Tmachine.set_size t.trm dim
      let size t = FB.term_size t.window (* TODO Tmachine.size t.trm *)
      let image t image = t.image <- image; Tmachine.image t.trm image

      let output_tty t str =
        let (width,height) = size t in
        let rpad_spaces s =
          (* TODO verify that notty takes care of this
          let len = String.length s in if len < width
          then String.concat "" [s;String.make (width - len) ' ']
          else*) s
        in
        let new_lines = String.split_on_char '\n' in
        let rec fill acc = function
          | [] -> acc
          | hd::tl when String.length hd > width ->
            let hdl = String.length hd in
            fill ((String.sub hd (hdl-width) width)::acc)
              ((String.sub hd 0 (hdl-width))::tl)
          | hd::tl -> fill (hd::acc) tl
        in
        let take_last n lst =
          let rec loop acc i = function
            | [] -> acc | _ when i <= 0 -> acc
            | hd::tl -> loop (hd::acc) (pred i) tl
          in loop [] n (List.rev lst)
        in
        let line_list =
          new_lines str |> fill [] |> take_last height |> List.map rpad_spaces
        in
      let() = Logs.info (fun m -> m "Strs: %a"
                           Fmt.(list ~sep:(unit "~")string) line_list) in
        let dim = Array.length t.text_lines in
        let lst_dim = List.length line_list in
        for i = 0 to dim - 1 do (* 0 -> (10-1)-2 = 8*)
          if 0 > i - lst_dim then () (* discard *)
          else begin
            (* shift previous line *)
            t.text_lines.(i - lst_dim) <- t.text_lines.(i)
          end
        done ;
        for i = dim-1 downto max 0 (dim - lst_dim) do (*(10-1) to 10 - 2*)
          (* insert new content *)
          t.text_lines.(i) <- List.nth line_list (dim-1-i)
        done;
        (* only draw what fits into (size t): *)
        let offset = dim - height in
        for y = height-1 downto 0 do
          FB.letters t.window t.text_lines.(y+offset)
            ~x:0 ~y:(y*16) (*TODO 16 = font_h*)
        done ;
        FB.redraw t.window

      let create ?(dispose=true) ?(mouse=true) ~window () =
        let _ = dispose in (* TODO *)
        let _, height = FB.term_size window in
        let t = { trm = Tmachine.create ~mouse ~bpaste:true Notty.Cap.dumb ;
                  window ;
                  text_lines = Array.init height (* TODO this is debug output:*)
                      (fun i -> String.make 3 (Char.chr ((i mod 10)+0x30))) ;
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
              | `Key (`Enter, _) ->
                Logs.debug (fun m -> m "got enter");
                image t Notty.I.(t.image <-> string A.empty "h"); refresh t
              | `Key (`Uchar x, _) ->
                let s = String.make 1 @@ Uchar.to_char x in
                image t Notty.I.(t.image <|>
                                 string A.empty (s)); refresh t
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
