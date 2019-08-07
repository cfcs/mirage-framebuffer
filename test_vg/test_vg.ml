open Lwt.Infix

module Make(FB:Framebuffer.S) = struct
  let pp_events fb () =
    let rec loop () =
      FB.recv_event fb >>= fun ev ->
      Logs.app (fun m -> m "%a" Framebuffer.pp_backend_event ev) ;
      let open Framebuffer__Keycodes in
      begin match ev with
        | Keypress {pressed ; scancode; mask; keysym; mods} ->
          begin match keysym, mods with
            | None, _ -> ()
            | Some ks, kmods ->
              Logs.app
                (fun m -> m "parsed keysym: @[down=%b; scancode: %d; mask: %d;\
                             @ sym: %a; mods: %a; char: %a@]"
                    pressed scancode mask
                    Framebuffer__Keycodes.pp_keysym ks
                    Fmt.(list ~sep:(unit "; ") pp_kmod) kmods
                    Fmt.(list ~sep:(unit ", ") char)
                    (US_keyboard.to_unicode kmods ks |> List.map Uchar.to_char)
                )
          end
        | _ -> ()
      end ;
      loop ()
    in loop ()

  let vg_stuff window =
    let open Gg in
    let open Vg in
    let module VG = Framebuffer_vg.Vgr_mirage_framebuffer(FB) in
    let r = Vgr.create (VG.target ~window ()) (`Buffer (Buffer.create 0)) in
    let size = Size2.v 100. 30. in
    let view = Box2.unit in
    let circle =
      P.empty
      |> P.rect @@ Gg.Box2.of_pts (P2.v 0.2 0.6) (P2.v 0.3 0.5) in
    let red = I.const Color.red in
    let red_box = I.cut circle red in
    ignore (Vgr.render r (`Image (size, view, red_box)));
    ignore (Vgr.render r `End) ;
    Lwt.return ()

  let rec finish fb () =
    FB.recv_event fb >>= function
    | Window_close -> Lwt.return_unit
    | _ -> finish fb ()

  let run () =
      FB.window ~width:100 ~height:100 >>= fun fb ->
      Lwt.async (pp_events fb);
      vg_stuff fb >>=
      finish fb

end

let main =
  let module A =  Framebuffer.Make(Framebuffer_tsdl) in
  A.init() >>= fun backend ->
  let module FB : Framebuffer.S = (val (backend)) in

  let module T = Make(FB) in
  T.run ()

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter () ;
  Logs.(set_level @@ Some Debug);
  Lwt_main.run main
