open Lwt.Infix

module Make(FB:Framebuffer.S) = struct
  let pp_events fb () =
    let rec loop () =
      FB.recv_event fb >>= fun ev ->
      (*Logs.app (fun m -> m "%a" Framebuffer.pp_backend_event ev) ;*)
      begin match ev with
        | Keypress {pressed ; scancode; mask; keysym; mods} ->
          begin match keysym, mods with
            | None, _ -> ()
            | Some ks, kmods ->
              let _ = Framebuffer__Keycodes.pp_keysym,
                      ks, kmods, pressed, scancode, mask in
              (*
              Logs.app
                (fun m -> m "parsed keysym: @[down=%b; scancode: %d; mask: %d;\
                             @ sym: %a; mods: %a; char: %a@]"
                    pressed scancode mask
                    Framebuffer__Keycodes.pp_keysym ks
                    Fmt.(list ~sep:(unit "; ") pp_kmod) kmods
                    Fmt.(list ~sep:(unit ", ") char)
                    (US_keyboard.to_unicode kmods ks |> List.map Uchar.to_char)
                )
*)
              ()
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
    let size = Size2.v 120. 50. in
    let view = Box2.unit in
    let red = I.const Color.red in
    let box =
      P.empty
      |> P.rect @@ Gg.Box2.of_pts (P2.v 5.0 10.0) (P2.v 30.0 25.) in
    let red_box = I.cut box red in
    let circle = P.empty |> P.circle (P2.v 50. 15.) 7. in
    let red_circle = I.cut circle red_box in
    let _ = red_circle in
    let triangle =
      let v1 = P2.v 80. 40. in
      let v2 = P2.v 100. 10. in
      let v3 = P2.v 120. 40. in
      P.empty |> P.sub v1 |> P.line v2 |> P.line v3 |> P.close  in
    let blue_triangle = I.cut triangle (I.const Color.blue) in

    let five =
      let v1 = P2.v 35. 40. in
      let v2 = P2.v 45. 10. in
      let v3 = P2.v 55. 20. in
      let v4 = P2.v 65. 10. in
      let v5 = P2.v 75. 25. in
      let v6 = P2.v 65. 35. in
      P.empty |> P.sub v1 |> P.line v2 |> P.line v3 |> P.line v4
      |> P.line v5 |> P.line v6 |> P.close  in
    let green_five = I.cut five (I.const Color.green) in

    let product = I.blend red_circle blue_triangle |> I.blend green_five in
    ignore (Vgr.render r (`Image (size, view, product)));
    ignore (Vgr.render r `End) ;
    FB.redraw window

  let rec finish fb () =
    FB.recv_event fb >>= function
    | Window_close -> Lwt.return_unit
    | _ -> finish fb ()

  let run () =
      FB.window ~width:200 ~height:200 >>= fun fb ->
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
