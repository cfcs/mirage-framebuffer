open Lwt.Infix

let mouse_callbacks = ref []
let new_size = ref (0,0)

module Make(FB:Framebuffer.S) = struct
  let pp_events fb () =
    let rec loop () =
      FB.recv_event fb >>= fun ev ->
      (*Logs.app (fun m -> m "%a" Framebuffer.pp_backend_event ev) ;*)
      begin match ev with
        | Keypress {pressed ; scancode; mask; keysym; mods} ->
          begin match keysym, mods with
            | None, _ -> Lwt.return_unit
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
              Lwt.return_unit
          end
        | Framebuffer.S.Mouse_motion {x;y} ->
          Lwt_list.iter_s (fun f -> f x y) !mouse_callbacks
        | Framebuffer.S.Resize (width,height) ->
          new_size := (width,height);
          FB.resize ~width ~height fb
        | _ -> Lwt.return_unit
      end >>= fun() ->
      loop ()
    in loop ()

  let vg_stuff window =
    let open Gg in
    let open Vg in
    let module VG = Framebuffer_vg.Vgr_mirage_framebuffer(FB) in
    let r = Vgr.create (VG.target ~window ()) (`Buffer (Buffer.create 32)) in
    let size = Size2.v 500. 500. in
    let view = Box2.unit in
    let red = I.const Color.red in
    let box =
      P.empty
      |> P.rect @@ Gg.Box2.of_pts (P2.v 5.0 20.0) (P2.v 10.0 10.) in
    let _ = box in let box = P.empty in
    let red_box = I.cut box red in
    let _ = red_box in
    let mouth = P.empty
                |> P.sub (P2.v 150. 150.)
                |> P.qcurve ~rel:true (P2.v 10. 30.) (P2.v 50. 0.)
                |> P.qcurve ~rel:true (P2.v ~-.10. ~-.30.) (P2.v ~-.50. 0.)
    in
    let mouth = I.cut mouth red in
    let eyes =
      let cut_white p = I.cut p (I.const Color.white) in
      let eye_1 = P.empty
              |> P.sub (P2.v 145. 110.)
              |> P.ellipse ~rel:true (P2.v 0. 0.) (P2.v 14. 8.) in
      let eye_2 start =
        start
        |> P.sub (P2.v 200. 105.)
        |> P.ellipse ~rel:true (P2.v 0. 0.) (P2.v 14. 8.) in
      let _expected =
        (* should draw two separate circles like below, test-case *)
        I.blend (cut_white eye_1) (cut_white (eye_2 P.empty)) in
      cut_white (eye_2 eye_1)
    in
    let eye_1_pos = P2.v 145. 110. in
    let eye_2_pos = P2.v 200. 105. in
    let eyes ?(mod1=P2.v 0. 0.) ?(mod2=P2.v 0. 0.) () =
      let cut_black p = I.cut p (I.const Color.black) in
      let eye_1 p = p |> P.sub eye_1_pos
                    |> P.circle ~rel:true (mod1) 3. in
      let eye_2 p = p |> P.sub eye_2_pos
                    |> P.circle ~rel:true (mod2) 3. in
      let dots = I.blend (cut_black @@ eye_1 P.empty)
          (cut_black @@ eye_2 P.empty) in
      I.blend eyes dots
    in
    let () =
      let f x y =
        let mouse = P2.v (float x) (float y) in
        let cmod pos =
          let d1 = V2.sub mouse pos in
          let a1 = V2.angle d1 in
          V2.v (5. *. cos a1) (5. *. sin a1) in
        let product = I.blend mouth (eyes
                                       ~mod1:(cmod eye_1_pos)
                                       ~mod2:(cmod eye_2_pos) () ) in
        let size = let x,y = FB.dim window in P2.v (float x) (float y) in
        let r = Vgr.create (VG.target ~window ()) (`Buffer (Buffer.create 32)) in
        ignore (Vgr.render r (`Image (size, view, product)));
        ignore (Vgr.render r `End) ;
        FB.redraw window
      in
      mouse_callbacks := f :: !mouse_callbacks ;
    in

    let circle = P.empty
                 (*|> P.qcurve (P2.v 120. 30.) (P2.v 16. 40.)*)

                 |> P.sub (P2.v 150. 150.)
                 (*|> P.ellipse ~rel:true
                   ~angle:(Float.rad_of_deg 10.) (P2.v 0. 0.) (P2.v 40. 20.)
                 *)

                 |> P.qcurve ~rel:true (P2.v 10. 30.) (P2.v 50. 0.)
                 |> P.qcurve ~rel:true (P2.v ~-.10. ~-.30.) (P2.v ~-.50. 0.)

                 (*|> P.sub (P2.v 158. 150.)
                 |> P.earc ~large:false ~cw:false ~angle:Stdlib.Float.epsilon
                   (P2.v 8. 5.) (P2.v 142. 150.)
                   |> P.close*)
(*
                 |> P.earc ~large:false ~cw:false ~angle:Stdlib.Float.epsilon
                   (P2.v 8. 5.) (P2.v 158. 150.)*)
                   (*
                   (Sub 
                      (0 0))
                   (Close, should draw until Sub)
                   (Earc large:false clockwise:false angle:0.000000 radii:8.000000x5.000000 pt:
                      (158 150))
                   (Earc large:false clockwise:false angle:0.000000 radii:8.000000x5.000000 pt:
                      (142 150))
                   (Sub 
                      (158 150))
                   (Close, should draw until Sub)
                   (Sub 
                      (150 150)))*)

                 (*|> P.earc ~large:false ~cw:false ~angle:0.1
                   (P2.v 8. 5.) (P2.v 142. 150.)
                                |> P.line (P2.v 140. 140.)*)
               |> P.close (*
                 |> P.line (P2.v 90. 30.)
                 |> P.line (P2.v 40. 40.)
                 |> P.close*) in
    (*let _, circle = circle,
                    P.sub P2.(v 70. 40.) P.empty
                    |> P.earc (P2.v 80. 30.) (P2.v 10. 10.)
                    (*|> P.line (P2.v 90. 40.)*)
                    (*|> P.earc (P2.v 10. 20.) (P2.v 50. 20.)*)
                    |> P.line (P2.v 90. 20.)
                    |> P.close
      in*)
    (*let circle =
      P.empty
      |> P.sub (P2.v 100. 40.)
      |> P.line (P2.v 20. 20.)
      (*|> P.qcurve (P2.v 4. 18.) (P2.v 18. 18.)*)
      (*|> P.circle (P2.v 90. 35.) 10.*)
      (*|> P.earc ~cw:true ~large:true (P2.v 10. 10.) (P2.v 107. 42.)*)
      |> P.line (P2.v 20. 30.)
      |> P.line (P2.v 40. 30.)
      |> P.close
      in*)
    let white_circle = I.cut circle (I.const Color.white) in
    let white_circle_with_dots =
      let p = P.empty
              |> P.sub (P2.v 150. 150.)
              |> P.rect ~rel:true (Box2.add_pt Box2.zero (P2.v 0. 0.))
      in I.blend white_circle (I.cut p (I.const Color.red))
    in
    let triangle =
      let v1 = P2.v 80. 40. in
      let v2 = P2.v 100. 10. in
      let v3 = P2.v 120. 40. in
      P.empty |> P.sub v1 |> P.line v2 |> P.line v3 |> P.close  in
    let blue_triangle = I.cut triangle (I.const Color.blue) in
    let _ = blue_triangle in
    (*let _, blue_triangle =
      blue_triangle,
      (*(P.empty |> P.rrect (Gg.Box2.of_pts (P2.v 5.0 20.0) (P2.v 30. 10.))
        (P2.v 5.0 5.0) |> I.cut) (I.const Color.blue)*)
      I.cut P.empty (I.const Color.blue)
      in*)

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
    let _ = green_five in
    let v6 =
      let v1 = P2.v 90. 50. in
      let v2 = P2.v 90. 20. in
      let v3 = P2.v 115. 20. in
      let v4 = P2.v 115. 35. in
      let v5 = P2.v 130. 35. in
      let v6 = P2.v 130. 20. in
      let v7 = P2.v 150. 20. in
      let v8 = P2.v 150. 50. in
      let v9 = P2.v 90. 50. in
      P.empty |> P.sub v1 |> P.line v2 |> P.line v3 |> P.line v4
      |> P.line v5 |> P.line v6 |> P.line v7 |> P.line v8
    |> P.line v9
    in
    let red_v6 = I.cut v6 (I.const Color.red) in
    let _ = red_v6, white_circle_with_dots  in
    (*let green_five = I.cut P.empty (I.const Color.green) in*)
    let product = (*I.blend red_box blue_triangle*)
                  (*|> I.blend green_five*)
      (*I.blend red_v6 blue_triangle
      |> I.blend green_five
        |> I.blend *)
      I.blend (eyes ()) mouth
      (*white_circle_with_dots*)
                  (*|> I.blend white_circle*)
    in
    ignore (Vgr.render r (`Image (size, view, product)));
    ignore (Vgr.render r `End) ;
    FB.redraw window

  let rec finish fb () =
    FB.recv_event fb >>= function
    | Window_close -> Lwt.return_unit
    | _ -> finish fb ()

  let run () =
      FB.window ~width:500 ~height:500 >>= fun fb ->
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
