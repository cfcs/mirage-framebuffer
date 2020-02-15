open Lwt.Infix

let mouse_callbacks = ref []

module Make(FB:Framebuffer.S) = struct
  let pp_events fb () =
    let rec loop () =
      FB.recv_event fb >>= begin function
        | Framebuffer.S.Mouse_motion {x;y} ->
          Lwt_list.iter_s (fun f -> f x y) !mouse_callbacks
        | Framebuffer.S.Resize (width,height) ->
          FB.resize ~width ~height fb
        | _ -> Lwt.return_unit
      end >>= loop
    in loop ()

  let vg_stuff window =
    let open Gg in
    let open Vg in
    let module VG = Framebuffer_vg.Vgr_mirage_framebuffer(FB) in
    let view = Box2.unit in
    let red = I.const Color.red in
    let mouth =
      let p = P.empty
              |> P.sub (P2.v 150. 150.)
              |> P.qcurve ~rel:true (P2.v 10. 30.) (P2.v 50. 0.)
              |> P.qcurve ~rel:true (P2.v ~-.10. ~-.30.) (P2.v ~-.50. 0.)
      in I.cut p red
    in
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
    (List.hd !mouse_callbacks) 0 0

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
  (*Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter () ;
  *)
  Logs.(set_level None);
  Lwt_main.run main
