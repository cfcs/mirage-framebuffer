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

  let rainbow fb () =
    let img = ImageLib.openfile "test_tsdl/rainbow.png" in
    let module IMG = Framebuffer_image.Make(FB) in
    IMG.draw_image fb img

  let draw_pixels fb () =
    let red = FB.compile_rgb ~r:'\xff' fb in
    let green = FB.compile_rgb ~g:'\xff' fb in
    let line = FB.compile_line [red;red;red;red;red;green;green;green] fb in
    FB.pixel fb ~x:10 ~y:10 red >>= fun()->
    FB.rect_lineiter fb ~x:15 ~y:10 ~y_end:11 (fun _ -> line)
    >>= fun () -> FB.redraw fb

  let draw_a_letter fb () =
    FB.letter fb 0x61 ~x:30 ~y:30 >>=fun()-> FB.redraw fb

  let draw_letters fb () =
    FB.letters fb ~x:50 ~y: 50 "hello" >>= fun() -> FB.redraw fb
  let rec finish fb () =
    FB.recv_event fb >>= function
            | Window_close -> Lwt.return_unit
            | _ -> Lwt_unix.sleep 0.001 >>= finish fb
end

let main =
  let module A =  Framebuffer.Make(Framebuffer_tsdl) in
  A.init() >>= fun backend ->
  let module FB : Framebuffer.S = (val (backend)) in

  let module T = Make(FB) in
  FB.window ~width:100 ~height:100 >>= fun fb ->
  Lwt.async (T.pp_events fb);
  T.rainbow fb () >>=
  T.draw_pixels fb >>=
  T.draw_a_letter fb >>=
  T.draw_letters fb >>= fun () ->
  T.finish fb ()

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter () ;
  Logs.(set_level @@ Some Debug);
  Lwt_main.run main
