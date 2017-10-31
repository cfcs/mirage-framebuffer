open Lwt.Infix

module Make(FB:Framebuffer.S) = struct
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
  let sleep () =
    Lwt_unix.sleep 10.0
end

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter () ;
  Logs.(set_level @@ Some Debug);
  let module A =  Framebuffer.Make(Framebuffer_tsdl) in
  let b = A.init() in
  let module FB : Framebuffer.S = (val (b)) in

  let module T = Make(FB) in
  Lwt_main.run (
  FB.window ~width:100 ~height:100 >>= fun fb ->
  T.rainbow fb () >>=
  T.draw_pixels fb >>=
  T.draw_a_letter fb >>=
  T.draw_letters fb >>= T.sleep)
