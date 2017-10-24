open Lwt.Infix

module Make(FB:Framebuffer__S.Framebuffer_S) = struct
  let draw_a_pixel fb =
    let red = FB.compile_rgb ~r:'\xff' fb in
    FB.pixel fb ~x:10 ~y:10 red >>=fun()-> FB.redraw fb

 let draw_a_letter fb () =
    FB.letter fb 0x61 ~x:30 ~y:30 >>=fun()-> FB.redraw fb

 let draw_letters fb () =
    FB.letters fb ~x:50 ~y: 50 "hello" >>= fun() -> FB.redraw fb
  let sleep () =
    Lwt_unix.sleep 10.0
end

let () =
  let module FB : Framebuffer__S.Framebuffer_S with type init_handle = unit
                = Framebuffer.Make(Framebuffer_tsdl) in
  let module T = Make(FB) in
  Lwt_main.run (
  FB.init ~width:100 ~height:100 () >>= fun fb ->
  T.draw_a_pixel fb >>=
  T.draw_a_letter fb >>=
  T.draw_letters fb >>= T.sleep)
