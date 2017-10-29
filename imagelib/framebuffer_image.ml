open! Image

module Make(FB : Framebuffer.S) =
struct
  let draw_image (fb:FB.t) {Image.width;height;pixels;_ } : unit Lwt.t =
  let r, g, b =
    let normalize = function
    | Image.Pixmap.Pix8 pixmap -> fun x y ->
        Char.chr (Bigarray.Array2.get pixmap x y)
    | Image.Pixmap.Pix16 pixmap -> fun x y ->
        Char.chr @@ (Bigarray.Array2.get pixmap x y)/256
    in
    begin match pixels with
    | (RGB  (r,g,b) | RGBA (r,g,b,_)) -> (* TODO alpha not handled *)
        normalize r , normalize g, normalize b
    | (Grey white | GreyA (white,_)) -> (* TODO alpha *)
        normalize white, normalize white, normalize white
    end
  in
  let open Framebuffer.Utils in
  lwt_for height
  (fun (y:int) ->
    lwt_for width
      (fun (x:int) ->
         let color = FB.compile_rgb ~r:(r x y) ~g:(g x y) ~b:(b x y) fb in
         FB.pixel fb ~x ~y (color : FB.color)))
end
