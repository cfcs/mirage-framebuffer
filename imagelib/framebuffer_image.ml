open Image

module Make(FB : Framebuffer.S) =
struct
  let draw_image (fb:FB.t) {Image.width;height;pixels;_ } : unit =
    let r, g, b =
      let normalize = function
        | Image.Pixmap.Pix8 pixmap -> fun x y ->
          Char.chr (Bigarray.Array2.get pixmap x y)
        | Image.Pixmap.Pix16 pixmap -> fun x y ->
          Char.chr @@ (Bigarray.Array2.get pixmap x y) lsr 8
      in
      begin match pixels with
        | (RGB  (r,g,b) | RGBA (r,g,b,_)) -> (* TODO alpha not handled *)
          normalize r , normalize g, normalize b
        | (Grey white | GreyA (white,_)) -> (* TODO alpha *)
          normalize white, normalize white, normalize white
      end
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let color = FB.compile_rgb ~r:(r x y) ~g:(g x y) ~b:(b x y) fb in
        let x = x * 3 in
        let y = y * 3 in
        for x' = 0 to 3 do
          for y' = 0 to 3 do
            FB.pixel fb ~x:(x+x') ~y:(y+y') (color : FB.color)
          done
        done
      done
    done
end
