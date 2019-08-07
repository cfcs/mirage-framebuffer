open Gg
open Vg
open Vgr.Private
(*open Vgr.Private.Data*)

module Vgr_mirage_framebuffer(FB:Framebuffer.S) = struct

  let image i = Vgr.Private.I.of_data i

  let fb_color ~window (c:v4) =
    let r, g, b =
      let srgb = Color.to_srgb c in
      char_of_int @@ int_of_float (Color.r srgb),
      char_of_int @@ int_of_float (Color.g srgb),
      char_of_int @@ int_of_float (Color.b srgb)
    in
    FB.compile_rgb ~r ~g ~b window

  let r_cut_glyphs s a _run = function _ as i ->
    ignore @@ failwith "cut glyphs shit";
    warn s (`Unsupported_glyph_cut (a, image i))


  let render_image (img:Data.image) =
    let pp_primitive ppf : Data.primitive -> unit =
      let open Fmt in function
      | Const gg_color ->
        (parens @@ vbox @@ prefix (unit "Color") (Gg.V4.pp)) ppf gg_color
      | Axial (_stops, v2, _v2') ->
        Fmt.prefix (Fmt.unit "Axial") (Fmt.append Gg.V2.pp Gg.V2.pp) ppf v2
      | Radial _ -> Fmt.prefix (Fmt.unit "Radial") Fmt.string ppf ""
      | Raster _ -> Fmt.prefix (Fmt.unit "Raster") Fmt.string ppf ""
    in
    let pp_segment ppf = function
      | `Ccurve (_Gg_p2, _Gg_p2', _Gg_p2'') -> Fmt.pf ppf "Ccurve"
      | `Close -> Fmt.pf ppf "Close"
      (*     [close p] is [p] with a straight line from [p]'s last point to
             [p]'s current subpath starting point, this ends the subpath.*)
      | `Earc (b1, b2, f, size2, p2) ->
        Fmt.pf ppf "Earc %b %b %f %fx%f %a" b1 b2 f
          (Size2.w size2) (Size2.h size2) Gg.V2.pp p2
      | `Line p2 -> Fmt.pf ppf "Line %a" Gg.V2.pp p2
      | `Qcurve (_Gg_p2, _Gg_p2') -> Fmt.pf ppf "Qcurve"
      | `Sub p2 -> Fmt.pf ppf "Sub %a" Gg.V2.pp p2
      (* [sub pt p] is [p] with a new subpath starting at [pt]. If [p]'s last
         subpath had no segment it is automatically {!close}d. *)
    in
    let pp_path = let open Fmt in
      parens @@ prefix (unit "Path ")
        (vbox @@ list ~sep:cut
           (parens pp_segment)) in
    let rec pp_img ppf (img:Data.image) =
      let open Fmt in
      match img with
      | Primitive prim ->
        (parens (prefix (unit "Primitive ") (vbox pp_primitive))) ppf prim
      | Cut (area,path,nested_img) ->
        (parens
           (prefix (unit "Cut ")
              @@ vbox ~indent:2 (fun ppf () -> pf ppf "%a %a@ %a"
                  Vg.P.pp_area area
                  pp_path path
                  (vbox @@ brackets pp_img) nested_img
              )))
          ppf ()
      | Cut_glyphs _ -> Fmt.pf ppf "Cut_glyphs-TODO"
      | Blend _ -> Fmt.pf ppf "Blend-TODO"
      | Tr _ -> Fmt.pf ppf "Tr-TODO"
    in
    Logs.warn (fun m -> m "img: %a" pp_img img) ;
    ()

  let render_fun ~window : Vg.Vgr.Private.render_fun =
    let _TODO = r_cut_glyphs in
    function
    | `End -> fun k -> k
    | `Image (size, view, image) ->
      let _ = (* resize to dimensions: *)
        let width = Size2.w size /. Box2.w view
                    |> int_of_float in
        let height = Size2.h size /. Box2.h view
                     |> int_of_float in
        Lwt.async (fun () -> FB.resize ~width ~height window) ;
        render_image image
      in
      fun k -> k

  let render_target ~window _renderer _dst =
    false, render_fun ~window

  let target ~(window:FB.t) ()
    : Vg.Vgr.dst Vg.Vgr.target =
    let target = render_target ~window in
    Vgr.Private.create_target target
end
