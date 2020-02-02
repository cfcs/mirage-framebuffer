open Gg
open Vg
open Vgr.Private
(*open Vgr.Private.Data*)

module Vgr_mirage_framebuffer(FB:Framebuffer.S) = struct

  let image i = Vgr.Private.I.of_data i

  let fb_color ~window (c:v4) =
    let r, g, b =
      let srgb = Color.to_srgb c in
      char_of_int @@ int_of_float (255. *. Color.r srgb),
      char_of_int @@ int_of_float (255. *. Color.g srgb),
      char_of_int @@ int_of_float (255. *. Color.b srgb)
    in
    FB.compile_rgb ~r ~g ~b window

  module Path = struct
    module IntMap = Map.Make(struct
        type t = int
        let compare : int -> int -> int = compare
      end)
    type t = {
      intmap: int IntMap.t;
      lines: (int*int*int) list;
    }
    let empty = { intmap = IntMap.empty; lines = []; }
    let pp ppf t =
      Fmt.pf ppf "intmap (empty when healthy):@. @[<v>%a@]@.lines: @[<v>%a@]"
        Fmt.(seq ~sep:(unit"@,")(pair ~sep:(unit" -> ")int int))
        (IntMap.to_seq t.intmap)
        Fmt.(list ~sep:(unit "@,") @@ (fun ppf -> fun (a,b,c) ->
          Fmt.pf ppf "%d,%d,%d" a b c)) t.lines
    let add_point ~x ~y t =
      match IntMap.find_opt y t.intmap with
      | None ->
        let intmap = IntMap.add y x t.intmap in
        {t with intmap}
      (*| Some oldest when oldest = x -> t (* ignore duplicates *)*)
      | Some oldest ->
        let first = min oldest x in
        let last = max oldest x in
        let lines = (y,first,last)::t.lines in
        let intmap = IntMap.remove y t.intmap in
        {lines; intmap}
    let add_line cur p2 t fb_t =
      Logs.warn (fun m -> m "add_line sub:%a p2:%a" Gg.V2.pp cur Gg.V2.pp p2);
      let i = int_of_float and t' = ref t in
      FB.line_bresenham ~cb:(fun _fb ~x ~y _color ->
          t':= add_point ~x ~y !t' ;
        ) fb_t
        ~x:(i @@ P2.x cur) ~y:(i @@ P2.y cur)
        ~x_end:(i @@ P2.x p2) ~y_end:(i @@ P2.y p2) (FB.compile_rgb fb_t);
      (* Adding extra points is kind of a hack here, to close lines
         formed by polygon "peaks" where a single pixel sticks out.
         It results in errors on the final `Close, but I had a hard
         time coming up with an efficient way to fix this, so moving
         on to more pressing matters now. TODO. *)
      t' := add_point ~x:(i @@ P2.x cur) ~y:(i @@ P2.y cur) !t' ;
      t' := add_point ~x:(i @@ P2.x p2) ~y:(i @@ P2.y p2) !t' ;
      !t'

  end

  type render_state = {
    const: FB.color ;
    path: Path.t;
    sub: P2.t; (* last sub addr, potentially this should be a stack*)
    cur: P2.t; (*current cursor/pointer*)
  }
  let empty_render_state ~window =
    { const = FB.compile_rgb ~r:'\xff' ~g:'\xff' ~b:'\000' window ;
      path = Path.empty;
      sub = P2.o;
      cur = P2.o;
    }

  let quadratic_bezier_step t p0 p1 p2 =
    let powt = Stdlib.Float.pow (1.-.t) 2. in
    let tsquare = t *. t  in
    let txx = (1.-.t) *. 2. *. t in
    let open Gg.P2 in
    let x = powt *. x p0 +.
            txx *. x p1 +.
            tsquare *. x p2 in
    let y = powt *. y p0 +.
            txx *. y p1 +.
            tsquare *. y p2 in
    x, y

  let cubic_bezier_step t p0 p1 p2 p3 =
    let open Gg.P2 in
    let powt = (Stdlib.Float.pow (1.-.t) 3.0) in
    let pow2 = (Stdlib.Float.pow (1.-.t) 2.) in
    let x = powt *. x p0 +.
            pow2 *. 3. *. t *. x p1 +.
            (1.-.t) *. 3. *. t *. t *. x p2 +.
            t *. t *. t *. x p3 in
    let y = powt *. y p0 +.
            pow2 *. 3. *. t *. y p1 +.
            (1.-.t) *. 3. *. t *. t *. y p2 +.
            t *. t *. t *. y p3 in
    x, y

  let plotEllipseRect x0 y0 x1 y1 =
    (* listing 6, Bresenham.pdf *)
    let a : int = abs (x1-0) and b = abs (y1-y0) in
    let b1 = ref @@ b land 1 in
    let a'f = float a and b'f = float b in
    let dx : float = 4. *. (1.0 -. a'f) *. b'f *.b'f in
    let dy : float = 4. *. (float (!b1 + 1)) *. a'f *.a'f in (*err increment*)
    let err : float ref = ref (dx +. dy +. (float @@ !b1 * a * a)) in
    let e2 = ref 0.0 and x0 = ref x0 and x1 = ref x1
    and y0 = ref y0 and y1 = ref y1 and a = ref a in
    if x0 > x1 then begin (* if called with swapped points*)
      x0 := !x1 ;
      x1 := !x1 + !a ;
    end ;
    if y0 > y1 then y0 := !y1 ; (* exchange them*)
    y0 := !y0 + (b+1)/2 ;
    y1 := !y0 - !b1 ;
    a := 8 * !a * !a ;
    b1 := 8*b*b ;

    let p = ref Path.empty in
    p := Path.add_point ~x:!x1 ~y:!y0 !p;
    p := Path.add_point ~x:!x0 ~y:!y0 !p;
    p := Path.add_point ~x:!x0 ~y:!y1 !p;
    p := Path.add_point ~x:!x1 ~y:!y1 !p;
    e2 := 2. *. !err ;
    if !e2 <= dy then begin
      incr y0 ;
      decr y1 ;
      err := !err +. dy ; (* TODO from here on*)
    end ;
    ()

  let plotRotatedEllipseRect x0 y0 x1 y1 zd =
    (* freely after program listing 13, Bresenham.pdf *)
    let xd : int = x1 - x0 and yd = y1 - y0 in
    let w : float = float @@ xd * yd in
    if zd = 0 then
      plotEllipseRect x0 y0 x1 y1
    else
      let w:float = if w <> 0.0 then (w-.float zd)/.(w+.w)
        else w in (*squared weight*)
      assert (w <= 1.0 && w >= 0.0) ; (* liit angle *)
      let xd = int_of_float @@ floor (float xd *. w +. 0.5)
      and yd = int_of_float @@ floor (float yd *. w +. 0.5) in
      (* ^ snap xe,ye to int *)
      let plotQuadRationalBezierSeg _ _ _ _ _ _ _ =() in
      plotQuadRationalBezierSeg x0 (y0+yd) x0 y0 (x0+xd) y0 (1.0-.w);
      plotQuadRationalBezierSeg x0 (y0+yd) x0 y1 (x1-xd) y1 w ;
      plotQuadRationalBezierSeg x1 (y1-yd) x1 y1 (x1-xd) y1 (1.0-.w);
      plotQuadRationalBezierSeg x1 (y1-yd) x1 y0 (x0+xd) y0 w ;
      ()

  let plotRotatedEllipse x y a b angle =
    (* Listing 13: Bresenham.pdf: plot ellipse rotated by angle (radian) *)
    let xd = float (a * a) and yd = float (b*b) in
    let s = sin angle in
    let zd = (xd -. yd) *. s in (* ellipse rotation *)

    let xd = sqrt (xd -. zd *. s) in
    let yd = sqrt (yd +. zd *. s) in (* surrounding rectangle *)
    let a = xd +. 0.5 and b = yd +. 0.5 in
    let zd = (zd *. a *. b) /. (xd *. yd) in (* scale to integer*)
    let i = int_of_float in
    plotRotatedEllipseRect (i@@ x-.a) (i@@ y-.b) (i@@ y+.b) (i@@ 4. *. zd *. cos angle)

  let _ = plotRotatedEllipse (** TODO *)

  let _ = quadratic_bezier_step, cubic_bezier_step

  let pp_segment ppf = function
    | `Ccurve (p0, p1, p2) ->
      Fmt.pf ppf "Ccurve c: %a p1:%a p2:%a"
        Gg.V2.pp p0 Gg.V2.pp p1 Gg.V2.pp p2
    | `Close ->
      Fmt.pf ppf "Close, should draw until Sub"
    (*     [close p] is [p] with a straight line from [p]'s last point to
           [p]'s current subpath starting point, this ends the subpath.*)
    | `Earc (large, cw, angle, radii_size2, p2) ->
      Fmt.pf ppf "Earc large:%b clockwise:%b angle:%f radii:%fx%f pt:%a"
        large cw angle
        (Size2.w radii_size2) (Size2.h radii_size2) Gg.V2.pp p2
    | `Line p2 ->
      Fmt.pf ppf "Line %a" Gg.V2.pp p2
    | `Qcurve (_Gg_p2, _Gg_p2') -> Fmt.pf ppf "Qcurve"
    | `Sub p2 ->
      Fmt.pf ppf "Sub %a" Gg.V2.pp p2
  (* [sub pt p] is [p] with a new subpath starting at [pt]. If [p]'s last
     subpath had no segment it is automatically {!close}d. *)

  let r_cut window (rs:render_state) r (path:Data.path) =
    List.fold_right (fun seg (rs,_OK_TODO) -> match seg with
        | `Line p2 ->
          Logs.warn (fun m -> m "draw at %a" Gg.V2.pp p2);
          Logs.warn (fun m -> m "%f %f"
                        (V2.comp 0 p2)
                        (V2.comp 1 p2)
                    );
          let x = Gg.P2.x p2 |> int_of_float in
          let y = Gg.P2.y p2 |> int_of_float in
          (* well here we should rasterize the box instead *)
          (*FB.rect window ~x ~y ~x_end ~y_end rs.const ;*)
          let _ = window,x,y in
          let rs = {rs with
                    cur = p2 ;
                    path = Path.add_line rs.cur p2 rs.path window} in
          Logs.debug (fun m -> m "%a" Path.pp rs.path);
          rs, `Ok
        | `Sub sub ->
          Logs.warn (fun m -> m "GOTO sub %a" Gg.V2.pp sub);
          {rs with sub; cur = sub}, `Ok
        | `Close ->
          Logs.warn (fun m -> m "CLOSING");
          (* drawing a line from cur to sub: *)
          let rs = {rs with cur = rs.sub;
                            path = Path.add_line rs.cur rs.sub rs.path window} in
          Logs.debug (fun m -> m "%a" Path.pp rs.path);
          rs, `Ok
        | `Earc (_large, _clockwise, _angle, _radii_size2, p2) ->
          (* see docstring for `val earc` in vg.mli *)
          Logs.warn (fun m -> m"EARC GOT: %a" pp_segment seg);
          let rs = {rs with cur = p2} in
          rs, `Ok
        | _ ->
          Logs.err (fun m -> m "r_cut:unmatched: %a" pp_segment seg);
          warn r (`Other "r_cut:unmatched, not drawing");
          rs, `Partial) path (rs, `Ok)

  let r_cut_glyphs s a _run = function _ as i ->
    ignore @@ failwith "cut glyphs shit";
    warn s (`Unsupported_glyph_cut (a, image i)) ;
    `Partial

  let pp_primitive ppf : Data.primitive -> unit =
    let open Fmt in function
      | Const gg_color ->
        (parens @@ vbox @@ prefix (unit "Color") (Gg.V4.pp)) ppf gg_color
      | Axial (_stops, v2, _v2') ->
        Fmt.prefix (Fmt.unit "Axial") (Fmt.append Gg.V2.pp Gg.V2.pp) ppf v2
      | Radial _ -> Fmt.prefix (Fmt.unit "Radial") Fmt.string ppf ""
      | Raster _ -> Fmt.prefix (Fmt.unit "Raster") Fmt.string ppf ""

  let pp_path = let open Fmt in
    parens @@ prefix (unit "Path ")
      (vbox @@ list ~sep:cut
         (parens pp_segment))

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
    | Blend (blender, size1,_img1, _img2)  ->
      let blend = match blender with
        | `Atop -> "Atop" | `Copy -> "Copy" | `In -> "In"
        | `Out -> "Out" | `Over -> "Over" | `Plus -> "Plus" | `Xor -> "Xor" in
      Fmt.pf ppf "Blend (%s, %a, img1, img2)TODO" blend Fmt.(option float) size1
    | Tr _ -> Fmt.pf ppf "Tr-TODO"

  let rec render_image ~window rs r (img:Data.image) =
    Logs.warn (fun m -> m "img: %a" pp_img img) ;
    match img with
    | Primitive Const v4 ->
      Logs.warn (fun m -> m "Primitive Const Color %a"
                    Gg.V4.pp
                    @@ Gg.Color.to_srgb v4);
      let color = fb_color ~window v4 in
      { rs with const = color ; } ,
      `Ok (* set draw color *)
    | Primitive _ ->
      Logs.warn (fun m -> m "Primitive ??");
      rs, `Partial
    | Cut (`Anz,p,i) ->
      Logs.warn (fun m -> m "Cut ANZ");
      let rs, _ = render_image ~window rs r i in
      let rs, status = r_cut window rs r p in
      List.iter (fun (y,x0,x1) ->
          Logs.warn (fun m -> m "drawing %d,%d -> %d,%d" x0 y x1 y);
          FB.rect window ~x:x0 ~y ~x_end:x1 ~y_end:y rs.const
        ) rs.path.lines ;
      let rs = {rs with path = Path.empty} in
      rs, status
    | Cut (_, _, i) ->
      Logs.warn (fun m -> m "Cut ??");
      render_image ~window rs r i
    | Cut_glyphs (a, gr, i)  ->
      Logs.warn (fun m -> m "Cut_glyphs ??");
      rs, r_cut_glyphs r a gr i
    | Blend (`Over, size, img1, img2) ->
      (* img2 is painted on top of ("Over") img1,
         seems to be the default for {!I.blend} *)
      Logs.warn (fun m -> m "Blend (Over, size? %a)"
                    Fmt.(option float) size);
      let rs, _todo = render_image ~window rs r img1 in
      render_image ~window rs r img2
    | Blend _ ->
      Logs.warn (fun m -> m "Blend (??, size, img1, img2)");
      rs, `Partial
    | Tr _ ->
      Logs.warn (fun m -> m "TR ??");
      rs, `Partial

  let render_fun r ~window : Vg.Vgr.Private.render_fun =
    let _TODO = r_cut_glyphs in
    function
    | `End -> fun k -> k
    | `Image (size, view, image) ->
      let _ = (* resize to dimensions: *)
        let width = Size2.w size /. Box2.w view
                    |> int_of_float in
        let height = Size2.h size /. Box2.h view
                     |> int_of_float in
        Lwt.async (fun () ->
            FB.resize ~width ~height window);
        let rs = empty_render_state ~window in
        render_image ~window rs r image
      in
      (fun k -> k)

  let render_target ~window renderer _dst =
    false, render_fun renderer ~window

  let target ~(window:FB.t) ()
    : Vg.Vgr.dst Vg.Vgr.target =
    let target = render_target ~window in
    Vgr.Private.create_target target
end
