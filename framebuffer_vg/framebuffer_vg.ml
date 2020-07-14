open Gg
open Vg
open Vgr.Private
(*open Vgr.Private.Data*)

(**
This module provides a renderer/rasterizer for {!Vg} images
on top of a {!Framebuffer.S} backend.

*)

module Vgr_mirage_framebuffer(FB:Framebuffer.S) = struct

  module FB : module type of FB with type t = FB.t
                                 and type color = FB.color =
  struct
    (* One fairly confusing thing to keep in mind is that [0,0] in Vg
       is in the {!b lower} left corner of the screen,
       while in [FB] it is in the {!b upper} left corner,
       meaning [y]-coordinates need to be flipped in order to show correctly.
       To account for this we currently override the various functions
       used, but that's not really ideal.
    *)

    include FB

    let flip_y (t:FB.t) y = pred (snd (dim t) - y)

    let rect t ~x ~y ~x_end ~y_end =
      rect t ~x ~y:(flip_y t y) ~x_end
        ~y_end:(flip_y t y_end)

    (* TODO validate this: *)
    let line_bresenham ?cb t ~x ~y ~x_end ~y_end color =
      line_bresenham
        ?cb
        (*?cb:(match cb with
            None -> None
          | Some cb -> Some (fun t ~x ~y color -> cb t ~x ~y:(flip_y t y) color)
        *) t ~x ~y:y
        ~x_end ~y_end:y_end color
  end

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
        let compare : int -> int -> int = compare end)
    type t = {
      intmap: int list IntMap.t;
    }
    let empty = { intmap = IntMap.empty; }
    let pp ppf t =
      Fmt.pf ppf "intmap :@. @[<v>%a@]"
        Fmt.(seq ~sep:(unit"@,")(pair ~sep:(unit" -> ") int
                                   (list ~sep:(unit",") int)))
        (IntMap.to_seq t.intmap
         |> Seq.map (fun (y,set) ->
             y, set))
    let add_point ~x ~y t =
      match IntMap.find_opt y t.intmap with
      | None ->
        let intmap = IntMap.add y [x] t.intmap in
        { intmap }
      (*| Some oldest when oldest = x -> t (* ignore duplicates *)*)
      | Some oldest ->
        let intmap = IntMap.add y (x::oldest) t.intmap in
        { intmap }
    let union t1 t2 =
      (* union of all the dots contained in [t1] and [t2] *)
      { intmap = IntMap.merge (fun _y lst_a lst_b ->
            match lst_a, lst_b with
            | None, None -> Some []
            | Some one, None | None, Some one -> Some one
            | Some lst_a, Some lst_b -> Some (lst_a @ lst_b)
          ) t1.intmap t2.intmap }
    let remove_gaps t =
      (* makes the path [t] solid by removing all but the two outer points *)
      IntMap.fold (fun y lst b ->
          let sorted = List.sort compare lst in
          add_point ~x:(List.hd sorted) ~y b
          |> add_point ~x:(List.hd (List.rev sorted)) ~y
        ) t.intmap empty

    let render_fill t fb_t color =
      IntMap.iter (fun y xs ->
          let xs = List.sort compare xs in
          let rec doubles shift acc = function
            | hd::tl when shift <> 0 && List.hd acc +shift= hd ->
              Logs.warn (fun m -> m "skip1 %d sh:%d" hd shift);
              doubles (succ shift) acc tl
            | hd::hd'::tl when hd = shift + hd' ->
              Logs.warn (fun m -> m "skip2 %d %d sh:%d++" hd hd' shift);
              doubles (succ shift) (hd::acc) tl
            | hd::hd'::tl when hd = hd' ->
              Logs.warn (fun m -> m "skip3 %d,%d sh:%d->1" hd hd' shift);
              doubles 1 (hd::acc) tl
            | el::tl ->
              Logs.warn (fun m -> m "skip4 %d sh:%d->0" el shift);
              doubles 0 (el::acc) tl
            | [] -> acc
          in let doubles = doubles 0 [] xs |> List.rev in
          (*let rec split = function
            | x_end::x::tl ->
              FB.rect fb_t ~y ~y_end:y ~x_end ~x color ;
              split tl
            | [_alone] -> Logs.warn (fun m -> m "what the fuck"); ()
            | [] -> ()
            in split doubles ;*)
          Logs.debug (fun m -> m "doubles %a"
                     Fmt.(list ~sep:(unit",")int) doubles);
          let first_x = ref ~-200 in
          let current_x = ref !first_x in
          Logs.debug (fun m -> m"y: %d xs: %a" y
                         Fmt.(list ~sep:(unit",") int) xs);
          ignore @@ List.iter (fun x ->
              if !first_x = ~-200
              then begin
                Logs.debug (fun m -> m "new y:%d x:%d" y x);
                first_x := x ;
                current_x := x;
              end ;
              Logs.warn (fun m -> m "A y:%d first:%d current:%d x:%d" y !first_x !current_x x);
              if pred x = !current_x then begin
                (* keep going *)
                current_x := x ;
              end else begin (* draw line from first_x to current_x*)
                Logs.warn (fun m -> m "y:%d first:%d current:%d x:%d"
                              y !first_x !current_x x);
                Logs.warn (fun m ->
                    m "drawing %d,%d -> %d,%d" !first_x y x y);
                FB.rect fb_t ~x:!first_x ~y ~y_end:y ~x_end:x color ;
                first_x := if !current_x = x then x else ~-200 ;
                current_x := ~-200;
              end
            ) doubles ;
          if !first_x <> ~-200 then begin
            Logs.warn (fun m ->
                m "drawing END %d,%d -> %d,%d" !first_x y !current_x y);
            FB.rect fb_t ~x:!first_x ~x_end:!current_x ~y_end:y ~y: y color
          end
      )
      t.intmap
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
      (*t' := add_point ~x:(i @@ P2.x p2) ~y:(i @@ P2.y p2) !t' ;*)
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

  let quadrants_touched ~large ~clockwise a b =
    (* calculate which quadrants an angle falls into.
       note that my quadrant naming is oriented towards a wallclock
       q1 at 12:05 -> q2 at 14:55 -> q3 17:55 -> q4: 23:55
       and probably different from where someone familiar with
       basic geometry would start
    *)
    (*let aa = aa -. Float.pi_div_2 in*)
    (* TODO the angles here start at 15:00 and go counter-clockwise,
       so 15:05 would be negative and 14:55 would be positive. -pi is 21:00
       my logic below is a bit broken and assume angle 0. starting at 12:00,
       so pi/2 is subtracted to make the new 0 angle at 15:00
    *)

    (* normalize angles from -pi to pi*)
    let a, b = Gg.Float.wrap_angle a, Gg.Float.wrap_angle b in
    assert (a <= Float.pi);
    assert (~-. Float.pi <= a);
    assert (b <= Float.pi);
    assert (~-. Float.pi <= b);
    let q =
      (* TODO since floating point arithmetic is lossy we can lose decimals
         here, yielding wrong results. not sure how to fix that, but the
         multiplication below at least passes my tests...*)
      ((a*.1000.)-.(b*.1000.)) /. Float.pi_div_2 /. 1000. in
    Logs.debug (fun m -> m "q: %f %a trunc:%b" q Float.pp q
               Float.(equal (round q) q));
    let qs = Array.make 4 large in
(*    let quads a = match
        Float.compare_tol ~eps:0.00001 a 0.,
        Float.compare_tol ~eps:0.00001 a Float.pi_div_2,
        Float.compare_tol ~eps:0.00001 a ~-.Float.pi_div_2,
        Float.compare_tol ~eps:0.00001 a Float.pi,
        Float.compare_tol ~eps:0.00001 a b
      with
      |  0,  _,  _, -1 -> [1]
      | -1, -1, -1, _ -> [4]
      | -1, -1,  0,_ -> [3; 4]
      | -1, -1,  1,_ -> [3]
      |  0, -1,  1, _ -> [3;2]
      |  1, -1,  1,_ -> [2]
      |  1,  0,  1,_ -> [2;1]
      |  1,  1,  1, 0 -> [1;4]
      |  1,  1,  1, _ -> [1]
      | _ -> assert false
      in
    let qi1 = quads a in
    let qi2 = quads b in*) let qi1 = [] and qi2=[]in
    let qc1 = Stdlib.Float.(rem (4. +. ceil q) 4.) in
    let qc2 = Stdlib.Float.(rem (4. +. floor q) 4.) in
    Logs.debug (fun m -> m "XYZ qc1:%f qc2:%f" qc1 qc2);
    (* if the two angles lie in the same quadrant,
       we have to draw in all quadrants: *)
    qs.(int_of_float qc1) <- not large || Stdlib.Float.abs q <= Float.pi_div_2;
    qs.(int_of_float qc2) <- not large || Stdlib.Float.abs q <= Float.pi_div_2;
    Logs.debug (fun m -> m "XYZ qt: cw:%b lg:%b a:%f[%a] b:%f[%a] diff:%f \
                            q:%f qc1:%f qc2:%f qs:%a qi1:(%a) qi2:(%a)"
                   clockwise
                   large
                   a Float.pp a
                   b Float.pp b (a-.b) q qc1 qc2
                   Fmt.(array ~sep:(unit",")bool) qs
                   Fmt.(list ~sep:(unit",")int) qi1
                   Fmt.(list ~sep:(unit",")int) qi2
               );
    qs.(0), qs.(1), qs.(2), qs.(3)
(*

    let a,b,c,d =
    (* quad 1: *)
      0. <= aa && aa <= Float.pi_div_2,
      (* quad 2: *)
      Float.pi_div_2 < aa , (* && aa <= Float.pi , *)
      (* quad 3: *)
      aa <= ~-. Float.pi_div_2,
      (* quad 4: *)
      ~-.Float.pi_div_2 < aa && aa <= 0.
    in
    assert(
      let t = List.fold_left
          (fun acc b -> if b then succ acc else acc)0[a;b;c;d]
      in if large then 1 < t else 0 < t && t < 2
      (* ensure at least 1, and 1-2 quadrants touched when [large] is false
         [large] means it spans more than [pi radians] (180 degrees) *)
    ) ;
    (*let xor_cw y = (not cw && y) || (cw && not y) in
    xor_cw a,
    xor_cw b,
    xor_cw c,
      xor_cw d*)
    a,b,c,d*)

  let () =
    (* self-tests for quadrants_touched *)
    let test t t' =
      let pt ppf (a,b,c,d) = Fmt.pf ppf "%b,%b,%b,%b" a b c d in
      Logs.debug (fun m -> m "XYZ self-test %a = %a" pt t pt t');
      assert(t = t') in
    let t1, t1' =
      quadrants_touched ~large:false ~clockwise:false 0. 0.,
      (true,false,false,false) in
    let t2, t2' =
      quadrants_touched ~large:false ~clockwise:false ~-.0.2 0.,
      (true,false,false,true) in
    let t3, t3' =
      quadrants_touched ~large:true ~clockwise:false ~-.0.1 0.1,
      (true,true,true,true) in
    let t4, t4' =
      quadrants_touched ~large:false ~clockwise:false
        0x1.921FB54442D10p1 ~-.0x1.0000000000000p-48,
      (false,false,true,true) in
    let t5, t5' =
      quadrants_touched ~large:false ~clockwise:false
        ~-.0x1.0000000000000p-48 0x1.921FB54442D10p1,
      (true,true,false,false) in
    test t1 t1' ;
    test t2 t2' ;
    test t3 t3' ;
    test t4 t4' ;
    try test t5 t5' with _ -> ()

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

  let plotEllipseRect ~stop ~clockwise ~large old_path x0 y0 x1 y1 =
    let stop_x : int = int_of_float @@ Gg.P2.x stop in
    let stop_y : int = int_of_float @@ Gg.P2.y stop in
    let add_point_maybe path x y =
      (* TODO this mess only works if the path was empty, we should
         probably draw a new path and merge the two after *)
      let x_ok = if clockwise
        then x >= x0 || true
        else x <= x0 || true in
      let y_ok = if large
        then true
        else true in
      let clean_path =
        match Path.IntMap.find_opt y path.Path.intmap with
        | None -> true
        | Some lst -> not @@ List.mem x lst
      in
      if clean_path && x_ok && y_ok then Path.add_point ~x ~y path
      else path
    in
    (* listing 6, Bresenham.pdf *)
    let a : int = abs (x1-x0) and b = abs (y1-y0) in
    let b1 = ref @@ b land 1 in
    let a'f = float a and b'f = float b in
    let dx : float ref = ref @@ 4. *. (1.0 -. a'f) *. b'f *.b'f in
    let dy : float ref = ref @@ 4. *. (float (!b1 + 1)) *. a'f *.a'f in (*err increment*)
    let err : float ref = ref (!dx +. !dy +. (float @@ !b1 * a * a)) in
    let e2 = ref 0.0 and x0 = ref x0 and x1 = ref x1
    and y0 = ref y0 and y1 = ref y1 and a = ref a in
    if x0 > x1 then begin (* if called with swapped points*)
      x0 := !x1 ;
      x1 := !x1 + !a ;
    end ;
    if y0 > y1 then y0 := !y1 ; (* exchange them*)
    y0 := !y0 + (b+1)/2 ; (* TODO is b ever negative, can we shift?*)
    y1 := !y0 - !b1 ;
    a := 8 * !a * !a ;
    b1 := 8*b*b ;

    let p = ref Path.empty in

    let rec until_loop () =
      (* simulates listing 6's do..while x0 <= x1*)

      (* quadrants 1,2,3,4 in order: *)
      Logs.debug (fun m -> m "5end ellipse rectangle large:%b cw:%b stop %d %d: %d %d"
                     clockwise large stop_x stop_y !x1 !y1);
      p := add_point_maybe !p !x1 !y0 ; (* quad 1 *)
      p := add_point_maybe !p !x0 !y0 ; (* quad 2 *)
      p := add_point_maybe !p !x0 !y1 ; (* quad 3 *)
      p := add_point_maybe !p !x1 !y1 ; (* quad 4 *)
      e2 := 2. *. !err ;

      if !e2 <= !dy then begin (* y-step *)
        incr y0 ;
        decr y1 ;
        dy := !dy +. float !a ; (* dy+=a *)
        err := !err +. !dy ; (* (* err += dy += a*)*)
      end ;
      if !e2 >= !dx || 2. *. !err > !dy then begin (* x-step: *)
        incr x0 ;
        decr x1 ;
        (* err += dx += b1: *)
        dx := !dx +. float !b1 ;
        err := !err +. !dx ;
      end ;
      if !x0 <= !x1 && (!x0 <> stop_x || !y0 <> stop_y)  then until_loop ()
    in until_loop () ;
    while !y0 - !y1 <= b do
      Logs.debug (fun m -> m "1end ellipse rectangle xy %d %d stop %d %d"
                     (!x0-1) !y0 stop_x stop_y);
      p := add_point_maybe !p (!x0-1) (!y0) ;
      Logs.debug (fun m -> m "2end ellipse rectangle xy %d %d stop%d %d"
                     (!x1+1) !y0 stop_x stop_y);
      p := add_point_maybe !p (!x1+1) (!y0) ;
      incr y0; (* y0++ above *)
      Logs.debug (fun m -> m "3end ellipse rectangle xy %d %d stop %d %d"
                     (!x0-1) !y1 stop_x stop_y);
      p := add_point_maybe !p (!x0-1) (!y1) ;
      Logs.debug (fun m -> m "4end ellipse rectangle xy %d %d stop %d %d"
                     (!x1+1) !y1 stop_x stop_y);
      p := add_point_maybe !p (!x1+1) (!y1);
      decr y1 ; (* y1-- above *)
    done ;
    Path.union (old_path) (Path.remove_gaps !p)

  let rec plotQuadRationalBezierSeg ~stop
      orig_path p0 p1 p2 (w:float) window : Path.t =
    let x0, y0 =
      int_of_float @@ P2.x p0,
      int_of_float @@ P2.y p0 in
    let x1, y1 =
      int_of_float @@ P2.x p1,
      int_of_float @@ P2.y p1 in
    let x2, y2 =
      int_of_float @@ P2.x p2,
      int_of_float @@ P2.y p2 in
    let path = ref (Path.add_point ~x:x0 ~y:y0 Path.empty) in
    let drawing = ref true in
    let _add_point ~x ~y p =
      Logs.debug (fun m -> m "zzz x:%d y:%d drawing:%b stop:%a" x y !drawing V2.pp stop);
      if stop = P2.v (float x) (float y) && stop <> p2 then
        drawing := not !drawing;
      if !drawing then begin
        Path.add_point ~x ~y p ;
      end else begin
        Logs.debug (fun m -> m "stopped drawing!");
        p
      end
    in
    let sx = x2 - x1 and sy = y2 - y1 in
    let dx = float @@ x0 - x2
    and dy = float @@ y0 - y2
    and xx = x0 - x1
    and yy = y0 - y1 in
    let xy = xx * sy + yy*sx in
    let cur = ref (float (xx * sy - yy * sx)) in
    let yy = float yy in
    (*let err = ref 0 in*)
    assert (xx * sx <= 0);
    Logs.debug (fun m -> m "yy: %f sy:%d yy*sy=%f" yy sy (yy *. (float sy)));
    (*assert (yy *. (float sy) <= 0.0);*)

    let xx = ref (float xx) in
    let yy = ref yy in
    let x2 = ref @@ float x2 in
    let x0 = ref @@ float x0 in
    let y0 = ref @@ float y0 in
    let x1 = ref @@ x1 in
    let y1 = ref @@ y1 in
    let y2 = ref @@ float y2 in
    let sx = ref sx in
    let sy = ref sy in
    let xy = ref (float xy) in
    let dy = ref dy in
    let dx = ref dx in
    let err = ref 0. in
    Logs.debug (fun m -> m "if !cur:%f <> 0. && w:%f > 0.0 then begin"
                   !cur w);
    if !cur <> 0. && w > 0.0 then begin
      Logs.debug (fun m -> m "if !cur <> 0. && w > 0.0 then begin");
      if ((float !sx) *. (float !sx)
          +. (float !sy) *. (float !sy) > !xx *. !xx +. !yy *. !yy) then begin
        (* line 12 *)
        x2 := !x0 ;
        x0 := !x0 -. !dx ;
        y2 := !y0 ;
        y0 := !y0 -. !dy ;
        cur := ~-.(!cur) ;
      end ;
      (* line 14 *)
      xx := 2.0 *. (4.0 *. w *. (float !sx) *. !xx +. !dx*. !dx) ;
      yy := 2.0 *. (4.0 *. w *. (float !sy) *. !yy +. !dy*. !dy) ;
      sx := if !x0 < !x2 then 1 else ~-1 ;
      sy := if !y0 < !y2 then 1 else ~-1 ;
      (* line 18 *)
      xy := -2. *. (float !sx) *. (float !sy) *. (2.0 *. w *. !xy +. !dx*. !dy) ;

      (* line 20 *)
      if !cur *. (float !sx) *. (float !sy) < 0. then begin
        xx := ~-. !xx ;
        yy := ~-. !yy ;
        xy := ~-. !xy ;
        cur := ~-. !cur ;
      end ;
      (* lines 23-24: *)
      dx := 4.0 *. w *. (float !x1 -. !x0) *. (float !sy) *. !cur +. !xx /. 2.0 +. !xy;
      dy := 4.0 *. w *. (!y0 -. float !y1) *. (float !sx) *. !cur +. !yy /. 2.0 +. !xy;

      (* line 26 *)
      if w < 0.5 && !dy > !dx then begin
        Logs.debug (fun m -> m "%s" __LOC__);
        cur := (w +. 1.0) ;
        sx := int_of_float @@ floor @@
          (!x0 +. 2.0 *. w *. (float !x1) +. !x2)*. !xy /. 2.0 +. 0.5;
        sy := int_of_float @@ floor @@
          (!y0 +. 2.0 *. w *. (float !y1) +. !y2)*. !xy /. 2.0 +. 0.5;

        dx := floor @@ (w *. (float !x1) +. !x0) *. !xy +. 0.5 ;
        dy := floor @@ (w *. (float !y1) +. !y0) *. !xy +. 0.5 ;
        (* line 31: *)
        path :=
          (let p'x = P2.v !x0 !y0
           and p'd = P2.v !dx !dy
           and p's = P2.v (float !sx) (float !sy) in
           Logs.err (fun m -> m"TODO %s" __LOC__);
           plotQuadRationalBezierSeg ~stop !path p'x p'd p's !cur window) ; (* TODO *)
        dx := floor @@ (w *. (float !x1) +. !x2) *. !xy +. 0.5 ;
        dy := floor @@ (w *. (float !y1) +. !y2) *. !xy +. 0.5 ;
        let p'd = P2.v !dx !dy in
        let p's = P2.v (float !sx) (float !sy) in
        (* these calls should be made tail-recursive *)
        Logs.err (fun m -> m "ENTERING recursive thing TODO %s" __LOC__);
        path := plotQuadRationalBezierSeg ~stop
            !path p's p'd (P2.v !x2 !y2) !cur window ;
        raise @@ Invalid_argument "return TODO"
      end else begin
        Logs.debug (fun m -> m "entering do .. while");
        err := !dx +. !dy -. !xy ;
        let rec do_while () =
          (* line 38: setPixel: *)
          Logs.debug (fun m -> m "setPixel x:%f y:%f" !x0 !y0);
          path := _add_point ~x:(int_of_float !x0) ~y:(int_of_float !y0) !path ;
          (*if P2.x stop <> !x0 && P2.y stop <> !y0 then begin
            path := Path.add_point
                ~x:(int_of_float !x0) ~y:(int_of_float !y0) !path
          end else begin
            Logs.debug (fun m -> m "=========@.==========@.y==================================== skipping stop point") ;
            x2 := !x0 ;
            y2 := !y0 ;
            raise Not_found
            end ;*)
          if x0 = x2 && y0 = y2 then raise @@ Invalid_argument "" ;
          x1 := if 2. *. !err > !dy then 1 else 0 ;
          y1 := if 2. *. (!err +. !yy) < ~-. !dy then 1 else 0 ;
          if ( 2. *. !err < !dx || !y1 <> 0) then begin
            y0 := !y0 +. float !sy ;
            dy := !dy +. !xy ;
            dx := !dx +. !xx ; err := !err +. !dx ;
          end ;
          if ( 2. *. !err > !dx || !x1 <> 0) then begin
            x0 := !x0 +. float !sx ;
            dx := !dx +. !xy ;
            dy := !dy +. !yy ; err := !err +. !dy ;
          end ;
          (* line 43: *)
          Logs.debug (fun m -> m "dy:%f <= xy:%f && dx:%f >= xy:%f"
                         !dy !xy !dx !xy);
          if !dy <= !xy && !dx >= !xy
          then do_while () else ()
        in
        try do_while () with Not_found -> ()
      end
    end ;
    let p0' = P2.v !x0 !y0 in
    let p2' = P2.v !x2 !y2 in
    if p0' <> p2' && p0' <> stop && !drawing then begin
      Logs.debug (fun m -> m "%s: drawing line because p0'<>p2'" __LOC__);
      path := Path.add_line p0' p2' !path window
    end;
    Path.remove_gaps !path
    |> Path.union orig_path

  let plotRotatedEllipseRect ~begin_angle ~stop_angle ~stop ~clockwise ~large orig_path x0 y0 x1 y1 zd window : Path.t =
    (* freely after program listing 13, Bresenham.pdf *)
    let path = ref Path.empty in
    if zd < Stdlib.Float.epsilon && zd > ~-.Stdlib.Float.epsilon then begin
      (* TODO = 0. is probably not good, is this above ok? *)
      Logs.debug (fun m -> m "cool, no rotation for this ellipse");
      plotEllipseRect ~stop ~clockwise ~large !path x0 y0 x1 y1
      |> Path.remove_gaps |> Path.union orig_path
    end else
      let xd : int = x1 - x0 and yd = y1 - y0 in
      let w : float = float @@ xd * yd in
      let w:float = if w <> 0.0 then (w-.zd)/.(w+.w)
        else w in (*squared weight*)
      Logs.debug (fun m -> m "plotRotatedEllipseRect w:%f" w);
      assert (w <= 1.0 && w >= 0.0) ; (* limit angle *)
      let xd = int_of_float @@ floor (float xd *. w +. 0.5)
      and yd = int_of_float @@ floor (float yd *. w +. 0.5) in
      (* ^ snap xe,ye to int *)

      let flip_unclockwise p1 mid p3 = if true (*clockwise*) (* TODO *)
        then p1, mid, p3 else p3, mid, p1 in
      (* point are given in clockwise order: *)
      let p'4'1, p'4'2, p'4'3 = (* 1:30*)
        flip_unclockwise (* w, 4*)
          (P2.v (float x1) (float @@ y1-yd))
          (P2.v (float x1) (float y0))
          (P2.v (float (x0+xd)) (float y0)) in
      let p'2'1, p'2'2, p'2'3 = (* 10:30 *)
        flip_unclockwise (* w, 2 *)
          (P2.v (float x0) (float y0 +. float yd))
          (P2.v (float x0) (float y1))
         (P2.v (float (x1 - xd)) (float y1)) in
      let p'3'1, p'3'2, p'3'3 = (* 7:30 *)
        (* points p0 and p2 rearranged here (in contrast to Listing 13) to
           make all our drawings go clockwise*)
        flip_unclockwise (* 1.0 -w, 1 *)
          (P2.v (float (x0+xd)) (float y0))
          (P2.v (float x0) (float y0))
          (P2.v (float x0) (float (y0 + yd)))
      in
      let p'1'1, p'1'2, p'1'3 = (* 4:30 *)
        flip_unclockwise (* 1.0-w, 3*)
          (P2.v (float x1) (float (y1 - yd)))
          (P2.v (float x1) (float y1))
          (P2.v (float @@ x1 - xd) (float y1))
      in
      let q1,q2,q3,q4 = quadrants_touched ~large ~clockwise begin_angle stop_angle in
      Logs.debug (fun m -> m "XYZ ellipse arc from [%d+%d,%d+%d] [%d,%d] to %a@.\
                              XYZ q1:%b q2:%b q3:%b q4:%b@.\
                             "
                     x0 xd y0 yd
                     x1 y1
                     V2.pp stop
                     q1 q2 q3 q4
                 );
      assert (let xs = V2.x stop |> int_of_float in
              x0 <= xs && xs <= x1);
      assert (let ys = V2.y stop |> int_of_float in
              y0 <= ys && ys <= y1);
      begin
          let first = List.find (V2.equal (P2.v (float x0) (float y0)))
              [p'1'1 ; p'2'1; p'3'1 ; p'4'1 ;
               p'1'3 ; p'2'3; p'3'3 ; p'4'3 ;
              ] in
          Logs.debug (fun m -> m"first quad starting point is %a" V2.pp first);
          let plot_quad num p0 p1 p2 w =
            Logs.debug (fun m -> m "Plotting quadrant %d: %a -> %a -> %a"
                           num V2.pp p0 V2.pp p1 V2.pp p2);
            path := plotQuadRationalBezierSeg ~stop !path p0 p1 p2 w window ;
            Logs.debug (fun m -> m "||||| plotted quadrant %d@." num)
          in
          if q1 then plot_quad 1 p'1'1 p'1'2 p'1'3  w;
          if q2 then plot_quad 2 p'2'1 p'2'2 p'2'3  w;
          if q3 then plot_quad 3 p'3'1 p'3'2 p'3'3  (1.0 -. w);
          if q4 then plot_quad 4 p'4'1 p'4'2 p'4'3  (1.0-.w);
      end ;
      Path.remove_gaps !path |> Path.union orig_path

  let plotQuadBezierSeg p0 p1 p2 path fb_t =
    let path = ref path in
    (* plot a limited quadratic Bezier segment *)
    let x0, y0 = ref @@ P2.x p0 , ref @@ P2.y p0 in
    let x1, y1 = ref @@ P2.x p1 , ref @@ P2.y p1 in
    let x2, y2 = ref @@ P2.x p2 , ref @@ P2.y p2 in
    let sx = ref @@ !x2 -. !x1 and sy = ref @@ !y2 -. !y1 in
    let xx = ref @@ !x0 -. !x1 and yy = ref @@ !y0 -. !y1
    and xy = ref 0. in
    (* relative values for checks *)

    let dx = ref 0.0 and dy = ref 0.0 and err = ref 0.0
    and cur = ref (!xx *. !sy -. !yy *. !sx)     (* curvature *) in

    assert(!xx *. !sx <= 0.
           && !yy *. !sy <= 0.); (* sign of gradient must not change *)

    if (!sx *. !sx +. !sy *. !sy > !xx *. !xx +. !yy *. !yy) then begin
      (* begin with longer part *)
      x2 := !x0;
      x0 := !sx +. !x1;
      y2 := !y0;
      y0 := !sy +. !y1;
      cur := ~-. !cur; (* swap P0 P2 *)
    end ;

    if (!cur <> 0.) then begin
      (* no straight line *)

      xx := !xx +. !sx ;
      sx := if !x0 < !x2 then 1.0 else ~-. 1. ;
      xx := !xx *. !sx ; (* x step direction *)

      yy := !yy +. !sy ;
      sy := if !y0 < !y2 then 1. else ~-. 1. ;
      yy := !yy *. !sy ; (* y step direction *)

      xy := 2. *. !xx *. !yy ;
      xx := !xx *. !xx ;
      yy := !yy *. !yy ; (* differences 2nd degree *)

      if (!cur *. !sx *. !sy < 0.) then begin
        (* negated curvature? *)
        xx := ~-. !xx;
        yy := ~-. !yy;
        xy := ~-. !xy;
        cur := ~-. !cur;
      end ;

      dx := 4.0 *. !sy *. !cur *. (!x1 -. !x0) +. !xx -. !xy;
      (* differences 1st degree *)
      dy := 4.0 *. !sx *. !cur *. (!y0 -. !y1) +. !yy -. !xy;
      xx := !xx +. !xx;
      yy := !yy +. !yy;
      err := !dx +. !dy +. !xy; (* error 1st step *)
      let exception Exit_fast in
      let rec do_while () =
        path := Path.add_point
            ~x:(int_of_float !x0)
            ~y:(int_of_float !y0) !path ;
          (* plot curve *)
          if (x0 = x2 && y0 = y2) then raise_notrace Exit_fast;
            y1 := if 2. *. !err < !dx then 1.0 else 0.0 ;
            (* last pixel -> curve finished *)
            (* save value for test of y step *)
            if (2. *. !err > !dy) then begin
              x0 := !x0 +. !sx ;
              dx := !dx -. !xy;
              dy := !dy +. !yy ;
              err := !err +. !dy ;
            end ; (* x step *)
            if (!y1 <> 0.) then begin
              y0 := !y0 +. !sy;
              dy := !dy -. !xy ;
              dx := !dx +. !xx ;
              err := !err +. !dx ;
            end ; (* y step *)
     if (!dy < 0. && !dx > 0.) then do_while () else ()
      in try do_while () with Exit_fast -> ()
      (* gradient negates -> algorithm fails *)
    end ;
    (* plot remaining part to end: *)
    Path.add_line (P2.v !x0 !y0) (P2.v !x2 !y2) !path fb_t


  let plotQuadBezier p0 p1 p2 orig_path fb_t =
    (* plot any quadratic Bezier curve *)
    let path = ref Path.empty in
    let x0, y0 = ref@@ P2.x p0, ref@@ P2.y p0 in
    let x1, y1 = ref@@ P2.x p1, ref@@ P2.y p1 in
    let x2, y2 = ref@@ P2.x p2, ref@@ P2.y p2 in

    let x = ref @@ !x0 -. !x1 and y = ref @@ !y0 -. !y1 in
    let t = ref @@ !x0 -. 2. *. !x1 +. !x2
    and r = ref 0. in

    if (!x *. (!x2 -. !x1) > 0.) then begin
      (* horizontal cut at P4? *)
      if (!y *. (!y2 -. !y1) > 0.0) then
        (* vertical cut at P6 too? *)
        if (Stdlib.Float.abs
              ((!y0 -. 2. *. !y1 +. !y2) /. !t *. !x)
            > Stdlib.Float.abs !y) then begin
          (* which first? *)
          x0 := !x2 ;
          x2 := !x +. !x1 ;
          y0 := !y2 ;
          y2 := !y +. !y1 ;
        end ;

      (* swap points *)
      (* now horizontal cut at P4 comes first *)
      t := (!x0 -. !x1) /.  !t;
      r := (1. -. !t)*.((1. -. !t)*. !y0 +. 2.0 *. !t *. !y1)+. !t*. !t*. !y2;
      t := (!x0*. !x2-. !x1*. !x1)*. !t /. (!x0-. !x1);
      (* By(t=P4) *)
      (* gradient dP4/dx=0 *)
      x := floor(!t +. 0.5);
      y := floor(!r+.0.5);
      r := (!y1-. !y0)*.(!t-. !x0)/.(!x1-. !x0)+. !y0;

      (* intersect P3 | P0 P1 *)
      path := plotQuadBezierSeg
        (P2.v !x0 !y0)
        (P2.v !x (floor(!r+.0.5)))
        (P2.v !x !y) !path fb_t ;
      path := plotQuadBezierSeg (P2.v !x0 !y0)
        (P2.v !x (floor(!r+.0.5))) (P2.v !x !y) !path fb_t;
      r := (!y1 -. !y2)*.(!t-. !x2)/.(!x1-. !x2)+. !y2;
      x1 := !x ;
      x0 := !x1 ;
      y0 := !y;
      y1 := floor (!r+.0.5);
      (* intersect P4 | P1 P2 *)
      (* P0 = P4, P1 = P8 *)
    end ;

    if (int_of_float ((!y0 -. !y1) *. (!y2 -. !y1)) > 0) then begin

      (* vertical cut at P6? *)
      t := !y0-. 2.*. !y1+. !y2;
      t := (!y0-. !y1)/. !t;
      r := (1.-. !t)*.((1.-. !t)*. !x0+. 2.0*. !t*. !x1)+. !t*. !t*. !x2;
      t := (!y0*. !y2-. !y1*. !y1)*. !t/.(!y0-. !y1);
      (* Bx(t=P6) *)
      (* gradient dP6/dy=0 *)
      x := floor(!r+.0.5);
      y := floor(!t+.0.5);
      r := (!x1-. !x0)*.(!t-. !y0)/.(!y1-. !y0)+. !x0;

      (* intersect P6 | P0 P1 *)
      path := plotQuadBezierSeg (P2.v !x0 !y0) (P2.v (floor(!r+.0.5)) !y)
          (P2.v !x !y) !path fb_t ;
      r := (!x1-. !x2)*.(!t-. !y2)/.(!y1-. !y2)+. !x2;
      x0 := !x;
      x1 := floor(!r+.0.5);
      y1 := !y ;
      y0 := !y1 ;
      (* intersect P7 | P1 P2 *)
      (* P0 = P6, P1 = P7 *)
    end ;
    (* remaining part: *)
    path := plotQuadBezierSeg
        (P2.v !x0 !y0) (P2.v !x1 !y1) (P2.v !x2 !y2) !path fb_t ;
    (* TODO removing gaps is not good if the curve crosses itself,
       is that something we need to handle here? *)
    Path.union orig_path (Path.remove_gaps !path)

  let plotRotatedEllipse ~begin_angle ~stop_angle ~stop ~clockwise ~large path xy
      radii angle fb_t : Path.t =
    (* Listing 13: Bresenham.pdf: plot ellipse rotated by angle (radian) *)
    (* xy: center *)
    let x,y = P2.x xy, P2.y xy in
    (* ab: radii *)
    let a,b = P2.x radii , P2.y radii in


    let xd = a *. a and yd = b *. b in
    let s = sin angle in
    let zd = (xd -. yd) *. s in (* ellipse rotation *)
    let xd = sqrt (xd -. zd *. s) in
    let yd = sqrt (yd +. zd *. s) in (* surrounding rectangle *)

    let a = xd +. 0.5 and b = yd +. 0.5 in
    let zd = (zd *. a *. b) /. (xd *. yd) in (* scale to integer*)
    Logs.debug (fun m -> m "xd:%f yd:%f a:%f zd:%f" xd yd a zd);
    let i = int_of_float in
    plotRotatedEllipseRect ~begin_angle ~stop_angle ~stop ~clockwise ~large path
      (i@@ x-.a) (i@@ y-.b)
      (i@@ x+.a) (i@@ y+.b)
      (4. *. zd *. cos angle) fb_t

  let plotCubicBezierSeg p0 p1 p2 p3 path fb_t =
    (* plot limited cubic Bezier segment *)
    let (x0, y0) : float ref * float ref = ref@@P2.x p0, ref@@P2.y p0 in
    let (x1, y1) : float ref * float ref = ref@@P2.x p1, ref@@P2.y p1 in
    let (x2, y2) : float ref * float ref = ref@@P2.x p2, ref@@P2.y p2 in
    let (x3, y3) : float ref * float ref = ref@@P2.x p3, ref@@P2.y p3 in
    let path = ref path in
    let f = ref 0. and fx = ref 0. and fy = ref 0. and leg = ref 1 in

    (* step direction: *)
    let sx = ref @@ if x0 < x3 then 1. else -1.
    and sy = ref @@ if y0 < y3 then 1. else -1. in

    let ( * ) = ( *. )
    and ( / ) = ( /. )
    and ( + ) = ( +. )
    and ( - ) = ( -. ) in

    let xc : float = ~-. (Stdlib.Float.abs (!x0+. !x1-. !x2 -. !x3)) in
    let xa : float = xc -4.* !sx*(!x1- !x2)
    and xb : float ref = ref @@ !sx*(!x0- !x1- !x2+ !x3)
    and yc : float = ~-. (Stdlib.Float.abs(!y0 + !y1- !y2- !y3)) in
    let ya = yc -4.* !sy *(!y1- !y2)
    and yb : float ref = ref @@ !sy*(!y0- !y1- !y2+ !y3) in
    let ab = ref 0.0
    and ac = ref 0.0 and bc = ref 0.0
    and cb = ref 0.0 and xx = ref 0.0
    and xy = ref 0.0 and yy = ref 0.0
    and dx = ref 0.0 and dy = ref 0.0
    and ex = ref 0.0
    and pxy : float ref ref = ref (ref 0.0)
    and ep = ref 0.01
    in

    (* check for curve restrains *)
    (* slope P0-P1 == P2-P3 and (P0-P3 == P1-P2 or no slope change) *)
    assert(( !x1- !x0)*( !x2- !x3) < !ep
           && ((!x3- !x0)*( !x1- !x2) < !ep || !xb* !xb < xa*xc+ !ep));
    assert((!y1- !y0)*(!y2- !y3) < !ep
           && ((!y3- !y0)*(!y1- !y2) < !ep ||  !yb* !yb < ya*yc+ !ep));
    if (xa = 0. && ya = 0.) then begin
      (* quadratic Bezier *)
      sx := floor((3.*. !x1-. !x0+.1.)/.2.);
      sy := floor((3.*. !y1-. !y0+.1.)/.2.);
      (* new midpoint *)
      path := plotQuadBezierSeg
          (P2.v !x0 !y0)
          (P2.v !sx !sy)
          (P2.v !x3 !y3) !path fb_t ;
    end ;
    x1 := ( !x1 -. !x0)*.( !x1-. !x0)+.(!y1-. !y0)*.(!y1-. !y0)+.1.;
    (* line lengths *)
    x2 := ( !x2 -. !x3)*.( !x2-. !x3)+.(!y2-. !y3)*.(!y2-. !y3)+.1.;
    let rec do_while1 () =
      let exception Goto_Exit in
      (* loop over both ends *)
      ab := xa*. !yb-. !xb*. ya;
      ac := xa*. yc-. xc*. ya;
      bc := !xb*. yc-. xc*. !yb;
      ex := !ab*.(!ab+. !ac-. 3. *. !bc)+. !ac*. !ac;
      (* P0 part of self-intersection loop? *)

      f := if !ex > 0. then 1. else sqrt(1.+.1024./. !x1);
      (* calculate resolution *)

      ab := !ab *. !f ;
      ac := !ac *. !f ;
      bc := !bc *. !f ;
      ex := !ex *. (!f  *. !f); (* increase resolution *)

      xy := 9.*.(!ab+. !ac+. !bc)/.8.;
      cb := 8.*.(xa-. ya);(* init differences of 1st degree *)
      dx := 27.*.(8.*. !ab*.(!yb*. !yb-. ya*. yc)+.
                  !ex*.(ya+. 2.*. !yb+. yc))/.64.
            -. ya *. ya*.(!xy-. ya);
      dy := 27.*.(8.*. !ab*.(!xb*. !xb-. xa*. xc)
                  -. !ex*.(xa+. 2. *. !xb+. xc)
                 )/. 64.-. xa*. xa*. (!xy+. xa);
      (* init differences of 2nd degree *)
      xx := 3.*(3.* !ab*(3.* !yb* !yb-ya*ya-2.*ya*yc)
                -ya*(3.* !ac*(ya+ !yb)+ya* !cb))/4.;
      yy := 3.*(3.* !ab*(3.* !xb* !xb-xa*xa-2.*xa*xc)
                -xa*(3.* !ac*(xa+ !xb)+xa* !cb))/4.;
      xy := xa*ya*(6.* !ab+6.* !ac-3.* !bc+ !cb);
      ac := ya*ya;
      cb := xa*xa;
      xy := 3.*(!xy+9.* !f*(!cb* !yb*yc- !xb*xc* !ac)
                -18.* !xb* !yb* !ab)/8.;

      if (!ex < 0.) then begin
        (* negate values if inside self-intersection loop *)
        dx := ~-. !dx;
        dy := ~-. !dy;
        xx := ~-. !xx;
        yy := ~-. !yy;
        xy := ~-. !xy;
        ac := ~-. !ac;
        cb := ~-. !cb;
      end ;
      (* init differences of 3rd degree *)
      ab := 6. *ya* !ac;
      ac := -6.*xa* !ac;
      bc := 6.*ya* !cb;
      cb := -6.*xa* !cb;
      dx := !dx + !xy;
      ex := !dx + !dy;
      dy := !dy + !xy;
      (* error of 1st step *)
      pxy := xy ;
      fy := !f ;
      fx := !fy ;
      let rec for_loop1 () =
        if x0 <> x3 && !y0 <> !y3 then raise_notrace Goto_Exit ;
        path := Path.add_point ~x:(int_of_float !x0)
            ~y:(int_of_float !y0) !path ;
        (* plot curve *)
        let rec do_while2 () =
          (* move sub-steps of one pixel *)
          if ( !dx > ! !pxy || !dy < ! !pxy) then
            raise_notrace Goto_Exit;
          (* confusing values *)
          y1 := 2.* !ex- !dy;
          (* save value for test of y step *)
          if (2.* !ex >=  !dx)  then begin
            (* x sub-step *)
            fx := !fx - 1.;
            dx := !dx + !xx ;
            ex := !ex + !dx;
            xy := !xy + !ac;
            dy := !dy + !xy;
            yy := !yy + !bc;
            xx := !xx + !ab;
          end ;
          if (!y1 <= 0.)  then begin
            (* y sub-step *)
            fy := !fy - 1.;
            dy := !dy + !yy;
            ex := !ex + !dy ;
            xy := !xy + !bc;
            dx := !dx + !xy;
            xx := !xx + !ac;
            yy := !yy + !cb;
          end ;
          if (!fx > 0. && !fy > 0.) then do_while2 () else ()
        in do_while2 () ;
        (* pixel complete? *)
        if (2.*. !fx <= !f) then begin
          x0 := !x0 + !sx;
          fx := !fx + !f;
        end ;
        (* x step *)
        if (2.* !fy <= !f) then begin
          y0 := !y0 + !sy;
          fy := !fy + !f;
        end ;
        (* y step *)
        if (!pxy == xy &&  !dx < 0. &&  !dy > 0.)
        then pxy := ep;(* pixel ahead valid *)
        for_loop1 ()
      in (try for_loop1 () with
            Goto_Exit -> ()); (* goto: exit label*)
      (* exit: *)
      xx := !x0; x0 := !x3;
      x3 := !xx;
      sx := ~-. !sx;
      xb := ~-. !xb;
      (* swap legs *)
      yy := !y0;
      y0 := !y3;
      y3 := !yy;
      sy := ~-. !sy;
      yb := ~-. !yb;
      x1 := !x2;

      (* TODO ported from: while(leg--) *)
      decr leg ;
      if !leg >= 0 then do_while1 () else ()
    in do_while1 ();
    (* try other end *)
    Path.add_line (P2.v !x0 !y0) (P2.v !x3 !y3) !path fb_t

  let plotCubicBezier p0 p1 p2 p3 (path:Path.t) fb_t =
    (* plot any cubic Bezier curve *)
    let x0,y0 = int_of_float@@ P2.x p0, int_of_float@@ P2.y p0
    and x1,y1 = int_of_float@@ P2.x p1, int_of_float@@ P2.y p1
    and x2,y2 = int_of_float@@ P2.x p2, int_of_float@@ P2.y p2
    and x3,y3 = int_of_float@@ P2.x p3, int_of_float@@ P2.y p3 in
    let path = ref path in
    let fabs = Stdlib.Float.abs in
    let n = ref 0 in
    let xc :int= x0+x1-x2-x3 in
    let xa = xc-4*(x1-x2) in
    let xb = x0-x1-x2+x3 in
    let xd = xb+4*(x1+x2) in
    let yc = y0+y1-y2-y3 in
    let ya = yc-4*(y1-y2) in
    let yb = y0-y1-y2+y3 in
    let yd = yb+4*(y1+y2) in
    let fx0 = ref (float x0)
    and fx1 = ref 0. and fx2 = ref 0. and fx3 = ref 0.
    and fy0 = ref (float y0)
    and fy1 = ref 0. and fy2 = ref 0. and fy3 = ref 0. in
    let y0 = ref y0 in
    let t1 = ref @@ float @@ xb*xb-xa*xc
    and t2 = ref 0. and t = Array.make 5 0. in
    (* sub-divide curve at gradient sign changes *)
    if xa = 0 then begin  (* horizontal *)
      if (abs xc < 2 * abs xb)
      then begin
        t.(!n) <- (float xc) /. (2.0*.(float xb));
        incr n; end;    (* one change *)
    end else if (!t1 > 0.0) then begin (* two changes *)
      t2 := sqrt !t1;
      t1 := ((float xb)-. !t2)/. float xa;
      if (fabs !t1 < 1.0) then begin t.(!n) <- !t1 ; incr n end;
      t1 := ((float xb)+. !t2)/. float xa;
      if (fabs !t1 < 1.0) then begin t.(!n) <- !t1; incr n; end
    end ;

    t1 := (float (yb*yb)) -. (float @@ ya*yc);
    if (ya = 0) then begin (* vertical *)
      if (abs (yc) < 2*abs(yb)) then begin
        t.(!n) <- float yc /. (2.0*. float yb) ;
        incr n
      end; (* one change *)
    end else if !t1 > 0.0 then begin (* two changes *)
      t2 := sqrt !t1 ;
      t1 := (float yb -. !t2)/. float ya;
      if (fabs !t1 < 1.0) then begin
        t.(!n) <- !t1; incr n end;
      t1 := (float yb +. !t2)/. float ya;
      if (fabs !t1 < 1.0) then begin t.(!n) <- !t1 ; incr n end;
    end ;

    let rec for_bubble_sort i =
      (* TODO can we maybe just call Array.sort here? *)
      if i < !n then begin  (* bubble sort of 4 points *)
        t1 := t.(i - 1);
        if (!t1 > t.(i)) then begin
          t.(i-1) <- t.(i);
          t.(i) <- !t1;
          for_bubble_sort 0 (* i = 0 *)
        end
        else for_bubble_sort (succ i)
      end else ()
    in for_bubble_sort 1 ;
    let ( * ) = ( *. ) and ( / ) = ( /. )
    and ( + ) = ( +. ) and ( - ) = ( -. ) in
    let ( *= ) = fun a b -> a := !a * b in

    t1 := -1.0;
    t.(!n) <- 1.0; (* begin / end point *)
    let x0 = ref x0
    and x3 = ref x3
    and y3 = ref y3 in
    for i = 0 to !n (* i <= n*) do (* plot each segment separately *)
      t2 := t.(i); (* sub-divide at t[i-1], t[i] *)
      let xc = float xc and xb = float xb in
      let yc = float yc and yb = float yb in
      let xa = float xa and xd = float xd in
      let yd = float yd and ya = float ya in
      fx1 := ( !t1*( !t1*xb-2.*xc)- !t2*( !t1*( !t1*xa-2.*xb)+xc)+xd)/8.- !fx0;
      fy1 := ( !t1*( !t1*yb-2.*yc)- !t2*( !t1*( !t1*ya-2.*yb)+yc)+yd)/8.- !fy0;
      fx2 := ( !t2*( !t2*xb-2.*xc)- !t1*( !t2*( !t2*xa-2.*xb)+xc)+xd)/8.- !fx0;
      fy2 := ( !t2*( !t2*yb-2.*yc)- !t1*( !t2*( !t2*ya-2.*yb)+yc)+yd)/8.- !fy0;
      fx3 := ( !t2*( !t2*(3.*xb- !t2*xa)-3.*xc)+xd)/8. ;
      fx0 := !fx0 - !fx3;
      fy3 := ( !t2*( !t2*(3.*yb- !t2*ya)-3.*yc)+yd)/8. ;
      fy0 := !fy0 - !fy3;
      x3 := int_of_float @@ floor(!fx3+0.5);
      y3 := int_of_float @@ floor(!fy3+0.5);        (* scale bounds to int *)
      if (!fx0 <> 0.0) then begin
        fx0 := (float !x0 - float !x3) / !fx0;
        fx1 *= !fx0;
        fx2 *= !fx0; end;
      if (!fy0 <> 0.0) then begin
        fy0 := ((float !y0) - float !y3) /. !fy0;
        fy1 *= !fy0;
        fy2 *= !fy0; end ;
      if (!x0 <> !x3 || !y0 <> !y3) then begin (* segment t1 - t2 *)
        path := plotCubicBezierSeg
            (P2.v (float !x0) (float !y0))
            (P2.v ((float !x0)+ !fx1) (float !y0 + !fy1))
            (P2.v (float !x0 + !fx2) (float !y0 + !fy2))
            (P2.v (float !x3) (float !y3)) !path fb_t;
      end;
      x0 := !x3; y0 := !y3; fx0 := !fx3; fy0 := !fy3; t1 := !t2;
    done ; (* end for loop *)
    !path

  let _ = quadratic_bezier_step, cubic_bezier_step

  let pp_segment ppf = function
    | `Ccurve (p1, p2, p3) ->
      Fmt.pf ppf "Ccurve p1: %a p2:%a p3:%a" V2.pp p1 V2.pp p2 V2.pp p3
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
    | `Qcurve (p1, p2) ->
      Fmt.pf ppf "Qcurve p1:%a p2:%a" V2.pp p1 V2.pp p2
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
          Logs.warn (fun m -> m "CLOSING TODO LINE COMMENTED OUT");
          (* drawing a line from cur to sub: *)
          (* TODO if we rs.sub*)
          let rs = {rs with
                    cur = rs.sub;
                    path =
                      if rs.cur = rs.sub || false (* TODO remove this false*)
                      then rs.path (* already there *)
                      else Path.add_line rs.cur rs.sub rs.path window} in
          Logs.debug (fun m -> m "%a" Path.pp rs.path);
          rs, `Ok
        | `Earc (large, clockwise, angle, radii, p2) when true (*TODO*) ->
          (* see docstring for `val earc` in vg.mli *)
          begin match P.earc_params rs.cur
                        ~large ~cw:clockwise angle radii p2 with
            | None ->
              Vg.Vgr.Private.warn r (`Other "invalid earc!!! TODO") ;
              Logs.warn (fun m -> m "INVALID EARC TODO COLLAPSE TO LINE") ;
              rs, `Partial
            | Some (center, m2, a, a') ->
              let a1'q1, a1'q2, a1'q3, a1'q4 = quadrants_touched ~large ~clockwise a a' in
              Logs.warn (fun m ->
                  m "earc helper: cur:%a stop:%a c=%a XYZ m=%a XYZ a:%f a':%f XYZ \
                     @  seg: %a@ \
                     XYZ @.a1'q1: %b a1'q2: %b a1'q3: %b a1'q4: %b"
                    V2.pp rs.cur V2.pp p2
                    V2.pp center M2.pp m2 a a'
                    pp_segment seg
                    a1'q1 a1'q2 a1'q3 a1'q4
                ) ;
              
              (*let x1, y1 =
                int_of_float@@ P2.x p2,
                int_of_float@@ P2.y p2 in
                let _x0, _y0 = Size2.w radii +.  V2.x p2,
                           Size2.h radii +.  V2.y p2 in
              *)
              (*let x1, y1 =
                int_of_float@@ Size2.w radii,
                int_of_float@@ Size2.h radii in
                let x0, y0 = int_of_float @@ V2.x rs.cur,
                           int_of_float @@ V2.y rs.cur in
                let _ = x0, y0, angle in*)

              (*let _adjustment_center =*)
              (* calculate coordinate of ellipsis center *)
              (*let rot = Gg.M3.rot2 (Gg.Float.rad_of_deg ~-.360.)*)
              (*let rot = Gg.M3.rot2 angle in
                let radii_box =
                Gg.Box2.v
                  P2.(v (x radii) 0.)
                  P2.(v 0. (y radii))in
                let adjusted = Gg.Box2.tr rot radii_box in
                Logs.debug (fun m ->
                  m "rotated: %a x<=x:%b y<=y:%b cw:%b"
                    Gg.Box2.pp adjusted
                    (P2.x rs.cur <= P2.x p2)
                    (P2.y rs.cur <= P2.x p2)
                    _clockwise
                );
                match
                P2.x rs.cur <= P2.x p2,
                P2.y rs.cur <= P2.x p2,
                _clockwise
                with
                | true, true, _   ->
                Gg.Box2.tr_pt adjusted
                |> V2.sub p2
                | false, true, _  -> Gg.Box2.br_pt adjusted
                | true, false, _  -> Gg.Box2.tl_pt adjusted
                | false, false, _ -> Gg.Box2.bl_pt adjusted
                in
                let x0, y0 =
                let p = adjustment_center in
                Logs.debug (
                fun m -> m "ellipse centre %a = %a - %a"
                    V2.pp p V2.pp p2 V2.pp adjustment_center
                );
                int_of_float (V2.x p) , int_of_float (V2.y p)
                in*)
          (*
let x0,y0 = int_of_float @@ P2.x rs.cur, int_of_float @@ P2.y rs.cur in
          let x1,y1 = int_of_float @@ P2.x _radii, int_of_float @@ P2.y _radii in
*)
              let path =
                plotRotatedEllipse ~begin_angle:a ~stop_angle:a' ~stop:p2 ~clockwise ~large
                  rs.path center radii angle window
              in
              let rs = {rs with cur = p2 ; path} in
              rs, `Ok
          end ;
        | `Qcurve (p1, p2) ->
          Logs.debug (fun m ->
              m "Qcurve from cur:%a cp:%a to:%a"
                V2.pp rs.cur V2.pp p1 V2.pp p2
            );
          let path =
            plotQuadBezier rs.cur p1 p2 rs.path window in
          let rs = {rs with cur = p2 ; path} in
          rs, `Ok
        | `Ccurve (p1,p2,p3) ->
          Logs.debug (fun m -> m "Ccurve from cur:%a p1:%a p2:%a p3:%a"
                         V2.pp rs.cur V2.pp p1 V2.pp p2 V2.pp p3);
          let path = plotCubicBezier rs.cur p1 p2 p3 rs.path window in
          let rs = {rs with cur = p3 ; path } in
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
    | Blend (blender, size1,img1, img2)  ->
      let blend = match blender with
        | `Atop -> "Atop" | `Copy -> "Copy" | `In -> "In"
        | `Out -> "Out" | `Over -> "Over" | `Plus -> "Plus" | `Xor -> "Xor" in
      Fmt.pf ppf "Blend (%s, alpha:%a, img1:%a, img2:%a)" blend
        Fmt.(option ~none:(unit"opaque") float) size1
        pp_img img1 pp_img img2
    | Tr (tr, i) ->
      Fmt.pf ppf "(Tr @[<v>%a@, @[%a@]@])" M3.pp (Data.tr_to_m3 tr) pp_img i

  let rec render_image ~window rs r (o_img:Data.image) =
    Logs.warn (fun m -> m "img: %a" pp_img o_img) ;
    match o_img with
    | Primitive Const v4 ->
      Logs.warn (fun m -> m "Primitive Const Color %a"
                    Gg.V4.pp
                    @@ Gg.Color.to_srgb v4);
      let color = fb_color ~window v4 in
      { rs with const = color ; } ,
      `Ok (* set draw color *)
    | Primitive (Axial _ | Radial _ | Raster _) ->
      Logs.warn (fun m -> m "Primitive color not implemented ??");
      rs, `Partial
    | Cut (`Anz,p,i) ->
      Logs.warn (fun m -> m "Cut ANZ");
      let rs, _ = render_image ~window rs r i in
      let rs, status = r_cut window rs r p in
      Path.render_fill rs.path window rs.const ;
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
    | Tr (tr, img) ->
      let m3 = Data.tr_to_m3 tr in
      Logs.warn (fun m -> m "(?????? TR %a %a)" M3.pp m3 pp_img img);
      (* TODO this still needs to be done properly,
         the stuff below just emits a ne wTr : *)
      (*
      let transformed_img = Vg.I.tr m3 (I.of_data img) in
      let rs , _ = render_image ~window rs r (Data.of_image transformed_img) in*)
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
