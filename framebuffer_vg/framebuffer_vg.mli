module Vgr_mirage_framebuffer :
  functor (FB:Framebuffer.S) ->
  sig
    val fb_color : window:FB.t -> Gg.v4 -> FB.color
    val target : window:FB.t ->unit ->
      Vg.Vgr.dst Vg.Vgr.target
  end
