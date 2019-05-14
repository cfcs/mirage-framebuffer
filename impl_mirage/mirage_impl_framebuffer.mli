(** Mirage device for the framebuffer module.
    The [{!framebuffer} impl] implementation can be used to
    implement cross-platform UIs for your unikernels.*)

type framebuffer_ty [@@inline]
(** Type of a framebuffer device.
    You probably don't need this unless you're writing your own
    framebuffer implementation external to this module.*)

val framebuffer : framebuffer_ty Mirage.impl [@@inline]
(** The default implementation of a framebuffer device.
    This is probably the value you need from this module.*)

val framebuffer_typ : framebuffer_ty Mirage.typ [@@inline]
(** Functoria type of a framebuffer device.
    You probably don't need this unless you're writing your own
    framebuffer implementation external to this module.*)
