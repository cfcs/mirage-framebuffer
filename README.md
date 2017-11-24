# Work in progress, nothing stable here.

# Mirage-framebuffer

This repository contains a work-in-progress framebuffer graphics library for [MirageOS](https://mirage.io), written in the [OCaml](https://ocaml.org) programming language.

The library exposes basic operations such as pixel blitting, resize and input / window resize events.

The codebase is still under development and the interfaces are very volatile and change often.
Contributions in the form of code, comments, ideas, research, or bug reports are *very* welcome, but please give me a heads-up in the form of a Github issue if you plan to introduce large changesets that may conflict with my local branches.

### Backends

Currently two backends are supported:

- `mirage-framebuffer-tsdl`: A backend using [Daniel Bünzli](https://github.com/dbuenzli)'s [tsdl library](http://erratique.ch/software/tsdl) that exposes bindings to [libSDL](https://www.libsdl.org)

- `mirage-framebuffer-qubes`: A backend that builds on [Thomas Leonard](https://github.com/talex5)'s [mirage-qubes library](https://github.com/talex5/mirage-qubes) to implement the [QubesOS-GUId](https://www.qubes-os.org/doc/gui/) protocol for graphics from inside a MirageOS unikernel running as a QubesOS AppVM.

I also [have plans](https://github.com/cfcs/mirage-framebuffer/issues/1) to implement an RDP/VNC backend that would act as a server exposing your graphical interfaces over a network connection.

Another backend I would like to see would be one using [BuckleScript](https://bucklescript.github.io/bucklescript/) or [js_of_ocaml](http://ocsigen.org/js_of_ocaml/) to draw on a [HTML5 Canvas](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) inside a web browser.

### Helpers for integration with other libraries

Included in this repository are some optional packages that let you use
existing OCaml libraries with `mirage-framebuffer`.

They define functors to be applied with a framebuffer module, for example `imagelib` can be instantited with the TSDL backend:
`Framebuffer_image.Make( Framebuffer.Make(Framebuffer_tsdl) )`.

- `mirage-framebuffer-imagelib`: Helper library defining `Framebuffer_image` for [Rodolphe Lepigre](https://github.com/rlepigre)'s [imagelib](https://github.com/rlepigre/ocaml-imagelib), a pure picture format parser.

- `mirage-framebuffer-notty`: Helper library defining `Framebuffer_notty` for use with [David Kaloper](https://github.com/pqwy)'s [notty](https://github.com/pqwy/notty), a declarative (text-based) terminal library.

I hope that there will one day be a helper library for Daniel Bünzli's [vg library](http://erratique.ch/software/vg) for declarative 2D graphics, but I am waiting in the hope that someone will do the hard work of writing a TTF/OTF font rasterizer to enable this effort, so there is no active development on this.

### Using the library

While it is intended to be used from MirageOS unikernels, taking advantage of its parametric compilation tooling to automatically select a backend, the library may also be used directly from your regular OCaml programs.

The main module defines a functor, `Framebuffer.Make`, which is applied with your backend of choice.

An example is included in [test_tsdl/test_tsdl.ml](./test_tsdl/test_tdl.ml#L80).


### Installing dependencies

```
# Patches to Mirage:
opam pin add -n mirage 'https://github.com/cfcs/mirage#fix_qubes'

# The main Framebuffer library:
opam pin add -n mirage-framebuffer 'https://github.com/cfcs/mirage-framebuffer.git#master'

# imagelib helper:
opam pin add -n mirage-framebuffer-imagelib 'https://github.com/cfcs/mirage-framebuffer.git#master'

# Notty helper:
opam pin add -n mirage-framebuffer-notty 'https://github.com/cfcs/mirage-framebuffer.git#master'

# TSDL backend:
opam pin add -n mirage-framebuffer-tsdl 'https://github.com/cfcs/mirage-framebuffer.git#master'

# QubesOS backend and required patched packages:
opam pin add -n mirage-xen   'https://github.com/cfcs/mirage-platform.git#virt_to_mfn'
opam pin add -n mirage-qubes 'https://github.com/cfcs/mirage-qubes.git#guid_mvar'
opam pin add -n mirage-framebuffer-qubes 'https://github.com/cfcs/mirage-framebuffer.git#master'

opam install mirage-framebuffer{,-tsdl,-qubes,-notty,-imagelib}
```

### Sample TSDL backend

```bash
make test
./_build/default/test_tsdl/test_tsdl.exe
```

### Projects using mirage-framebuffer:

- [eye-of-mirage](https://github.com/cfcs/eye-of-mirage), my image viewer.
