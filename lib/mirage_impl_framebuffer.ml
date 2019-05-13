open Mirage

type framebuffer_ty = Framebuffer_ty
let framebuffer_typ = Mirage.typ Framebuffer_ty

let framebuffer =
  impl @@ object inherit Mirage.base_configurable
    method module_name = "Framebuffer_placeholder_goes_here"
    method name = "framebuffer"
    method ty = framebuffer_typ
    method! packages : package list value =
    (Key.match_ Key.(value target) @@ begin function
      | `Xen -> [package ~min:"0.4" "mirage-qubes";
                 package "mirage-framebuffer-qubes"]
      | `Unix | `MacOSX ->
         [package "mirage-unix"; package "mirage-framebuffer-tsdl"]
      | `Qubes | `Hvt | `Virtio | _ -> []
      end)
    |> Mirage.Key.map (List.cons (package "mirage-framebuffer"))
    method! deps = []
    method! connect mirage_info _modname _args =
      Key.eval (Info.context mirage_info) @@
      Key.match_ Key.(value target) @@ begin function
        | `Unix | `MacOSX ->
            {| Lwt.return (
                 let fb =
                   let module X = Framebuffer.Make(Framebuffer_tsdl) in
                   X.init ()
                 in
                 Lwt.return ((), fb))
            |}
        | `Xen ->
            {| Lwt.return (
                 Qubes.RExec.connect ~domid:0 () >>= fun qrexec ->
                 Qubes.GUI.connect ~domid:0 () >>= fun gui ->

                 let fb =
                   let module X = Framebuffer.Make(Framebuffer_qubes) in
                   X.init gui
                 in
                 Lwt.async (Qubes.GUI.listen gui) ;
                 Lwt.async (fun () ->
                   OS.Lifecycle.await_shutdown_request ()
                   >>= fun (`Poweroff | `Reboot) ->
                   Qubes.RExec.disconnect qrexec
                 );

                 Lwt.return ((12345, qrexec, gui),fb) )
            |}
        | `Virtio | `Hvt ->
          failwith "Mirage_Framebuffer is not implemented for Virtio | Hvt"
        | `Qubes ->
          failwith "Mirage_framebuffer must be used with -t xen for Qubes"
        | _ ->
          failwith "Mirage_Framebuffer is not implemented for your target"
      end
  end
