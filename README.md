# Work in progress, nothing usable here.

### dependencies

```
opam pin add mirage --dev -k git 'https://github.com/cfcs/mirage#fix_qubes'
opam pin add mirage-framebuffer --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'

opam pin add mirage-framebuffer-tsdl --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'

opam pin add mirage-framebuffer-imagelib --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'

opam pin add mirage-qubes --dev -k git 'https://github.com/cfcs/mirage-qubes#guid_mvar'
opam pin add mirage-xen --dev -k git 'https://github.com/cfcs/mirage-platform#virt_to_mfn'
opam pin add mirage-framebuffer-qubes --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'
```

### test SDL backend

```bash
make test
./_build/default/test_tsdl/test_tsdl.exe
```
