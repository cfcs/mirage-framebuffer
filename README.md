# Work in progress, nothing usable here.

### dependencies

```
opam pin mirage-framebuffer --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'

opam pin mirage-framebuffer-tsdl --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'

opam pin mirage-qubes --dev -k git 'https://github.com/cfcs/mirage-qubes#guid_mvar'
opam pin mirage-xen --dev -k git 'https://github.com/cfcs/mirage-platform#virt_to_mfn'
opam pin mirage-framebuffer-qubes --dev -k git 'https://github.com/cfcs/mirage-framebuffer#master'
```

### test SDL backend

```bash
make test
./_build/default/test_tsdl/test_tsdl.exe
```
