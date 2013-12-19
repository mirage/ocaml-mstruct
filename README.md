## Mstruct

Mstruct is a thin mutable layer on top of [cstruct](https://github.com/mirage/ocaml-cstruct).

### Examples

```ocaml
# #require "mstruct";;
# Log.set_log_level Log.DEBUG;;
# let b = Mstruct.create 9;;
val b : Mstruct.t = <abstr>
# Mstruct.set_string b "hello";;
2013-12-19 20:09:50.610 DEBUG: set (5)
2013-12-19 20:09:50.610 DEBUG: --> [[ t=[0,9](9) "xxxxxxxxx" ]]
2013-12-19 20:09:50.610 DEBUG: <-- [[ t=[5,4](9) "helloxxxx" ]]
- : unit = ()
# Mstruct.set_int32 b 32l;;
Error: Unbound value Mstruct.set_int32
Did you mean set_uint32?
# Mstruct.set_uint32 b 32l;;
2013-12-19 20:10:31.187 DEBUG: set (4)
2013-12-19 20:10:31.187 DEBUG: --> [[ t=[5,4](9) "helloxxxx" ]]
2013-12-19 20:10:31.187 DEBUG: <-- [[ t=[9,0](9) "hello\000\000\000 " ]]
- : unit = ()
```