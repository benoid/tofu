tofu
====

An implementation of a bf-like language in Racket.
Source code must be neatly cubed.

```
read byte:          ,
write byte:         .
increment byte:     ^
decrement byte:     _
move pointer left:  |
move pointer right: I
comment begin/end:  `
```



Hello, World in tofu
```
#lang tofu

^^^^^^[I^^^^`
^^^^^^^^|_]I.
I^^^^^^^^^^[I
^^^^^^^^^^|_]
I^.^^^^^^^..^
^^.I^^^^[I^^^

                 ^^^^
                 ^^^^
                 |_]I  .


|^^^[I___    _|_]I.
|||||^^^[    I^^^^^
|_]I.II.^    ^^.___
             ___.__
             ______ 

   .II^.
```
