# `load-module`, a portable module system for Scheme

*load-module* is a portable module system for Scheme.

It works in the following implementations:

+ Larceny
+ Chez
+ Gambit

## Overview

*load-module* is a single-file module system that lives "off to the side" of your files of "regular" Scheme code.

Why develop a module system?

1. For fun, and to learn - this is basically a toy, but it does also work!  Which is nice!

2. For use in personal projects - some Schemes don't have a module system; worse, implementations often have **wildly different, incompatible** module systems, which sucks and may even be worse than nothing.

3. Because this is Scheme, damn it!  You don't need the language implementor to agree to every little thing, you can just do it yourself.

The way it works is, you have a file of Scheme code, something like this:

```
;; utils.scm

(define *random-seed* 2718281828)
(define *random-constant* 31415821)

(define *chars* '(#\a #\s #\o #\r #\t #\i #\n #\g #\e #\x #\a #\m #\p #\l #\e))
(define *strings* '("a" "s" "o" "r" "t" "i" "n" "g" "e" "x" "a" "m" "p" "l" "e"))

(define (random-integer n)
  (let ((answer #f))
    (begin
      (set! *random-seed*
        (modulo (+ (* *random-seed* *random-constant*) 1) n))
      (set! answer *random-seed*)
      answer)))

(define (atom? x)
  (not (or (vector? x) (pair? x) (null? x))))
  
(define (take xs i)
  (let loop ((xs xs) (ys '()) (i i))
    (cond ((null? xs) (reverse ys))
          ((zero? i) (reverse ys))
          (else (loop (cdr xs) (cons (car xs) ys) (- i 1))))))
```

You don't want to export any of the variables used by your code.  You only want to provide a few procedures to the module's users (probably yourself).

With *load-module*, you don't need to modify the Scheme code to make it a module or library.  You create another file that lives alongside your code, called (in this case) *utils.mod*.  The module definition is very simple:

```
(define-module utils
  (exports random-integer atom? take))
```

(Note: I found some other uses of `define-module` in the Scheme wild.  I don't use those implementations, but if you do it's trivial to replace the use of `define-module` in the module definition with something else you prefer -- one of the benefits of keeping it simple.)

Now, assuming you've already loaded `load-module.scm`, you have access to a new procedure, `load-module`.

`load-module` reads in this module file and loads the three procedures you want from `utils.scm` into your environment.  Run it like so:

```
> (load "load-module.scm")
> (load-module 'utils)
#t
> (random-integer 199)
76
> (atom? 199)
#t
```

## How it works

1. You hang a module definition file off to the side of your "normal"
   Scheme code.  If it's `utils.scm`, this file should be called
   `utils.mod`.

2. *load-module* parses the module definition and learns which symbols will
   be exported definitions.

3. *load-module* reads in your file of Scheme code, and learns about all of
   your top-level definitions, e.g., `(define x 1)` or `(define (foo
   x) (+ x x))`

4. *load-module* rewrites your file of Scheme code so that all of the
   internally defined procedures and variables are replaced with
   gensyms.  Then, it reads in and `eval`s the rewritten code.

5. In your REPL, you now have access to the public definitions from
   `utils.scm` as defined by `utils.mod` (in this case,
   `RANDOM-INTEGER`, `ATOM?`, etc.).  Any internal variables are still
   technically available in your top-level, but they now have names
   like `%--gensym-utils-*random-number*-7981237123`.  This means that
   they are effectively "invisible" and thus "private".  There's no
   real way for you to conveniently access them (unless you edit
   `utils.mod` and reload the module).

## Limitations

*load-module* is meant for use at the REPL.  You can't drive it in an
automated way (it lacks the necessary features).

It doesn't know how to "Require module X from inside this module".
This may be added.

It doesn't do anything with "load paths" or what-have-you.  It assumes
that you have everything you need either from the implementation or
locally stored code.  It assumes your project structure is a flat
directory of Scheme files, e.g.:

```
$ ls myproj
README.md
mergesort.scm
mergesort.mod
sugar.scm
sugar.mod
utils.scm
utils.mod
```

At the end of the day, *load-module* is a "toy" module system, but you may
find it useful if:

+ You prefer to develop and build from the REPL

+ You want to write code that you can run across multiple Scheme
  implementations in a portable, low-overhead way

+ You work on projects that don't touch too much of the
  implementation's runtime (once you get into using runtime-specific
  code such as for networking or advanced file operations, you're tied
  to the implementation)

+ You work on projects that include all of their dependencies locally
  (a.k.a. "vendoring")
