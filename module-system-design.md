# Module System Design

The module system is designed to be portable to any conforming Scheme implementation (conforming to which standard?), and to "slide in alongside" an existing file of Scheme code.

It is also "transparent", in that the programmer has access to internal implementations (variables, procedures) in the main namespace, but in an obfuscated way that can be easily ignored.  This is something of an accident, but may be a useful property nonetheless, even if it is "messy".

What do I mean by "slide in alongside"?

To make a module:

Given a file `utils.scm` that has some procedures that you'd like to become the basis of a module, add a file called `utils.mod` to the same directory (naming is still in flux).

The structure of `utils.mod` is as follows:

```
(define-module utils
  (exports random-integer random-char atom?))
```

The module system code (which all fits in one file) will read the module definition from `utils.mod` and load the corresponding Scheme code file `utils.scm`, with the following behavior:

+ It will read all top-level definitions in the file, e.g. `(define (foo) 'FOO)`

+ If the top-level definition is not one of the exported variables or procedures from `utils.mod`, it will rewrite the name as a "gensym" (generated symbol), including in all of the calling code from that file.

Thus, each load of the module generates a fresh set of the internal "helper" etc. procedures that are used, in addition to reloading the exported procedures.  This hinges on the module system's implementation of `GENSYM` working well enough to generate fresh names, which in practice, it does.

Consider the following contents of `utils.scm`:

```
;; Generate random numbers using Sedgewick, 2nd ed., p. 513

(define *random-seed* 2718281828)
(define *random-constant* 31415821)

(define (random-integer n)
  (let ((answer #f))
    (begin
      (set! *random-seed*
        (modulo (+ (* *random-seed* *random-constant*) 1) n))
      (set! answer *random-seed*)
      answer)))

(define (random-char . seed)
  ;; A-Z :: 65-90
  ;; a-z :: 97-122
  (let ((i (mod (random-integer 10000000) 122)))
    (integer->char i)))

;; Testing for atoms.

(define (atom? x)
  (not (or (vector? x) (pair? x) (null? x))))
```

In this file, only the procedures `ATOM?`, `RANDOM-INTEGER`, and `RANDOM-CHAR` are meant for export.  The variables related to random number generation are for internal use only.  Therefore, we need to rewrite those variables using the gensyms facility.

## Limitations

A limitation of this module system is that it uses the host Scheme's `READ`.  This means that e.g. if Larceny's reader does not like Gambit's convention of using `(##foo)` to denote internal-to-Gambit procedures, Larceny will barf.  Or another example: if Scheme48 tries to read in some JScheme code that uses the Javadot syntax, Scheme48 will signal an error, since it wants everything starting with a `.` to be a number.
