# notes

## on eval

larceny, chez, gambit:

`(eval '(+ 1 2))`

gambit:

`(eval '(+ 1 2) '())`

scheme48:

`(eval '(+ 1 2) (interaction-environment))`

MIT:

`(eval '(+ 1 2) (the-environment))`

## on requiring other modules

The project layout model will be the same as for C: there is no package manager, so you'll need to put whatever files of Scheme code you want into the project.

A project tree will look something like this:

- project.scm
- main.scm
- vectors.scm
- json.scm
- http-client.scm
- srfi-9.scm
- srfi-1.scm
- receive.scm

The assumption is that your "main loop" will live in `main.scm` (naturally).  Yes, this follows C conventions and is somewhat arbitrary, but it's a well-known model for structuring a project.

(Note: we need to decide whether to leave `load-module` as it is -- a simple "module system" for interactive loading -- and start a new project that handles project-level builds/dependencies, or to add this functionality to `load-module`.)

`project.scm` is where the project definition will live.  It looks like:

```
(define-project foo-api-client
  (modules
    (main main)
    (vectors make-vector vector-set! vector-ref)
    (json json/parse-string json/parse-file)
    (http-client http-get)
    (srfi-9 define-record-type)
    (receive receive)
    (srfi-1 first second take drop)))
```

If a module has a dependency -- for example, the reference implementation of SRFI-1 requires `RECEIVE` -- then the dependency should appear earlier in the list of modules.  Generally speaking, the modules should be arranged in "ascending order" of how many dependencies they require - the last module should have the most dependencies.

This will require some work on the part of the project author, but should result in reliable builds once the dependency order of a project is shaken out.

You may say, "but what about the diamond dependency problem?  What about version pinning?" to which I reply, we are modeling the project structure on C, another minimalist language (which Scheme arguably is).  In C, you don't have a package manager, much less one with dependency resolution.  If you are writing a C project, then you know to include your dependencies in the project.  An obvious exception to this analogy is the C standard library, for which no equivalent exists that works across Scheme implementations (as of this writing in 2017, at least!).

There is one procedure exported, `load-project`.

