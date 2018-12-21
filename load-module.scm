;;; LOAD-MODULE: A portable Scheme module system.

;; +++ It could be nice to wrap this in syntax somehow (one
;; possibility shown below).  However, I wasn't very happy with my
;; first attempt, so I'm reverting back to using a procedure for now!
;;
;; (define-syntax load-module
;;   (syntax-rules ()
;;     ((load-module module ...)
;;      (load-module* (quote module ...)))))

(define (load-module module)

  ;; Utilities

  (define (take xs i)
    ;; List Integer -> List
    (let loop ((xs xs) (ys '()) (i i))
      (cond ((null? xs) (reverse ys))
            ((zero? i) (reverse ys))
            (else (loop (cdr xs) (cons (car xs) ys) (- i 1))))))

  (define (assoc* a as)
    ;; Symbol List -> List
    (let ((val (assoc a as)))
      (if val (cdr val) #f)))

  ;; Gensyms

  (define *random-seed* 2718281828)
  (define *random-constant* 31415821)

  (define (random-integer n)
    ;; Integer -> Integer
    (let ((answer #f))
      (begin
        (set! *random-seed*
              (modulo (+ (* *random-seed* *random-constant*) 1) n))
        (set! answer *random-seed*)
        answer)))

  (define (gensym module sym)
    ;; Symbol Symbol -> Symbol
    (let* ((i (random-integer 10000000000))
           (num (number->string i))
           (root "%--gensym")
           (module* (symbol->string module))
           (sym* (symbol->string sym))
           (gensym (string-append root "-" module* "-" sym* "-" num))
           (gensym* (string->symbol gensym)))
      gensym*))

  (define (namespace module sym)
    ;; Symbol Symbol -> Symbol
    (let* ((module* (symbol->string module))
           (sym* (symbol->string sym))
           (namespaced (string-append module* "/" sym*))
           (namespaced* (string->symbol namespaced)))
      namespaced*))

  ;; Parsing module definitions

  (define (parse-module-definition tree)
    ;; List -> List
    (let ((modname (cadr tree))
          (exports (cdaddr tree))
          (requires (maybe-parse-requires tree)))
      (list (cons 'name (list modname))
            (cons 'exports exports)
            (cons 'requires requires))))

  (define (maybe-parse-requires tree)
    ;; List -> List
    (if (= (length tree) 3)
        '()
        (let ((requires (cdr (cadddr tree))))
          requires)))

  (define (parse-module-file file)
    ;; Pathname -> List
    (call-with-input-file file
      (lambda (input-port)
        (let ((code (read input-port)))
          (parse-module-definition code)))))

  (define (get-module-name module)
    ;; List -> List
    (assoc* 'name module))

  (define (get-module-exports module)
    ;; List -> List
    (assoc* 'exports module))

  (define (get-module-requires module)
    ;; List -> List
    (assoc* 'requires module))

  (define (get-definition-name code)
    ;; List -> Symbol
    (let ((operator (car code))
          (allowed-operators '(define define-syntax))
          (val (cadr code)))
      (cond
       ((not (member operator allowed-operators)) #f)
       ((symbol? val) val)
       ((list? val) (car val))
       (else #f))))

  ;; Parsing project files

  ;; ++ This will probably go away and live in its own project at some
  ;; point.  This code isn't used by anything else in the file.  See
  ;; the relevant section of the notes file in this directory.

  (define (parse-project-definition tree)
    ;; List -> List
    (let ((project-name (cadr tree))
          (modules (cdaddr tree)))
      (list (cons 'name (list project-name))
            (cons 'modules modules))))

  (define (get-project-name project)
    ;; List -> List
    (assoc* 'name project))

  (define (get-project-modules project)
    ;; List -> List
    (assoc* 'modules project))

  ;; Converting between module names and filenames

  (define (module->source-file mod)
    ;; Symbol -> Pathname
    (string-append (symbol->string mod) ".scm"))

  (define (module->module-file mod)
    ;; Symbol -> Pathname
    (string-append (symbol->string mod) ".mod"))

  (define (file->module filename)
    ;; String -> Symbol
    (define (find-char ch string)
      (let loop ((looking-at #\space)
                 (chars (string->list string))
                 (i 0))
        (if (char=? looking-at ch)
            i
            (loop (car chars)
                  (cdr chars)
                  (+ i 1)))))
    (let* ((i (find-char #\. filename))
           (xs (take (string->list filename) (- i 1)))
           (mod (string->symbol (list->string xs))))
      mod))

  ;; Code walking

  (define (gather-defined-symbols module)
    ;; Symbol -> Alist
    (let* ((module-file (module->module-file module))
           (source-file (module->source-file module))
           (module-definition (parse-module-file module-file))
           (exports (get-module-exports module-definition))
           (requires (get-module-requires module-definition)))
      (call-with-input-file source-file
        (lambda (input-port)
          (let loop ((code (read input-port)) (exports exports) (internal-symbols '()))
            (if (eof-object? code)
                (list (cons 'internal-symbols internal-symbols)
                      (cons 'exports exports))
                (let ((name (get-definition-name code)))
                  (if (member name exports)
                      (loop (read input-port) exports internal-symbols)
                      (loop (read input-port) exports (cons name internal-symbols))))))))))

  (define (map* leaf-func tree)
    ;; Procedure List -> List
    (cond ((null? tree) '())
          ((symbol? tree)
           (leaf-func tree))
          ((char? tree)
           (leaf-func tree))
          ((string? tree)
           (leaf-func tree))
          ((number? tree)
           (leaf-func tree))
          ((boolean? (car tree))
           (cons (car tree)
                 (map* leaf-func (cdr tree))))
          ((or (symbol? (car tree))
               (number? (car tree)))
           (cons (leaf-func (car tree))
                 (map* leaf-func (cdr tree))))
          (else (cons (map* leaf-func (car tree))
                      (map* leaf-func (cdr tree))))))

  (define (tree-rewrite tree rewrite-proc)
    ;; List Procedure -> List
    (map* (lambda (atom) (rewrite-proc atom)) tree))

  (define (annotate-internal-symbols module internal-symbols)
    ;; List -> Alist
    (let loop ((internal-symbols internal-symbols) (annotated '()))
      (if (null? internal-symbols)
          annotated
          (let ((name (car internal-symbols)))
            (if (equal? name #f)
                (loop (cdr internal-symbols) annotated)
                (loop (cdr internal-symbols)
                      (cons (cons name (gensym module name)) annotated)))))))

  ;; Main loop

  (let* ((module-file (module->module-file module))
         (source-file (module->source-file module))
         (module-definition (parse-module-file module-file))
         (requires (get-module-requires module-definition))
         (symbols (gather-defined-symbols module))
         (internal-symbols (assoc* 'internal-symbols symbols))
         (annotated-internal-symbols (annotate-internal-symbols module internal-symbols)))

    (begin

      ;; First, load the prerequisite modules, if any
      (if (and (list? requires)
               (not (null? requires)))
          (let* ((module-name (symbol->string module))
                 (module-key (string-append "'(" module-name " . ")))
            (display module-key)
            (display requires)
            (display ")")
            (newline)
            (for-each
             (lambda (req) (eval `(load-module (quote ,req)) (interaction-environment))) requires)))

      ;; Next, load the actual code
      (call-with-input-file source-file
        (lambda (input-port)
          (let loop ((code (read input-port)))
            (if (eof-object? code)
                #t
                (let ((code* (tree-rewrite code
                                           (lambda (atom)
                                             (if (member atom internal-symbols)
                                                 (assoc* atom annotated-internal-symbols)
                                                 atom)))))
                  (begin
                    (eval code* (interaction-environment))
                    (loop (read input-port)))))))))))

;; eof
