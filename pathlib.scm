;;; Path Utilities for Scheme
(module pathlib (path-exists?
                 path-component-anchor
                 path-component-anchor?
                 path-component-relative-anchor
                 path-component-relative-anchor?
                 path-component-relative-to
                 path-component-relative-to?
                 path-component-parent-to
                 path-component-parent-to?
                 path-search
                 path-sep
                 path?
                 path-component?
                 path-mkdir!
                 path-parent
                 path-is-root?
                 path-is-relative-root?
                 path-write-text!
                 path
                 list->path
                 path->str)
  (import scheme)

  (import (chicken file))       ; installed w/ chicken
  (import (chicken condition))  ; installed w/ chicken
  (import srfi-1)           ; chicken-install srfi-1
  (import simple-sha1)      ; chicken-install simple-sha1

  ; Supporting Routines

  ; Construct new list of n items taken from head of ll
  (define (%list-take-head ll n)
    (if (and (> n 0) (pair? ll))
        (cons (car ll) (%list-take-head (cdr ll) (- n 1)))
        '()))

;;; ---------------------------------------------------------------------------
;;; Path Manipulation Routines
;;;   Module defines a base 'path' that allows simple modification operations.
;;; ---------------------------------------------------------------------------
  (define (path* . components) `(path . ,components))

  ; Represent an error in processing path information
  (define (path-error msg)
    (condition `(path-error msg ,msg)))

  (define (throw-path-error msg)
    (abort (path-error msg)))

  ; Path component representing relative entry (.)
  (define path-component-relative-to ".")
  (define (path-component-relative-to? n) (eq? n path-component-relative-to))

  ; Path component representing relative to parent
  (define path-component-parent-to "..")
  (define (path-component-parent-to? n) (eq? n path-component-parent-to))

  ; Path component representing absolute path follows (..)
  (define path-component-anchor 'fs://)
  (define (path-component-anchor? n) (eq? n path-component-anchor))

  ; Path component representing for representing relative directory
  (define path-component-relative-anchor /)
  (define (path-component-relative-anchor? n)
    (or (eq? n path-component-relative-anchor) (eq? n '/)))

  ; Path separator
  (define path-sep "/")

  ; List of all path predicates
  (define path-component-predicates
    (list string? symbol?
          path-component-relative-to?
          path-component-anchor?))

  ; Determine if symbol is a valid path component
  (define (path-component? c)
    (any (lambda (n) (n c)) path-component-predicates))

  ; Get path components list
  (define (%path.components p)
    (cdr p))

  ; Get head of path (first directory)
  (define (path.head* p)
    (if (path? p)
        (cadr p)
        (throw-path-error "Attempted to get path head of non-path")))

  ; Get all elements after head of path
  (define (path.after_head* p)
    (if (path? p)
        (let ((comp (%path.components p)))
          (if (pair? comp)
              (cdr comp)
              '()))))

  ; Type verification
  (define (path? p)
    (and (pair? p) (eq? (car p) 'path)))

  ; Construct path with appended component
  ;   base is checked to verify proper path
  ;   component should be validated or path-error
  (define (%path-append-raw base component)
    (cond ((path-component-anchor? component) (throw-path-error "Anchor path attempted after beginning of relative or anchored path"))
          ((path-component-relative-anchor? component) base)
          ((path? component) (%path-join-raw base (%path.components component)))
          ((list? component) (%path-join-raw base component))
          ((path? base)
            (cons 'path (append (%path.components base) (list component))))
          (#t (throw-path-error "Attempted to append path to non-path"))))

  ; Construct new path with added components
  (define (%path-join-raw path components)
    (if (pair? components)
        (%path-join-raw (%path-append-raw path (car components)) (cdr components))
        (if (list? components)
            path
            (throw-path-error "Expected path component list"))))

  ; Creators
  (define (path-component n)
    (if (path-component? n)
        (path* n)
        (throw-path-error "Bad path component")))

  ; Convert list head to path
  (define (%list->path.head head)
    (cond
      ((path? head) head)
      ((path-component-relative-anchor? head) (path* path-component-anchor))
      ((pair? head) (list->path head))
      (#t (path-component head))))

  ; Convert list to path
  (define (list->path components)
    (if (pair? components)
        (%path-join-raw (%list->path.head (car components)) (cdr components))
        (path* path-component-relative-to)))

  ; Convert path to string
  (define (%path.component->str comp)
    (cond ((path-component-anchor? comp) "/")
          ((symbol? comp) (symbol->string comp))
          ((string? comp) comp)))

  (define (%path.components->str comps)
    (let ((head (car comps))
          (tail (cdr comps)))
      (if (pair? tail)
          (string-append (%path.component->str head)
                         path-sep
                         (%path.components->str tail))
          (%path.component->str head))))

  (define (path->str p)
    (cond
      ((path? p)
        (let ((head (path.head* p))
              (tail (path.after_head* p)))
          (if (path-component-anchor? head)
              (string-append path-sep (%path.components->str tail))
              (%path.components->str (%path.components p)))))
      (#t (path->str (path p)))))

  ; Create path from arbitrary thing(s)
  (define (path . components)
    (list->path components))

  ; Test for single component path
  (define (%path-is-single? p)
    (and (path? p)
         (eq? (cdr (%path.components p)) '())))

  ; Test for root file system anchor path
  (define (path-is-root? p)
    (and (%path-is-single? p)
         (path-component-anchor? (car (%path.components p)))))

  ; Test for '.' relative
  (define (path-is-relative-root? p)
    (and (%path-is-single? p)
         (path-component-relative-to? (car (%path.components p)))))

  ; Check if path exists
  (define (path-exists? p)
    (file-exists? (path->str (path p))))

  ; Search for file in list of paths
  (define (path-search plist p)
    (if (pair? plist)
        (if (path-exists? (path (car plist) p))
            (path (car plist) p)
            (path-search (cdr plist) p))
        #f))

  ; Get parent of path
  (define (path-parent p)
    (cond
      ((path-is-root? p) p)
      ((path-is-relative-root? p) p)
      ((%path-is-single? p) (path path-component-relative-to))
      (#t (let ((original (%path.components p)))
            (list->path (%list-take-head original (- (length original) 1)))))
      ))

  ; Create Path
  (define (path-mkdir! p #!optional (parents? #t))
    (let ((clean-path (path p)))
      (create-directory (path->str clean-path) parents?)))

  ; Write text file at path
  (define (path-write-text! p text)
    (let ((outfile (open-output-file (path->str (path p)) #:text)))
      (begin
        (display text outfile)
        (close-output-port outfile))))


) ; End Module

