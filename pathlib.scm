;;; Path Utilities for Scheme
(module pathlib (path-error? path-error
                 path-exists?
                 path-relative-to
                 path-anchor
                 path-search
                 path-sep
                 path?
                 path-component?
                 path-mkdir!
                 path-parent
                 path-parent-to
                 path-parent-to?
                 path-is-root?
                 path-is-relative-root?
                 path-write-text!
                 path
                 list->path
                 path->str)
  (import scheme)

  (import (chicken file))   ; installed w/ chicken
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
  (define path.sym 'path)
  (define (path* . components) `(,path.sym . ,components))
  ; Represent an error in processing path information
  (define path-error* 'path-error)
  (define (path-error msg) (list path-error* msg)) 
  (define (path-error? v)
    (and (pair? v) (eq? (car v) path-error*)))

  ; Path component representing relative entry (.)
  (define path-relative-to ".")
  (define (path-relative-to? n) (eq? n path-relative-to))

  ; Path component representing relative to parent
  (define path-parent-to "..")
  (define (path-parent-to? n) (eq? n path-parent-to))

  ; Path component representing absolute path follows (..)
  (define path-anchor 'fs://)
  (define (path-anchor? n) (eq? n path-anchor))

  ; Path component representing for representing relative directory
  (define path-relative-anchor /)
  (define (path-relative-anchor? n) (or (eq? n path-relative-anchor) (eq? n '/)))

  ; Path separator
  (define path-sep "/")

  ; List of all path predicates
  (define path-component-predicates
    (list string? symbol? path-relative-to? path-anchor?))

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
        (path-error "Attempted to get path head of non-path")))

  ; Get all elements after head of path
  (define (path.after_head* p)
    (if (path? p)
        (let ((comp (%path.components p)))
          (if (pair? comp)
              (cdr comp)
              '()))))

  ; Type verification
  (define (path? p)
    (and (pair? p) (eq? (car p) path.sym)))

  ; Construct path with appended component
  ;   base is checked to verify proper path
  ;   component should be validated or path-error
  (define (%path-append-raw base component)
    (cond ((path-error? base) base)
          ((path-error? component) component)
          ((path-anchor? component) (path-error "Anchor path attempted after beginning of relative or anchored path"))
          ((path-relative-anchor? component) base)
          ((path? component) (%path-join-raw base (%path.components component)))
          ((list? component) (%path-join-raw base component))
          ((path? base)
            (cons path.sym (append (%path.components base) (list component))))
          (#t (path-error "Attempted to append path to non-path"))))

  ; Construct new path with added components
  (define (%path-join-raw path components)
    (if (pair? components)
        (%path-join-raw (%path-append-raw path (car components)) (cdr components))
        (if (list? components)
            path
            (path-error "Expected path component list"))))

  ; Creators
  (define (path-component n)
    (if (path-component? n)
        (path* n)
        (path-error "Bad path component")))

  ; Convert list head to path
  (define (%list->path.head head)
    (cond
      ((path? head) head)
      ((path-error? head) head)
      ((path-relative-anchor? head) (path* path-anchor))
      ((pair? head) (list->path head))
      (#t (path-component head))))

  ; Convert list to path
  (define (list->path components)
    (if (pair? components)
        (%path-join-raw (%list->path.head (car components)) (cdr components))
        (path* path-relative-to)))

  ; Convert path to string
  (define (%path.component->str comp)
    (cond ((path-anchor? comp) "/")
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
          (if (path-anchor? head)
              (string-append path-sep (%path.components->str tail))
              (%path.components->str (%path.components p)))))
      ((path-error? p) p)
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
         (path-anchor? (car (%path.components p)))))

  ; Test for '.' relative
  (define (path-is-relative-root? p)
    (and (%path-is-single? p)
         (path-relative-to? (car (%path.components p)))))


  ; Check if path exists
  (define (path-exists? p)
    (file-exists? (path->str (path p))))

  ; Search for file in list of paths
  (define (path-search plist p)
    (if (pair? plist)
        (if (path-exists? (path (car plist) p))
            (path (car plist) p)
            (path-search (cdr plist) p))
        (path-error "Not found")))

  ; Get parent of path
  (define (path-parent p)
    (cond
      ((path-is-root? p) p)
      ((path-is-relative-root? p) p)
      ((%path-is-single? p) (path path-relative-to))
      (#t (let ((original (%path.components p)))
            (list->path (%list-take-head original (- (length original) 1)))))
      ))

  ; Create Path
  (define (path-mkdir! p #!optional (parents? #t))
    (let ((clean-path (path p)))
      (if (path? clean-path)
          (create-directory (path->str clean-path) parents?)
          (path-error "Invalid path requested"))))

  ; Write text file at path
  (define (path-write-text! p text)
    (let ((outfile (open-output-file (path->str (path p)) #:text)))
      (begin
        (display text outfile)
        (close-output-port outfile))))


) ; End Module

