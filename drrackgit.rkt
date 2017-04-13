#lang racket/gui

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         framework)

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define git-menu-mixin
      (mixin (frame:standard-menus<%>) (frame:basic<%>)
        (super-new)
        (inherit get-menu-bar)
        (new menu%
             (label "Git")
             (parent (get-menu-bar)))))
    
    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame git-menu-mixin)))
