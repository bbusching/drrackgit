#lang racket/gui

(require drracket/tool
         drracket/tool-lib
         racket/class
         racket/gui/base
         racket/unit
         framework
         ffi/unsafe
         (rename-in libgit2
                    (object? git_object?)))

(provide tool@)

(define relative-path
  (λ (p1 p2)
    (substring p2 (string-length p1))))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define git-menu-mixin
      (mixin (frame:standard-menus<%>
              drracket:unit:frame<%>) ()
        (define unit-frame (super-new))
        (inherit get-menu-bar
                 get-current-tab
                 get-tabs)
        
        (define git-menu
          (new menu%
             (label "&Git")
             (parent (get-menu-bar))))

        (define git-clone-menu
          (new menu-item%
               (label "Clone")
               (parent git-menu)
               (callback (λ (menu event)
                           (message-box "git clone")))))

        (new separator-menu-item% (parent git-menu))

        (define git-add-menu
          (new menu%
               (label "Add")
               (parent git-menu)))
        
        (define git-add-cur-tab-menu
          (new menu-item%
               (label "Add Current Tab")
               (parent git-add-menu)
               (callback (λ (menu event)
                           (map
                            (λ (tab-pair)
                              (let* ([dir (path->string (send (car tab-pair) get-directory))]
                                     [repo (git_repository_open dir)]
                                     [index (git_repository_index repo)])
                                (git_index_add_all
                                 index
                                 (make-strarray (relative-path (git_repository_workdir repo)
                                                               (string-append dir (cdr tab-pair))))
                                 'GIT_INDEX_ADD_DEFAULT
                                 #f
                                 #f)
                                (git_index_write index)))
                            (filter
                             (λ (tab-pair) (send (car tab-pair) is-current-tab?))
                             (map cons
                                  (get-tabs)
                                  (map (λ (i) (send this get-tab-filename i))
                                       (range (send this get-tab-count))))))))))
        (define git-add-all-tabs-menu
          (new menu-item%
               (label "Add All Tabs")
               (parent git-add-menu)
               (callback (λ (menu event)
                           (map
                            (λ (tab-pair)
                              (let* ([dir (path->string (send (car tab-pair) get-directory))]
                                     [repo (git_repository_open dir)]
                                     [index (git_repository_index repo)])
                                (git_index_add_all
                                 (git_repository_index repo)
                                 (make-strarray (relative-path (git_repository_workdir repo)
                                                               (string-append dir (cdr tab-pair))))
                                 'GIT_INDEX_ADD_DEFAULT
                                 #f
                                 #f)
                                (git_index_write index)))
                            (map cons
                                 (get-tabs)
                                 (map (λ (i) (send this get-tab-filename i))
                                      (range (send this get-tab-count)))))))))


        (define git-commit-dialog (instantiate dialog% ("Git Commit")))
        (define git-commit-message-field
          (new text-field%
               [parent git-commit-dialog]
               [label "Message: "]
               [min-width 600]
               [min-height 300]
               [font (make-object font% 10 'modern)]
               [style (list 'multiple)]))
        (define panel (new horizontal-panel% [parent git-commit-dialog]
                           [alignment '(center center)]))
        (new button%
             [parent panel]
             [label "Cancel"]
             [callback (λ (b e)
                         (begin (send git-commit-message-field set-value "")
                                (send git-commit-dialog show #f)))])
        (new button%
             [parent panel]
             [label "Ok"]
             [callback (λ (b e)
                         (let* ([repo (git_repository_open (path->string (send (get-current-tab) get-directory)))]
                                [signature (git_signature_default repo)]
                                [head_oid (cast (malloc _git_oid) _pointer _oid)]
                                [tree_oid (cast (malloc _git_oid) _pointer _oid)])
                           (git_index_write_tree tree_oid (git_repository_index repo))
                           (git_reference_name_to_id head_oid repo "HEAD")
                           (git_commit_create_v
                            (cast (malloc _git_oid) _pointer _oid)
                            repo
                            "HEAD"
                            signature
                            signature
                            #f
                            (send git-commit-message-field get-value)
                            (git_tree_lookup repo tree_oid)
                            1
                            (git_commit_lookup repo head_oid))
                           (message-box "done")
                           (send git-commit-message-field set-value "")
                           (send git-commit-dialog show #f)))])
        (when (system-position-ok-before-cancel?)
          (send panel change-children reverse))
        (define git-commit-menu
          (new menu-item%
               (label "Commit")
               (parent git-menu)
               (callback (λ (menu event)
                           (send git-commit-dialog show #t)))))
        
        (new separator-menu-item% (parent git-menu))

        (define git-push-menu
          (new menu-item%
               (label "Push")
               (parent git-menu)
               (callback (λ (menu event)
                           (message-box "git push")))))
        (define git-pull-menu
          (new menu-item%
               (label "Pull")
               (parent git-menu)
               (callback (λ (menu event)
                           (message-box "git pull")))))))
    
    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame git-menu-mixin)))
