#lang racket/gui

(require drracket/tool
         drracket/tool-lib
         racket/class
         racket/gui/base
         racket/unit
         framework
         framework/gui-utils
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

        (define git-auth-dialog
          (instantiate dialog% ("Git Authentication")))
        (define git-auth-user-field
          (new text-field%
               [parent git-auth-dialog]
               [label "Username: "]
               [min-width 100]))
        (define git-auth-password-field
          (new text-field%
               [parent git-auth-dialog]
               [label "Password: "]
               [min-width 100]
               [style (list 'single 'password)]))
        (gui-utils:ok/cancel-buttons
         git-auth-dialog
         (λ (b e) (send git-auth-dialog show #f))
         (λ (b e) (send git-auth-dialog show #f)))
        (define (cred_cb out url username allowed payload)
          (begin
            (send git-auth-dialog show #t)
            (ptr-set! out
                      _cred
                      (git_cred_userpass_plaintext_new
                       (send git-auth-user-field get-value)
                       (send git-auth-password-field get-value)
                       (send git-auth-user-field set-value "")
                       (send git-auth-password-field set-value "")))
            0))

        
        (define git-clone-dialog (instantiate dialog% ("Git Clone")))
        (define git-clone-url-field
          (new text-field%
               [parent git-clone-dialog]
               [label "URL: "]
               [min-width 300]
               [style (list 'single)]))
        (define git-clone-dir-field
          (new text-field%
               [parent git-clone-dialog]
               [label "Dir: "]
               [min-width 300]
               [style (list 'single)]))
        (gui-utils:ok/cancel-buttons
         (new horizontal-panel% [parent git-clone-dialog])
         (λ (b e) ; confirm
           (begin
             (let ([clone_opts (cast (malloc _git_clone_opts) _pointer _git_clone_opts-pointer)])
               (git_clone_init_options clone_opts 1)
               (set-git_remote_callbacks-credentials!
                (git_fetch_opts-callbacks
                 (git_clone_opts-fetch_opts clone_opts))
                cred_cb)
               (git_clone (send git-clone-url-field get-value)
                          (send git-clone-dir-field get-value)
                          clone_opts))
             (send git-clone-url-field set-value "")
             (send git-clone-dir-field set-value "")
             (send git-clone-dialog show #f)))
         (λ (b e) ;cancel
           (begin (send git-clone-url-field set-value "")
                  (send git-clone-dir-field set-value "")
                  (send git-clone-dialog show #f))))
        (define git-clone-menu
          (new menu-item%
               (label "Clone")
               (parent git-menu)
               (callback (λ (menu event)
                           (send git-clone-dialog show #t)))))

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
                           (with-handlers
                               ([exn:fail? (λ (exn)
                                             (git_commit_create_v
                                              (cast (malloc _git_oid) _pointer _oid)
                                              repo
                                              "HEAD"
                                              signature
                                              signature
                                              #f
                                              (send git-commit-message-field get-value)
                                              (git_tree_lookup repo tree_oid)
                                              0))])
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
                              (git_commit_lookup repo head_oid)))
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

        (define remote-list
          (λ (repo)
            (let ([strarr (make-strarray)])
              (git_remote_list strarr repo)
              (map
               (λ (i)
                 (ptr-ref (git_strarray-strings strarr) _string i))
               (range (git_strarray-count strarr))))))
        
        
        (define git-push
          (λ (repo remote)
            (let ([push_opts (cast (malloc _git_push_opts) _pointer _git_push_opts-pointer)])
              (git_push_init_options push_opts 1)
              (set-git_remote_callbacks-credentials!
               (git_push_opts-callbacks push_opts)
               cred_cb)
              (let ([ref (git_reference_name (git_repository_head repo))])
                (git_remote_push
                 (git_remote_lookup repo remote)
                 (make-strarray (string-append ref ":" ref))
                 push_opts)))))
        (define git-push-dialog (instantiate dialog% ("Git Push")))
        (define git-push-remote
          (new choice%
               [label "Remote: "]
               [choices (list)]
               [parent git-push-dialog]
               [callback (λ (c e)
                           (git-push (git_repository_open
                                      (path->string
                                       (send (get-current-tab) get-directory)))
                                     (send c get-string-selection)))]))
        (define git-push-menu
          (new menu-item%
               (label "Push")
               (parent git-menu)
               (callback (λ (menu event)
                           (let* ([repo (git_repository_open
                                         (path->string
                                          (send (get-current-tab) get-directory)))]
                                  [remotes (remote-list repo)])
                             (if (eq? 1 (length remotes))
                                 (git-push repo (first remotes))
                                 (begin
                                   (map
                                    (λ (remote)
                                      (send git-push-remote append remote))
                                    remotes)
                                   (send git-push-dialog show #t))))))))
        (define git-pull-menu
          (new menu-item%
               (label "Pull")
               (parent git-menu)
               (callback (λ (menu event)
                           (message-box "git pull")))))))
    
    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame git-menu-mixin)))
