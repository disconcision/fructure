#lang racket


#| 2018.06.15

 this is the transformation library used for the
 second attempt at the fructure structure editor.
 it is reproduced here to study as a source for
 abstractions to target in implementing fructerm.

|#


#; (main-loop
    
    ; amodal functions
    ['f1 (!do (λ (_) backup-stage))] ; restores stage to initial state
    ['f2 (pretty-print (project-symbol stage-gui))] ; print s-expr representing stage
    ['f3 (pretty-print stage-gui)] ; print raw stage data

    (SELECT     
     ['home                  [,a
                              ⋱↦ (▹ ,a)] [(▹ ,a) ⋱↦ ,a]]                         
     [#\return               ([((▹ ,(? form-name? a)) ,x ...)
                               ⋱↦ (c▹ (c▹▹ ,empty-symbol) (,a ,@x))]
                              [(▹ ,a)
                               ⋱↦ (c▹ (c▹▹ ,empty-symbol) ,a)]) 
                             (mode: TRANSFORM)]
                           
     ['right                 (▹-next-? atom?)]
     ['left                  (▹-prev-? atom?)]
     ['up                    [(,a ... (▹ ,b ...) ,c ...)
                              ⋱↦ (▹ (,@a ,@b ,@c))]]
     ['down                  [(▹ (,a ,b ...))
                              ⋱↦ ((▹ ,a) ,@b)]]
                           
     [#\space                simple-paint]
     ['escape                [(⋈ ,a ,b)
                              ⋱↦ ,b]]
                           
     [(reg "[A-Za-z_]")      [(▹ ,a)
                              ⋱↦ (s▹ ,(string key-code) ,((▹▹tag-hits (string key-code)) a))]
                             (mode: SEARCH)])
   
    (SEARCH    
     [(or 'escape #\return)  (compose [(s▹ ,buf ,sel)
                                       ⋱↦ (▹ ,sel)]
                                      [(▹▹ ,a)
                                       ⋱↦ ,a])
                             (mode: SELECT)]
                           
     ['right                 ▹-cycle-▹▹]
                           
     [(or 'left #\backspace) [(s▹ ,buf ,sel)
                              ⋱↦ ,(let ([bu (remove-last-char-str buf)])
                                    `(s▹ ,bu ,((▹▹tag-hits bu) sel)))]]
                           
     [(reg "[A-Za-z0-9_]")   [(s▹ ,buf ,sel)
                              ⋱↦ ,(let ([new ((append-char-to-str key-code) buf)])
                                    `(s▹ ,new ,((▹▹tag-hits new) sel)))]])

    (TRANSFORM
     ['escape                (compose [(c▹ ,buf ,sel)
                                       ⋱↦ (▹ ,sel)]
                                      [(⋈ ,num ,sel)
                                       ⋱↦ ,sel])
                             (mode: select)]
                           
     [#\return               [(c▹ ,buf ,sel)
                              ⋱↦ (▹ ,(([(c▹▹ ,x) ⋱↦ ,x]) (eval-painted-buffer buf sel)))]
                             (mode:select)]
                           
     [(or 'right #\space)    ([(,as ...  (c▹▹ ,(? empty-symbol?)))
                               ⋱↦ (,@as (c▹▹ ,empty-symbol))]
                              [(,as ...  (c▹▹ ,b))
                               ⋱↦ (,@as ,b  (c▹▹ ,empty-symbol))]
                              [(,(and as (not (== 'c▹))) ... (c▹▹ ,b) ,c ,cs ...)
                               ⋱↦ (,@as ,b  (c▹▹ ,c) ,@cs)]
                              [(c▹▹ ,(? empty-symbol?))
                               ⋱↦ (c▹▹ ,empty-symbol)]
                              [(c▹▹ ,a)
                               ⋱↦ (,a  (c▹▹ ,empty-symbol))])]
                           
     ['down                  [(c▹▹ ,a)
                              ⋱↦ ((c▹▹ ,a))]]
                           
     ['up                    ([(,as ... (,bs ... (c▹▹ ,(? empty-symbol?))))
                               ⋱↦ (,@as (,@bs) (c▹▹ ,empty-symbol))]
                              [(,as ... (,bs ... (c▹▹ ,c)))
                               ⋱↦ (,@as (,@bs ,c) (c▹▹ ,empty-symbol))])]
                           
     [(or 'left #\backspace) ([((c▹▹ ,(? empty-symbol?)) ,as ...)
                               ⋱↦ (c▹▹ ,empty-symbol)]
                              [(c▹▹ ,(? symbol? s))
                               ⋱↦  (c▹▹ ,(remove-last-char s))]
                              [(c▹▹ ,(? atom? s))
                               ⋱↦  (c▹▹ ,empty-symbol)]
                              [(c▹ (c▹▹ ,(? empty-symbol?)) ,xs ...)
                               ⋱↦ (c▹ (c▹▹ ,empty-symbol) ,@xs)]
                              [(,xs ... ,(? atom? x) (c▹▹ ,(? empty-symbol?)) ,ys ...)
                               ⋱↦  (,@xs (c▹▹ ,x) ,@ys)]
                              [(,xs ... (,as ...) (c▹▹ ,(? empty-symbol? s)) ,ys ...)
                               ⋱↦  (,@xs (,@as (c▹▹ ,empty-symbol)) ,@ys)])]
                           
     [(reg "[0-9]")          (named-paint-c▹▹ (string->number (string key-code)))]
     ['control               [(c▹ ,buf ,sel)
                              ⋱↦ (c▹ ,buf ,(toggle-paint sel))]]
     [#\tab                  replace-with-first-autocomplete-match]
     [(reg "[A-Za-z_]")      [(c▹▹ ,(? symbol? s))
                              ⋱↦ (c▹▹ ,((append-char-to key-code) s))]])

    (PROJECT))