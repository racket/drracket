(let ([gen:477:join-env (void)]
      [gen:473:extend-env* (void)]
      [gen:476:extend-env (void)]
      [gen:474:lookup (void)]
      [gen:475:empty-env (void)])
  (begin (set! gen:475:empty-env '())
         (begin (set! gen:474:lookup
                  (lambda (gen:493:env gen:492:x)
                    (let ([gen:494:r (assq gen:492:x gen:493:env)])
                      (if (eq? gen:494:r '#f)
                          (error 'lookup '"no binding for ~a" gen:492:x)
                          (cdr gen:494:r)))))
                (begin (set! gen:476:extend-env
                         (lambda (gen:491:env gen:489:x gen:490:v)
                           (cons (cons gen:489:x gen:490:v) gen:491:env)))
                       (begin (set! gen:473:extend-env*
                                (lambda (gen:488:env gen:486:xs gen:487:vs)
                                  (append (map2 cons gen:486:xs gen:487:vs) gen:488:env)))
                              (begin (set! gen:477:join-env
                                       (lambda (gen:485:env gen:484:newenv)
                                         (append gen:484:newenv gen:485:env)))
                                     (make-rs:module
                                       (lambda (gen:478:msg)
                                         (let ([gen:479:%%tmp gen:478:msg])
                                           (if (memv gen:479:%%tmp '(empty-env))
                                               gen:475:empty-env
                                               (let ([gen:480:%%tmp gen:478:msg])
                                                 (if (memv gen:480:%%tmp '(lookup))
                                                     gen:474:lookup
                                                     (let ([gen:481:%%tmp gen:478:msg])
                                                       (if (memv gen:481:%%tmp '(extend-env))
                                                           gen:476:extend-env
                                                           (let ([gen:482:%%tmp gen:478:msg])
                                                             (if (memv gen:482:%%tmp '(extend-env*))
                                                                 gen:473:extend-env*
                                                                 (let ([gen:483:%%tmp gen:478:msg])
                                                                   (if (memv gen:483:%%tmp '(join-env))
                                                                       gen:477:join-env
                                                                       (error 'import
                                                                         '"attempting to import ~s~n        from module with signature ~s"
                                                                         gen:478:msg
                                                                         '(empty-env
                                                                            lookup
                                                                            extend-env
                                                                            extend-env*
                                                                            join-env))))))))))))))))))))
