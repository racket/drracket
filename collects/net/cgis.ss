(require-library "macro.ss")

(define-signature mzlib:cgi^
  (
    ;; -- exceptions raised --
    (struct cgi-error ())
    (struct incomplete-%-suffix (chars))
    (struct invalid-%-suffix (char))

    ;; -- cgi methods --
    get-bindings
    get-bindings/post
    get-bindings/get
    generate-html-output
    generate-error-output
    bindings-as-html
    extract-bindings
    extract-binding/single
    get-cgi-method
   
    ;; -- general HTML utilities --
    string->html
    generate-link-text
    ))
