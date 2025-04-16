#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/list)

;; In-memory storage of entries
(define entries (make-parameter '()))

;; Helper to get form values safely
(define (get-field field-name bindings)
  (define found
    (findf (lambda (b)
             (and (binding:form? b)
                  (equal? (bytes->string/utf-8 (binding-id b))
                          (symbol->string field-name))))
           bindings))
  (if found
      (bytes->string/utf-8 (binding:form-value found))
      ""))

;; Main handler
(define (start req)
  (define bindings (request-bindings/raw req))
  (define name (get-field 'name bindings))
  (define email (get-field 'email bindings))
  (define phone (get-field 'phone bindings))
  (define action (get-field 'action bindings))

  ;; Process form actions
  (cond
    [(string=? action "save")
     (when (and (not (string=? name ""))
                (not (string=? email ""))
                (not (string=? phone "")))
       (entries (cons (list name email phone) (entries))))]
    [(string=? action "clear") (void)]
    [else (void)])

  ;; Render response
  (response/xexpr
   `(html
     (head (title "People Info Form"))
     (body
      (h2 "Add a Person")
      (form ((action "/") (method "post"))
        (label "Name: ") (input ((type "text") (name "name"))) (br)
        (label "Email: ") (input ((type "text") (name "email"))) (br)
        (label "Phone: ") (input ((type "text") (name "phone"))) (br)
        (button ((type "submit") (name "action") (value "save")) "Save")
        (button ((type "submit") (name "action") (value "clear")) "Clear"))
      (hr)
      (h2 "Saved People")
      ,(if (null? (entries))
           `(p "No entries yet.")
           `(table ((border "1") (cellpadding "5"))
              (tr (th "Name") (th "Email") (th "Phone"))
              ,@(for/list ([entry (in-list (reverse (entries)))])
                  (match entry
                    [(list n e p) `(tr (td ,n) (td ,e) (td ,p))]))))))))
               
;; Launch the server
(serve/servlet start
               #:port 8000
               #:servlet-path "/"
               #:launch-browser? #t)
