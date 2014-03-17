;; -*- lexical-binding: t; eval: (font-lock-add-keywords nil '(("defexamples\\| => " (0 'font-lock-keyword-face)))); -*-

(require 'dash-pt)

(def-example-group "List to list" nil
  (defexamples -pl-make
    (-pl-make (1 2) (3 4) (5 6)) => '(1 2 3 4 5 6)
    (-pl-make (1 (1+ 1)) (3 (* 2 2)) (5 6)) => '(1 2 3 4 5 6)
    )

  (defexamples -pl-size
    (-pl-size nil) => 0
    (-pl-size '(a b c d)) => 2
    )

  (defexamples -pl-null-p
    (-pl-null-p nil) => t
    (-pl-null-p '(a b c d)) => nil
    )

  (defexamples -pl-lookup-by
    (-pl-lookup-by nil 'eq 'a) => nil
    (-pl-lookup-by nil 'eq nil) => nil
    (-pl-lookup-by '(nil b c d) 'eq nil) => '(nil . b)
    (-pl-lookup-by '(nil b c d) 'equal nil) => '(nil . b)
    (-pl-lookup-by '(nil nil c d) 'eq nil) => '(nil . nil)
    (-pl-lookup-by '(a b c d) 'eq 'a) => '(a . b)
    (-pl-lookup-by '(a b c d) 'eq (make-symbol "a")) => nil
    (-pl-lookup-by '(a b c d) 'equal 'a) => '(a . b)
    (-pl-lookup-by '(a b c d) 'equal (make-symbol "a")) => nil
    (-pl-lookup-by '(a b c d) (-on 'equal 'symbol-name) (make-symbol "a")) => '(a . b)
    (-pl-lookup-by '(a b c nil) 'equal 'c) => '(c . nil)
    (-pl-lookup-by '(a b c nil) 'eq 'e) => nil
    )

  (defexamples -pl-lookup
    (-pl-lookup nil 'a) => nil
    (-pl-lookup nil nil) => nil
    (-pl-lookup '(nil b c d) nil) => '(nil . b)
    (-pl-lookup '(nil nil c d) nil) => '(nil . nil)
    (-pl-lookup '(a b c d) 'a) => '(a . b)
    (-pl-lookup '(a b c nil) 'c) => '(c . nil)
    (-pl-lookup '(a b c nil) 'e) => nil
    )

  (defexamples -pl-member-by
    (-pl-member-by nil 'eq 'a) => nil
    (-pl-member-by nil 'string= 'a) => nil
    (-pl-member-by nil 'string= nil) => nil
    (-pl-member-by '(nil b c d) 'eq nil) => t
    (-pl-member-by '(nil b c d) 'equal nil) => t
    (-pl-member-by '(a b c d) 'eq 'a) => t
    (-pl-member-by '(a b c nil) 'eq 'c) => t
    (-pl-member-by '(a b c nil) 'eq 'e) => nil
    (-pl-member-by '(a b c d) 'eq (make-symbol "a")) => nil
    (-pl-member-by '("a" 1 "b" 2) 'eq "a") => nil
    (-pl-member-by '("a" 1 "b" 2) 'string= "a") => t

    ;; (--pl-member-by (eq new old) nil 'a) => nil
    ;; (--pl-member-by (string= new old) nil 'a) => nil
    ;; (--pl-member-by (string= new old) nil nil) => nil
    ;; (--pl-member-by (eq new old) '(nil b c d) nil) => t
    ;; (--pl-member-by (equal new old) '(nil b c d) nil) => t
    ;; (--pl-member-by (eq new old) '(a b c d) 'a) => t
    ;; (--pl-member-by (eq new old) '(a b c nil) 'c) => t
    ;; (--pl-member-by (eq new old) '(a b c nil) 'e) => nil
    ;; (--pl-member-by (eq new old) '(a b c d) (make-symbol "a")) => nil
    ;; (--pl-member-by (eq new old) '("a" 1 "b" 2) "a") => nil
    ;; (--pl-member-by (string= new old) '("a" 1 "b" 2) "a") => t
    )

  (defexamples -pl-member
    (-pl-member nil 'a) => nil
    (-pl-member nil nil) => nil
    (-pl-member '(nil b c d) nil) => t
    (-pl-member '(a b c d) 'a) => t
    (-pl-member '(a b c nil) 'c) => t
    (-pl-member '(a b c nil) 'e) => nil
    (-pl-member '(a b c d) (make-symbol "a")) => nil
    (-pl-member '("a" 1 "b" 2) "a") => t
    )

  (defexamples -pl-get-by
    )

  (defexamples -pl-get
    (-pl-get nil 'a) => nil
    (-pl-get '(a b c d) 'a) => 'b
    (-pl-get '(a b c nil) 'c) => nil
    (-pl-get '(a b c nil) 'e) => nil
    )

  (defexamples -pl-insert-withkey-by
    (-pl-insert-withkey-by nil (lambda (k v old) (+ k v old)) 'equal 3 1) => '(1 3)

    (-pl-insert-withkey-by '(1 2 3 4 5 6) (lambda (k v old) (+ k v old)) 'equal 3 1) => '(1 6 3 4 5 6)
    (-pl-insert-withkey-by '(1 2 3 4 5 6) (lambda (k v old) (+ k v old)) 'equal 3 3) => '(1 2 3 10 5 6)
    (-pl-insert-withkey-by '(1 2 3 4 5 6) (lambda (k v old) (+ k v old)) 'equal 3 5) => '(1 2 3 4 5 14)
    (-pl-insert-withkey-by '(1 2 3 4 5 6) (lambda (k v old) (+ k v old)) 'equal 3 7) => '(1 2 3 4 5 6 7 3)
    (let ((r '(1 2 3 4 5 6))) (-pl-insert-withkey-by r (lambda (k v old) (+ k v old)) 'equal 3 3) r) => '(1 2 3 4 5 6)
    )

  (defexamples -pl-insert-with-by
    (-pl-insert-with-by '(1 2 3 4 5 6) (lambda (v old) (+ v old)) 'equal 3 1) => '(1 5 3 4 5 6)
    (-pl-insert-with-by '(1 2 3 4 5 6) (lambda (v old) (+ v old)) 'equal 3 3) => '(1 2 3 7 5 6)
    (-pl-insert-with-by '(1 2 3 4 5 6) (lambda (v old) (+ v old)) 'equal 3 5) => '(1 2 3 4 5 9)
    )

  (defexamples -pl-insert-by
    )

  (defexamples -pl-insert-withkey
    )

  (defexamples -pl-insert-with
    (let (r) (-pl-insert-with r '+ 2 1)) => '(1 2)
    (let ((r '(1 2 3 4))) (-pl-insert-with r '+ 5 3)) => '(1 2 3 9)
    (let ((r '(1 2 3 4))) (-pl-insert-with r '+ 6 5)) => '(1 2 3 4 5 6)
    (let ((r '(1 2 3 4))) (-pl-insert-with r '+ 5 3) r) => '(1 2 3 4)
    (let ((r '(1 2 3 4))) (-pl-insert-with r '+ 6 5) r) => '(1 2 3 4)
    )

  (defexamples -pl-insert
    (let (r) (-pl-insert r 2 1)) => '(1 2)

    (let ((r '(1 2 3 4))) (-pl-insert r 5 3)) => '(1 2 3 5)
    (let ((r '(1 2 3 4))) (-pl-insert r 6 5)) => '(1 2 3 4 5 6)
    (let ((r '(1 2 3 4))) (-pl-insert r 5 3) r) => '(1 2 3 4)
    (let ((r '(1 2 3 4))) (-pl-insert r 6 5) r) => '(1 2 3 4)

    (-pl-insert '(1 2 3 4) "a" 3 :nested) => '(1 2 3 (:nested "a"))
    (-pl-insert '(1 2 3 (:nested "a")) "b" 3 :another) => '(1 2 3 (:nested "a" :another "b"))
    (-pl-insert '(1 2 3 (:nested "a" :another "b")) "c" 3 :nested) => '(1 2 3 (:nested "c" :another "b"))
    (-pl-insert '(1 2 3 (:nested "c" :another "b")) "foo" 1) => '(1 "foo" 3 (:nested "c" :another "b"))
    (-pl-insert '(1 2 3 4) "a" 3 :nested :even :more) => '(1 2 3 (:nested (:even (:more "a"))))

    )

  (defexamples -pl-update-withkey-by
    )

  (defexamples -pl-update-by
    )

  (defexamples -pl-update-withkey
    )

  (defexamples -pl-update
    )

  (defexamples -pl-delete-by
    )

  (defexamples -pl-delete
    )

  (defexamples -pl-adjust-withkey-by
    )

  (defexamples -pl-adjust-by
    )

  (defexamples -pl-adjust-withkey
    )

  (defexamples -pl-adjust
    (let (r) (-pl-adjust r 2 1)) => nil
    (-pl-adjust '(1 2 3 4) 6 5) => '(1 2 3 4)

    (let ((r '(1 2 3 4))) (-pl-adjust r '1+ 3)) => '(1 2 3 5)
    (let ((r '(1 2 3 4))) (-pl-adjust r '1+ 5)) => '(1 2 3 4)
    (let ((r '(1 2 3 4))) (-pl-adjust r '1+ 3) r) => '(1 2 3 4)
    (let ((r '(1 2 3 4))) (-pl-adjust r '1+ 5) r) => '(1 2 3 4)

    (-pl-adjust '(1 2 3 4) '1+ 3 :nested) => '(1 2 3 4)
    (-pl-adjust '(1 2 3 (:nested 4)) '1+ 3 :nested) => '(1 2 3 (:nested 5))
    (-pl-adjust
     '(1 2 3 (:nested "a" :another "b"))
     (lambda (v) (concat "new:" v)) 3 :nested)
    => '(1 2 3 (:nested "new:a" :another "b"))
    (-pl-adjust
     '(1 2 3 (:nested "a" :another "b"))
     (lambda (v) (concat "new:" v)) 3 :another)
    => '(1 2 3 (:nested "a" :another "new:b"))
    (-pl-adjust
     '(1 2 3 (:nested "a" :another "b"))
     (lambda (v) (concat "new:" v)) 3 :nonexistant)
    => '(1 2 3 (:nested "a" :another "b"))
    (-pl-adjust '(1 2 3 (:nested "c" :another "b")) (-const "foo") 1) => '(1 "foo" 3 (:nested "c" :another "b"))
    (-pl-adjust '(1 (:nested (:even (:more 1)) "a" "b") 3 4) '1- 1 :nested :even :more)
    => '(1 (:nested (:even (:more 0)) "a" "b") 3 4)
    ))
