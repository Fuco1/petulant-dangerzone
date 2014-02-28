;; -*- lexical-binding: t; eval: (font-lock-add-keywords nil '(("defexamples\\| => " (0 'font-lock-keyword-face)))); -*-

(require 'dash-pt)

(def-example-group "List to list" nil
  (defexamples -pl-size
    (-pl-size nil) => 0
    (-pl-size '(a b c d)) => 2
    )

  (defexamples -pl-null
    (-pl-null nil) => t

    (-pl-null '(a b c d)) => nil
    )

  (defexamples -pl-get
    (-pl-get nil 'a) => nil

    (-pl-get '(a b c d) 'a) => 'b
    (-pl-get '(a b c nil) 'c) => nil
    (-pl-get '(a b c nil) 'e) => nil
    )

  (defexamples -pl-member-by
    (-pl-member-by 'eq nil 'a) => nil
    (-pl-member-by 'string= nil 'a) => nil
    (-pl-member-by 'string= nil nil) => nil
    (-pl-member-by 'eq '(nil b c d) nil) => t
    (-pl-member-by 'equal '(nil b c d) nil) => t

    (-pl-member-by 'eq '(a b c d) 'a) => t
    (-pl-member-by 'eq '(a b c nil) 'c) => t
    (-pl-member-by 'eq '(a b c nil) 'e) => nil
    (-pl-member-by 'eq '(a b c d) (make-symbol "a")) => nil
    (-pl-member-by 'eq '("a" 1 "b" 2) "a") => nil
    (-pl-member-by 'string= '("a" 1 "b" 2) "a") => t

    (--pl-member-by (eq new old) nil 'a) => nil
    (--pl-member-by (string= new old) nil 'a) => nil
    (--pl-member-by (string= new old) nil nil) => nil
    (--pl-member-by (eq new old) '(nil b c d) nil) => t
    (--pl-member-by (equal new old) '(nil b c d) nil) => t


    (--pl-member-by (eq new old) '(a b c d) 'a) => t
    (--pl-member-by (eq new old) '(a b c nil) 'c) => t
    (--pl-member-by (eq new old) '(a b c nil) 'e) => nil
    (--pl-member-by (eq new old) '(a b c d) (make-symbol "a")) => nil
    (--pl-member-by (eq new old) '("a" 1 "b" 2) "a") => nil
    (--pl-member-by (string= new old) '("a" 1 "b" 2) "a") => t
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

  (defexamples -pl-lookup
    (-pl-lookup nil 'a) => nil
    (-pl-lookup nil nil) => nil
    (-pl-lookup '(nil b c d) nil) => '(nil . b)
    (-pl-lookup '(nil nil c d) nil) => '(nil . nil)

    (-pl-lookup '(a b c d) 'a) => '(a . b)
    (-pl-lookup '(a b c nil) 'c) => '(c . nil)
    (-pl-lookup '(a b c nil) 'e) => nil
    )

  (defexamples -pl-lookup-def
    (-pl-lookup-def 'x nil 'a) => '(a . x)
    (-pl-lookup-def 'x nil nil) => '(nil . x)
    (-pl-lookup-def 'x '(nil b c d) nil) => '(nil . b)
    (-pl-lookup-def 'x '(nil nil c d) nil) => '(nil . nil)

    (-pl-lookup-def 'x '(a b c d) 'a) => '(a . b)
    (-pl-lookup-def 'x '(a b c nil) 'c) => '(c . nil)
    (-pl-lookup-def 'x '(a b c nil) 'e) => '(e . x)
    )

  (defexamples -pl-insert-with
    (let (r) (-pl-insert-with '+ 1 2 r)) => '(1 2)

    (let ((r '(1 2 3 4))) (-pl-insert-with '+ 3 5 r)) => '(1 2 3 9)
    (let ((r '(1 2 3 4))) (-pl-insert-with '+ 5 6 r)) => '(1 2 3 4 5 6)
    (let ((r '(1 2 3 4))) (-pl-insert-with '+ 3 5 r) r) => '(1 2 3 4)
    (let ((r '(1 2 3 4))) (-pl-insert-with '+ 5 6 r) r) => '(1 2 3 4)
    )

  (defexamples -pl-insert
    (let (r) (-pl-insert 1 2 r)) => '(1 2)

    (let ((r '(1 2 3 4))) (-pl-insert 3 5 r)) => '(1 2 3 5)
    (let ((r '(1 2 3 4))) (-pl-insert 5 6 r)) => '(1 2 3 4 5 6)
    (let ((r '(1 2 3 4))) (-pl-insert 3 5 r) r) => '(1 2 3 4)
    (let ((r '(1 2 3 4))) (-pl-insert 5 6 r) r) => '(1 2 3 4)
    )
  )
