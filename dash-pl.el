;; -*- lexical-binding: t; -*-
(require 'dash)

;; posunut key na koniec, alebo ako vymysliet varianty kde bude nested structure.
;; (-pl-get :foo plist default)
;; (-pl-get '(:foo :bar) plist default)

(defun -pl-each-while (plist pred fun)
  "Call FUN for each element of PLIST until PRED is non-nil.

Both FUN and PRED are functions taking two arguments: key and
value."
  (-when-let* ((k (car plist))
               (v (cadr plist)))
    (while (and plist (funcall pred k v))
      (funcall fun k v)
      (setq plist (cddr plist))
      (setq k (car plist))
      (setq v (cadr plist)))))

(defun -pl-each (plist fun)
  "Call FUN for each element of PLIST.

FUN is function taking two arguments: key and value."
  (-pl-each-while plist (lambda (_ _) t) fun))

(defmacro -pl-make (&rest values)
  "Return a new plist containing key value pairs VALUES.

The elements in VALUES should have a form (KEY VALUE)

Type: [(k a)] -> Plist k a"
  `(list ,@(apply 'append values)))

(defun -pl-size (plist)
  "Return the number of elements in the PLIST.

Type: Plist k a -> Int"
  (/ (length plist) 2))

(defun -pl-null-p (plist)
  "Return non-nil if the PLIST is empty.

Type: Plist k a -> Bool"
  (not plist))

(defalias '-pl-null? '-pl-null-p)

(defun -pl-lookup-by (plist equiv key &rest keys)
  "Find the value at KEY in PLIST.

Return (k . a) if the key is found, or nil otherwise.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> k -> Bool) -> k -> Maybe (k . a)"
  (-pl-each-while
   plist
   (lambda (k _)
     (not (funcall equiv k key)))
   (lambda (_ _) (setq plist (cddr plist))))
  (when plist
    (if keys
        (apply '-pl-lookup-by (when (listp (cadr plist))
                                (cadr plist))
               equiv (car keys) (cdr keys))
      (cons (car plist) (cadr plist)))))

(defun -pl-lookup (plist key &rest keys)
  "Find the value at KEY in PLIST.

Return (k . a) if the key is found, or nil otherwise.

The keys are compared using `equal'.

Type: Plist k a -> k -> Maybe (k . a)"
  (apply '-pl-lookup-by plist 'equal key keys))

(defun -pl-member-by (plist equiv key &rest keys)
  "Return non-nil if PLIST contains KEY.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> k -> Bool) -> k -> Bool"
  (not (null (apply '-pl-lookup-by plist equiv key keys))))

(defun -pl-member (plist key &rest keys)
  "Return non-nil if PLIST contains KEY.

The keys are compared using `equal'.

Type: Plist k a -> k -> Bool"
  (not (null (apply '-pl-lookup plist key keys))))

(defun -pl-get-by (plist equiv key &rest keys)
  "Return the value in PLIST at KEY.

Return nil if no such element is found.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Warning: this function might return nil even if the key is
present in the situation when the value for this key is nil.  Use
`-pl-member-by' to test if the key is in the map or
`-pl-lookup-by' to retrive the (key . value) pair safely.

Type: Plist k a -> (k -> k -> Bool) -> k -> a"
  (cdr (apply '-pl-lookup-by plist equiv key keys)))

(defun -pl-get (plist key &rest keys)
  "Return the value in PLIST at KEY.

Return nil if no such element is found.

Keys are compared using `equal'.

Warning: this function might return nil even if the key is
present in the situation when the value for this key is nil.  Use
`-pl-member' to test if the key is in the map or `-pl-lookup' to
retrive the (key . value) pair safely.

Type: Plist k a -> k -> a"
  (apply '-pl-get-by plist 'equal key keys))

;; TODO we can unify this and update into one function
(defun -pl-insert-withkey-by (plist fun equiv value key &rest keys)
  "Insert with a function, combining key, new value and old value.

Insert the pair (KEY, VALUE) into MAP if key does not exist in
the map.  If the key does exist, insert the pair (KEY, (FUN KEY VALUE
old-value)).

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> a -> a -> a) -> (k -> k -> Bool) -> a -> k -> Plist k a"
  (let ((pl plist) r)
    (-pl-each-while
     plist
     (lambda (k _)
       (not (funcall equiv k key)))
     (lambda (k v)
       (push k r)
       (push v r)
       (setq pl (cddr pl))))
    (push key r)
    (if keys
        (push (apply '-pl-insert-withkey-by (when (listp (cadr pl)) (cadr pl))
                     fun equiv value (car keys) (cdr keys)) r)
      (if pl
          (push (funcall fun key value (cadr pl)) r)
        (push value r)))
    (--if-let (cddr pl) (-concat (nreverse r) it) (nreverse r))))

(defun -pl-insert-with-by (plist fun equiv value key &rest keys)
  "Insert with a function, combining new value and old value.

Insert the pair (KEY, VALUE) into MAP if key does not exist in
the map.  If the key does exist, insert the pair (KEY, (FUN VALUE
old-value)).

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (a -> a -> a) -> (k -> k -> Bool) -> a -> k -> Plist k a"
  (apply '-pl-insert-withkey-by plist
         (lambda (_ v old) (funcall fun v old))
         equiv value key keys))

(defun -pl-insert-by (plist equiv value key &rest keys)
  "Insert a new KEY and VALUE in the MAP.

If the key is already present in the map, the associated value is
replaced with the supplied value.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> k -> Bool) -> a -> k -> Plist k a"
  (apply '-pl-insert-withkey-by plist (lambda (_ v _) v) equiv value key keys))

(defun -pl-insert-withkey (plist fun value key &rest keys)
  "Insert with a function, combining key, new value and old value.

Insert the pair (KEY, VALUE) into MAP if key does not exist in
the map.  If the key does exist, insert the pair (KEY, (FUN KEY VALUE
old-value)).

The keys are compared by `equal'.

Type: Plist k a -> (a -> a -> a) -> a -> k -> Plist k a"
  (apply '-pl-insert-withkey-by plist fun 'equal value key keys))

(defun -pl-insert-with (plist fun value key &rest keys)
  "Insert with a function, combining new value and old value.

Insert the pair (KEY, VALUE) into MAP if key does not exist in
the map.  If the key does exist, insert the pair (KEY, (FUN VALUE
old-value)).

The keys are compared by `equal'.

Type: Plist k a -> (a -> a -> a) -> a -> k -> Plist k a"
  (apply '-pl-insert-withkey-by plist
         (lambda (_ v old) (funcall fun v old))
         'equal value key keys))

(defun -pl-insert (plist value key &rest keys)
  "Insert a new KEY and VALUE in the MAP.

If the key is already present in the map, the associated value is
replaced with the supplied value.

The keys are compared by `equal'.

Type: Map k a -> a -> k -> Map k a"
  (apply '-pl-insert-withkey-by plist (lambda (_ v _) v) 'equal value key keys))

(defun -pl-update-withkey-by (plist fun equiv key &rest keys)
  "Update value at KEY.

If (FUN KEY value) returns nil, the key-value pair is deleted
from the map.

Otherwise (FUN KEY value) should return a list and the value at
key will be set to `car' of this list.

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> a -> a) -> (k -> k -> Bool) -> k -> Plist k a"
  (let ((pl plist) r)
    (-pl-each-while
     plist
     (lambda (k _)
       (not (funcall equiv k key)))
     (lambda (k v)
       (push k r)
       (push v r)
       (setq pl (cddr pl))))
    (when pl
      (if keys
          (progn
            (push key r)
            (push (or (apply '-pl-update-withkey-by (when (listp (cadr pl)) (cadr pl))
                             fun equiv (car keys) (cdr keys))
                      (cadr pl)) r))
        (-when-let (new-val (funcall fun key (cadr pl)))
          (push key r)
          (push (car new-val) r))))
    (--if-let (cddr pl) (-concat (nreverse r) it) (nreverse r))))

(defun -pl-update-by (plist fun equiv key &rest keys)
  "Update value at KEY.

If (FUN value) returns nil, the key-value pair is deleted
from the map.

Otherwise (FUN value) should return a list and the value at
key will be set to `car' of this list.

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (a -> a) -> (k -> k -> Bool) -> k -> Plist k a"
  (apply '-pl-update-withkey-by plist
         (lambda (_ v) (funcall fun v))
         equiv key keys))

(defun -pl-update-withkey (plist fun key &rest keys)
  "Update value at KEY.

If (FUN KEY value) returns nil, the key-value pair is deleted
from the map.

Otherwise (FUN KEY value) should return a list and the value at
key will be set to `car' of this list.

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: Plist k a -> (k -> a -> a) -> k -> Plist k a"
  (apply '-pl-update-withkey-by plist
         (lambda (_ v) (funcall fun v))
         'equal key keys))

(defun -pl-update (plist fun key &rest keys)
  "Update value at KEY.

If (FUN value) returns nil, the key-value pair is deleted
from the map.

Otherwise (FUN value) should return a list and the value at
key will be set to `car' of this list.

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: Plist k a -> (a -> a) -> k -> Plist k a"
  (apply '-pl-update-withkey-by plist
         (lambda (_ v) (funcall fun v))
         'equal key keys))

(defun -pl-delete-by (plist equiv key &rest keys)
  "Delete KEY and its value from PLIST.

When KEY is not a member of PLIST, the original PLIST is
returned unmodified.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> k -> Bool) -> k -> Plist k a"
  (apply '-pl-update-withkey-by plist (-const nil) equiv key keys))

(defun -pl-delete (plist key &rest keys)
  "Delete KEY and its value from PLIST.

When KEY is not a member of PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: Plist k a -> k -> Plist k a"
  (apply '-pl-update-withkey-by plist (-const nil) 'equal key keys))

(defun -pl-adjust-withkey-by (plist fun equiv key &rest keys)
  "Update value at KEY with (FUN KEY value).

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (k -> a -> a) -> (k -> k -> Bool) -> k -> Plist k a"
  (apply '-pl-update-withkey-by plist
         (lambda (k v) (list (funcall fun k v)))
         equiv key keys))

(defun -pl-adjust-by (plist fun equiv key &rest keys)
  "Update value at KEY with (FUN value).

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared using EQUIV, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: Plist k a -> (a -> a) -> (k -> k -> Bool) -> k -> Plist k a"
  (apply '-pl-adjust-withkey-by plist (lambda (_ v) (funcall fun v)) equiv key keys))

(defun -pl-adjust-withkey (plist fun key &rest keys)
  "Update value at KEY with (FUN KEY value).

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: Plist k a -> (k -> a -> a) -> k -> Plist k a"
  (apply '-pl-adjust-withkey-by plist fun 'equal key keys))

(defun -pl-adjust (plist fun key &rest keys)
  "Update value at KEY with (FUN value).

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: Plist k a -> (a -> a) -> k -> Plist k a"
  (apply '-pl-adjust-withkey-by plist (lambda (_ v) (funcall fun v)) 'equal key keys))

;; TODO:
;; add:
;; [update]-with-key

;; - Combination
;; [[union
;;   unions
;;   difference
;;   intersection]-with]-key
;; merge-with-key

;; - Traversal
;; [map]-with-key
;; [map-accum]-with-key
;; [fold[r|l]]-with-key
;; min
;; max
;; delete-min
;; delete-max
;;   - these all should work on keys or values

;; - Conversions
;; elems
;; keys

;; - Assoc
;; to-assoc
;; from-assoc

;; - Sorting
;; sort-on-key
;; sort-on-value
;; sort-on-key-value

;; - Filtering
;; [filter]-with-key (by default on values)
;;   - also various partitions, splits etc. We should wait for dash 3.0
;;     to make the naming consistent

;; - Predicates
;; [[is-subplist]-by]-on < not good name
;;   - we need to be able to test only on keys, or on both keys and
;;     values, so one or two comparators can be supplied

;; - Indexing

(provide 'dash-pt)
