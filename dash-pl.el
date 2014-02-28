(require 'dash)

(defun -pl-size (plist)
  "Return the number of elements in the PLIST.

Type: Plist k a -> Int"
  )

(defun -pl-null (plist)
  "Return non-nil if the PLIST is empty.

Type: Plist k a -> Bool"
  )

(defun -pl-get (key plist &optional default)
  "Return the value at KEY in PLIST.

Return nil if no such element is found or DEFAULT if this is
supplied.

Warning: this function might return nil even if the key is
present in the situation when the value for this key is nil.  Use
`-pl-member' to test if the key is in the map or `-pl-lookup' to
retrive the (key . value) pair safely.

Type: k -> Plist k a -> a"
  )

(defun -pl-member-by (comp key plist)
  "Return non-nil if PLIST contains KEY.

The keys are compared using COMP, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: (k -> k -> Bool) -> k -> Plist k a -> Bool"
  )

(defun --pl-member-by (form plist key)
  "Anaphoric version of `-pl-member-by'."
  )

(defun -pl-member (key plist)
  "Return non-nil if PLIST contains KEY.

The keys are compared using `equal'.

Type: k -> Plist k a -> Bool"
  )

(defun -pl-lookup-by (comp key plist &optional default)
  "Find the value at KEY in PLIST.

Return (k . a) if the key is found, or nil otherwise.

If DEFAULT is supplied, return this instead of nil if key is not
found.

The keys are compared using COMP, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: (k -> k -> Bool) -> k -> Plist k a -> Maybe (k . a)"
  )

(defun -pl-lookup (key plist &optional default)
  "Find the value at KEY in PLIST.

Return (k . a) if the key is found, or nil otherwise.

If DEFAULT is supplied, return this instead of nil if key is not
found.

The keys are compared using `equal'.

Type: k -> Plist k a -> Maybe (k . a)"
  )

(defun -pl-insert-with-by (fun comp key value plist)
  "Insert with a function, combining new value and old value.

Insert the pair (KEY, VALUE) into MAP if key does not exist in
the map.  If the key does exist, insert the pair (KEY, (FUN VALUE
old-value)).

The keys are compared using COMP, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: (a -> a -> a) -> (k -> k -> Bool) -> k -> a -> Plist k a -> Plist k a"
  )

(defun -pl-insert-by (comp key value plist)
  "Insert a new KEY and VALUE in the MAP.

If the key is already present in the map, the associated value is
replaced with the supplied value.

The keys are compared using COMP, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: (k -> k -> Bool) -> k -> a -> Map k a -> Map k a"
  )

(defun -pl-insert-with (fun key value plist)
  "Insert with a function, combining new value and old value.

Insert the pair (KEY, VALUE) into MAP if key does not exist in
the map.  If the key does exist, insert the pair (KEY, (FUN VALUE
old-value)).

The keys are compared by `equal'.

Type: (a -> a -> a) -> k -> a -> Plist k a -> Plist k a"
  )

(defun -pl-insert (key value plist)
  "Insert a new KEY and VALUE in the MAP.

If the key is already present in the map, the associated value is
replaced with the supplied value.

The keys are compared by `equal'.

Type: k -> a -> Map k a -> Map k a"
  )

(defun -pl-delete-by (comp key plist)
  "Delete KEY and its value from PLIST.

When KEY is not a member of PLIST, the original PLIST is
returned unmodified.

The keys are compared using COMP, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: (k -> k -> Bool) -> k -> Plist k a -> Plist k a"
  )

(defun -pl-delete (key plist)
  "Delete KEY and its value from PLIST.

When KEY is not a member of PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: k -> Plist k a -> Plist k a"
  )

(defun -pl-adjust-by (comp fun key plist)
  "Update value at KEY with (FUN value).

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared using COMP, which should return non-nil if
keys are \"equal\" and nil otherwise.

Type: (k -> k -> Bool) -> (a -> a) -> k -> Plist k a -> Plist k a"
  )

(defun -pl-adjust (fun key plist)
  "Update value at KEY with (FUN value).

When KEY is not a member of the PLIST, the original PLIST is
returned unmodified.

The keys are compared by `equal'.

Type: (a -> a) -> k -> Plist k a -> Plist k a"
  )

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
