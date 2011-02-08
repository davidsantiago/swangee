(ns swangee.utils)

;; This function lets us return singletons or collections and treat them like they
;; are all collections.
(defn as-coll
  "If the argument is a collection, returns it. Otherwise, puts it in a vector.
   Nil counts as a collection."
  [arg]
  (if (or (nil? arg) (coll? arg))
    arg
    [arg]))