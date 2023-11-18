Assume a type
```
(X, (X, [])) where X = (X, []) | ([], [])
```
You can enter this type in CDuce which simplifies to `[[X] [X]] where X = [X?]`
(binary trees of `[]`). Such a type can be built with the 
```
t where X = ... and Y = ...
```
syntax or from a simpler sequence type where common sub-expressions are
hash-consed. And even if this recursive type looks weird, in all generality
sub-typing must work correctly for it. Such types may also arise as the result
of instanciation, or applications of boolean connectives on other "more natural"
types.

We want to show it is not empty (e.g. an inhabitant of that type is: 
`[ [[]] [[]] ]`), so we explore it's structure until we can prove the type
to be non empty. In what follows, "assume a type is empty" means record it has
empty in the memoization table.

1. Show that `(X, (X, []))` is not empty, assume it is:
   1. Show that `X` and `(X, [])` are not empty, (product)
   2. Show that `X = (X, []) | ([], [])` is not empty, assume it is:
       1. Show that `(X, [])` (union 1) is not empty, assume it is:
          1. Show that `X` and `[]` are not empty (product)
          2. X is assumed to be empty, the whole product
             is empty, so explore the rest of the union.
             Here it is correct to say that `(X, [])` is empty
             since it is a cycle and we have not found a
             base case yet to stop the recursion
       2. Show that `([], [])` (union 2) is not empty, assume it is:
          1. Show that `[]` is not empty, OK (basic type)
          2. Show that `[]` is not empty, OK (basic type)
       3. Update memo: `([], [])` is non empty
   3. Update memo, `X` is non empty.
      We have learned that `X` is non empty, because of
      its `([], [])` component. However, we still
      have memoized that `(X, [])` is empty in step 1.2.1
      If we don't undo that, mayhem ensues:
2. Show that `(X, [])` is not empty (second part of the
   product of the top-level type we are checking).
   From the memo table, `(X, [])` is empty (found in
   1.2.1)
3. One of the component of the product is empty, so
   the whole type is deduced to be empty.

It shows that in 1.3, when coming back to `X`, we must undo the types we found
empty that relied on the (wrongly assumed) fact that `X` was empty.
Indeed, if in step 1.3 we correctly undo things and remove `(X, [])` from the
memo table, then when doing 2., we will explore `(X, [])` again, this time with
the (correct) hypothesis that `X` is non empty, and find that the whole product is
non empty.


The crux of the matter is that, when checking a type t that may be cyclic, we
need to:
1. first assume that it is empty (since may be
   cyclic), until we find a base case to the recursion.
2. if no such base case is found, e.g. t = (t, t) then
   the type is indeed empty (there are no inhabitants
   since semantic sub-typing assumes finite values).
3. if we find a (non-empty) base case, while returning
   from the recursive call, we must invalidate intermediary
   results that were found empty and (*) that used the hypothesis
   that t was empty.


A sound and easy approximation of (*) is that we invalidate all types that were
found empty during the descent. Frisch does something much more sophisticated
and tracks the dependencies a bit differently and more precisely (this is the
CPS style code in types.ml from CDuce and the combination section 7.1.3 and 7.2
of his thesis). Types whose vacuity depend and the status of other types (in our
example `(X, [])` depends on `X`), are kept in a todo list and when all their
dependencies are satisfied (here `X`) *and* if they are needed, then they are
computed. This prevents both the "undoing" (backtracking).

Here, intuitively while descending the first time into `(X, [])` CDuce's code
computes a closure
```
 f = (fun () -> is_empty memo X && is_empty memo [])
```
which is kept in the memo table. When coming back the second time on `(X, [])`, then
the algorithm computes `f ()`. Since at that moment the correct and final status of
X is in the memo table, the function returns the correct result for that
function.

It is unclear whether building these intermediary closures is slower or faster
than invalidated and re-exploring types. Also the code in CDuce is further complicated
by the fact that it does not compute a boolean but rather a witness that the type
is inhabited.
