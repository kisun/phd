# Idea for using route/shape segmentation

The idea is to incrementally split shapes into smaller and smaller pieces that are common across more than one shape.
An example with two shape file is:
```
shape1: {a, b, c,  d, e}
shape2: {a, b, c', d, e}
```
That is, shapes 1 and 2 differ only in the third segment.
There may be a third route that implements a variation on segment `a` in that if `a = {a1, a2, a3}`, then `a' = {a1, a2', a3}`.
Therefore, the new shape files become:
```
shape1: {a1, a2,  a3, c,  d, e}
shape2: {a1, a2,  a3, c', d, e}
shape3: {a1, a2', a3, c,  d, e}
```
and so on until all of the shape files have been processed.
Of course, each segment will have have an arbitrary ID (e.g., `fdsa73bas`).

## Usefulness

Hopefully, this will make it possible to get information related so a __segment of road__, rather than be stuck with a particular route.
Say the bus of interest is traveling along shape 1, and is in segment `a3`. We can get all data for the segment `{a3, c, d, e}` (i.e., what remains of its route) which can come from shapes 2 and 3.

Also, say the bus traveling along shape1 is about to start, and also two buses traveling along shapes 2 and 3 are already en route. Even though neither match up completely, we have "real-time"* data for all of shape 1!

(* within the last 30 minutes or so)

## Difficulty

The trouble is figuring out how to split the shapes up in a way that's not too computationally demanding (it may need to be run once a month or so when shapes are changed).
