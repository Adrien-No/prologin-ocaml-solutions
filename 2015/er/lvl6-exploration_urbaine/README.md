### proof of correctness for optimisation 

# Lemma :

Let `S` be a vertex of the tree, `A` a vertex such that dist(S, A) is maximized. 
If `B` is a vertex such that dist(A, B) is maximized, then this is the diameter of the tree.

# Proof : 
By absurd.
Let P, Q such that dist(P, Q) > dist(A, B) is the diameter of the tree.

- dist(P, Q) = dist(P, S) + dist(S, Q)
Indeed, if not we would have dist(P, Q) <= dist(A, S) which would be absurd.

- dist(A, S) >= dist(P, S) by hypothesis (dist(S, A) is maximized)

- dist(A, B) >= dist(A, Q) also by hypothesis.
So, even if dist(A, S) = dist(P, S), we have dist(A, B) >= dist(P, Q). contradiction. 
