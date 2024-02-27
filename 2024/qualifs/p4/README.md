D'après les spécifications du problème, R (le nombre de déplacements) peut-être très grand. Deux possibilités :
- Réduire au maximum les facteurs autres que R (c'est ce qu'on a essayé de faire avec les arbres segmentés, voir `segtree.ml`) : on peut espérer obtenir une complexité en O(R * N)
- Faire un algorithme indépendant de R, c'est ce qu'on va faire ici (voir `final-sparse_table.ml`). 

On précalcule une "sparse table" immuable (à l'inverse de l'arbre segmenté dynamique) pour une complexité O(NlogN)
Puis on construit des requêtes intelligentes sur le principe d'une fenêtre glissante (avec un peu d'arithmétique),
donnant à chaque ville son nombre maximum de batiments cassés.
finalement, complexité en O(NlogN + N) = O(NlogN)
