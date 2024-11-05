#lang racket
(require hosted-minikanren)
(require hosted-minikanren/numbers)
 
;; element (listof element) -> goal
;; Purpose: Determines if an element is part of a "collection"
(defrel (membero element loe)
  (conde
   [(== loe '()) fail]                     
   [(fresh (head tail)
           (== (cons head tail) loe)
           (== element head))]                  
   [(fresh (head tail)
           (== (cons head tail) loe)
           (membero element tail))]))   

;; element (listof element) -> goal
;; Purpose: Determines if an element is not part of a "collection"
(defrel (not-membero element loe)
  (conde
   [(== loe '()) succeed] 
   [(fresh (head tail)
           (== (cons head tail) loe)
           (=/= element head)
           (not-membero element tail))]))

;; color color -> goal
;; Purpose: Makes sure two neighbor node's colors are not the same
(defrel (not-equal-color c1 c2)
  (=/= c1 c2))  

;; graph (listof color) node-coloring -> goal
;; Purpose: Solve the graph coloring problem and outputs the possible
;; solutions following the constraints and assigning each node to a color
(defrel (color-graph graph loc node-coloring)
  (fresh (nodes)
         (new-node graph nodes)
         (assign-colors nodes loc node-coloring)
         (check-neighbors graph node-coloring)))

;; (listof edge) (listof node) -> goal
;; Purpose: To extract nodes from each edge to create a (listof new-node)
;; making sure no duplicates are in it more than once
(defrel (new-node loe lon)
  (conde
   [(== loe '()) (== lon '())]
   [(fresh (p1 p2 tail new-rest p1-node)
           (== (cons (list p1 p2) tail) loe)
           (new-node tail new-rest)
           (add-node p1 new-rest p1-node)
           (add-node p2 p1-node lon))]))

;; elem (listof node) result -> goal
;; Purpose: Helper function for new-node that makes sure to add an element to the list
;; iff it has not been added yet
(defrel (add-node elem lon result)
  (conde
   [(membero elem lon) (== result lon)]
   [(fresh (new-result)
           (not-membero elem lon)
           (== new-result (cons elem lon))
           (== result new-result))]))

;; (listof node) (listof color) node-coloring -> goal
;; Purpose: Assigns colors to nodes depending on what options there are in
;; the given (listof colors) following constraints
(defrel (assign-colors lon loc node-coloring)
  (conde
   [(== lon '()) (== node-coloring '())]
   [(fresh (head tail color rest-coloring)
           (== (cons head tail) lon)
           (membero color loc)
           (assign-colors tail loc rest-coloring)
           (== node-coloring (cons (list head color) rest-coloring)))]))

;; (listof edge) node-coloring -> goal
;; Purpose: Checks neighboring edges and makes sure they have different
;; colors from their neighbors
(defrel (check-neighbors loe node-coloring)
  (conde
   [(== loe '())] 
   [(fresh (edge node1 node2 node1-color node2-color rest-edges)
           (== (cons edge rest-edges) loe)
           (== edge (list node1 node2))
           (membero (list node1 node1-color) node-coloring)
           (membero (list node2 node2-color) node-coloring)
           (not-equal-color node1-color node2-color)
           (check-neighbors rest-edges node-coloring))]))
 
;; Sample values for (listof color):
(define loc1 '(red green blue))
(define loc2 '(maroon pink))
(define loc3 '(blue yellow green orange))

;; Sample values for a graph:
(define graph1 '((a b) (a c) (b c) (b d) (d c) (c d)))
(define graph2 '((a b) (a c)))
(define graph3 '((a b) (a c) (b c) (b d) (d c) (c d) (b e) (d a) (f a) (f b)))

;; Sample expressions for color-graph:
(define GCP1
  (run* (q)
      (color-graph graph1 loc1 q)))

(define GCP2
  (run* (q)
      (color-graph graph2 loc2 q)))

(define GCP3
  (run* (q)
      (color-graph graph3 loc3 q)))

#|

graph 1:

a-----b
|    /|
|  /  |
|/    |
c-----d

graph 2:

   b
  / 
 /
a
 \
  \
   c

graph 3:
   
   f
  / \
 /   \
a-----b-----e
| \  /|
|  /  |
|/   \|
c-----d

|#
