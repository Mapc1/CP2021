module BTree
open Cp
import Data.Monoid
import Control.Applicative
import Data.List
-- (1) Datatype definition -----------------------------------------------------
type btree = Empty | Node(a, (BTree a, BTree a))
let inBTree x = 
         match x with either (const Empty) Node
let outBTree x =
         match x with
         | Empty -> Left ()
         | Node (a,(t1,t2)) -> Right(a,(t1,t2))
-- (2) Ana + cata + hylo -------------------------------------------------------
let recBTree f = baseBTree id f         
let rec cataBTree a = a << (recBTree (cataLBree a)) << outBree
let rec anaBTree f = inLTree << (recBTree (anaBTree f) ) << f
let hyloBTree a c = cataBTree a << anaBTree c
-- (3) Map ---------------------------------------------------------------------
instance Functor BTree
         where fmap f = cataBTree ( inBTree . baseBTree f id )
let fmap f = cataBTree ( inBTree << baseBTree f id )
-- (4.1) Inversion (mirror) ----------------------------------------------------
let invBTree x = cataBTree (inBTree << (id -|- swap)) x
-- (4.3) Serialization -------------------------------------
let preord = 
    let g(x,(l,y))= x :: l @ y
    in either nil g 
let inord  = 
    let f(x,(l,y))= l @ [x] @ y
    in either nil f
let posord = 
    let h(x,(l,y))=l @ y @ [x]
    in either nil h
let inordt = cataBTree inord
let preordt = cataBTree preord
let postordt = cataBTree posord 
-- (4.4) Quicksort -----------------------------------------
let rec =
    match l with
        | q [] -> ([],[])
        | q (h::t) ->
                if q h then let (s,l) = part q t in (h::s,l)
                else let (s,l) = part q t in (s,h::l)
let qsep l =
    match l with
        | [] -> i1 ()
        | (h::t) -> i2 (h,(s,l)) where (s,l) = part (<h) t
let qSort = hyloBTree inord qsep)
-- (4.5) Traces -------------------------------------------
let tunion(a,(l,r)) = union (map (a) l) (map (a) r) 
let traces = cataBTree (either (konst [[]]) tunion))
-- (5) Depth and balancing (using mutual recursion) --------
let h (a,((b1,b2),(d1,d2))) = (b1 && b2 && abs(c1-c2)<=1,1+max c1 c2)
let f((b1,c1),(b2,c2)) = ((b1,b2),(c1,c2))
let g = either (const(true,1)) (h << (id><f))
let baldepth = cataBTree g
let balBTree = p1 << baldepth
let baldepthBTree = p2 << baldepth