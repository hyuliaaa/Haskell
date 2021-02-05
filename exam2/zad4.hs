
data BTree = Empty | Node Int BTree BTree

t1 :: BTree 
t1 = Node 8 (Node 3 (Node 1 Empty Empty) 
 (Node 4 Empty Empty)) 
 (Node 10 (Node 9 Empty Empty) 
 (Node 14 Empty Empty))


t2 :: BTree 
t2 = Node 8 (Node 3 (Node 1 Empty Empty) 
 (Node 4 Empty Empty)) 
 (Node 10 (Node 5 Empty Empty) 
 (Node 14 Empty Empty)) 

t3 :: BTree 
t3 = Node 8 (Node 3 (Node 5 Empty Empty) 
 (Node 6 Empty Empty)) 
 (Node 10 (Node 9 Empty Empty) 
 (Node 14 Empty Empty))


isBinarySearchTree :: BTree -> Bool
isBinarySearchTree Empty  = True
isBinarySearchTree (Node x Empty Empty) = True
isBinarySearchTree (Node x left right)  = x > getRootData (getTree left) && x <= getRootData ( getTree right) && isBinarySearchTree left && isBinarySearchTree right 


 


getRootData (Node x _ _) = x

getTree::BTree ->BTree
getTree Empty = Empty
getTree t@(Node x left right) =t

getElemets:: BTree ->[Int]
getElemets Empty = []
getElemets  (Node x Empty Empty) =[x]
getElemets  (Node x left right) =getElemets left ++[x]++ getElemets right



