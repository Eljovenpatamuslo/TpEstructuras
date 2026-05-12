--ghci -fno-warn-tabs tp.hs

import Data.List

------------------------------------------------------------chatgpteada de ayrton para mostrar NdTree (BORRAR)
instance Show p => Show (NdTree p) where
    show tree = showTree tree 0
        where
            showTree Empty _ = "E"
            showTree (Node left val right prio) indent =
                replicate indent ' ' ++ "(" ++ show val ++ "," ++ show prio ++ ")\n" ++
                replicate indent ' ' ++ "├─ left: " ++ showTree left (indent + 4) ++ "\n" ++
                replicate indent ' ' ++ "└─ right: " ++ showTree right (indent + 4)
-------------------------------------------------------------

data NdTree p = Empty | Node (NdTree p) p (NdTree p) Int deriving (Eq,Ord)

class Punto p where
    dimension :: p -> Int
    coord :: Int -> p -> Double
    dist :: Punto p => p -> p -> Double
    dist p1 p2 = sqrt (distaux p1 p2 ((dimension p1) - 1))
                where
                    distaux p1 p2 0 = ((coord 0 p2) - (coord 0 p1)) ^ 2
                    distaux p1 p2 i = (distaux p1 p2 (i-1)) + (((coord i p2) - (coord i p1)) ^ 2) 

newtype Punto2d = P2d (Double,Double) deriving (Show,Eq)
newtype Punto3d = P3d (Double,Double,Double) deriving (Show,Eq)

instance Punto Punto2d where
    dimension :: Punto2d -> Int
    dimension _ = 2

    coord :: Int -> Punto2d -> Double
    coord 0 (P2d (x,y)) = x
    coord 1 (P2d (x,y)) = y

instance Punto Punto3d where
    dimension :: Punto3d -> Int
    dimension _ = 3

    coord :: Int -> Punto3d -> Double
    coord 0 (P3d (x,y,z)) = x
    coord 1 (P3d (x,y,z)) = y
    coord 2 (P3d (x,y,z)) = z

--------------------------2--------------------------------

--Toma una lista de puntos, un punto pmed y un eje.
--Devuelve una lista de puntos los cuales son menores a pmed en la coordenada del eje
minimoMediana :: Punto p => [p] -> p -> Int -> [p]
minimoMediana [] pmed d     = []
minimoMediana (p:xp) pmed d | (coord d p) < (coord d pmed) = (p:minimoMediana xp pmed d)
                            | otherwise = (minimoMediana xp pmed d)

--Toma una lista de puntos, un punto pmed y un eje.
--Devuelve una lista de puntos los cuales son mayores a pmed en la coordenada del eje
maximoMediana :: Punto p => [p] -> p -> Int -> [p]
maximoMediana [] pmed d     = []
maximoMediana (p:xp) pmed d | (coord d p) > (coord d pmed) = (p:maximoMediana xp pmed d)
                            | otherwise = (maximoMediana xp pmed d)

--Toma una lista de puntos, un eje y devuelve la lista de puntos ordenada
--en base a la coordenada correspondiente al eje
sortPuntos :: Punto p => [p] -> Int -> [p]
sortPuntos xp d = sortBy (\p1 p2 -> compare (coord d p1) (coord d p2)) xp

--Toma una lista generica y devuelve la mediana de la lista
--si la lista es par, toma el valor de la derecha
mediana :: [a] -> a
mediana (p:[])     = p
mediana (p1:p2:[]) = p2
mediana xp         = mediana (reverse (tail (reverse (tail xp))))

--2)
fromList :: Punto p => [p] -> NdTree p
fromList xp = fromListAux xp 0
              where
                    fromListAux [] _     = Empty
                    fromListAux (p:xp) l = let 
                                            pmed = mediana (sortPuntos (p:xp) (mod l (dimension p)))
                                            izq = (fromListAux (minimoMediana (p:xp) pmed (mod l (dimension p)) ) (l+1))
                                            der = (fromListAux (maximoMediana (p:xp) pmed (mod l (dimension p))) (l+1))
                                          in (Node izq pmed der (mod l (dimension p)))     


---------------------------------3---------------------------------------
--3)
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p nt = insertarAux p nt 0
                where
                    insertarAux p Empty c           = (Node Empty p Empty (mod c (dimension p)))
                    insertarAux p (Node l pt r e) c | (coord e p) <= (coord e pt) = (Node (insertarAux p l (c+1)) pt r e) 
                                                    | otherwise = (Node l pt (insertarAux p r (c+1)) e)

---------------------------------4-----------------------------------------
--Toma Un NdTree p, un eje y devuleve el punto maximo de ese eje 
maximo :: Punto p => NdTree p -> Int -> p
maximo t@(Node _ p _ _) e = maximoAux t e p
                          where  
                                maximoAux (Node Empty pt Empty et) e pMax | e==et && (coord e pt) > (coord e pMax) = pt
                                                                          | otherwise = pMax

                                maximoAux (Node l pt Empty et) e pMax | e==et && (coord e pt) > (coord e pMax) = maximoAux l e pt
                                                                      | otherwise = maximoAux l e pMax

                                maximoAux (Node l pt r et) e pMax | e==et && (coord e pt) > (coord e pMax) = maximoAux r e pt
                                                                  | otherwise = maximoAux r e pMax

--Toma Un NdTree p, un eje y devuleve el punto minimo de ese eje 
minimo :: Punto p => NdTree p -> Int -> p
minimo t@(Node _ p _ _) e = minimoAux t e p
                            where
                                minimoAux (Node Empty pt Empty et) e pMin | e==et && (coord e pt) < (coord e pMin) = pt
				                                                          | otherwise = pMin

                                minimoAux (Node Empty pt r et) e pMin | e==et && (coord e pt) < (coord e pMin) = minimoAux r e pt
                                                                      | otherwise = minimoAux r e pMin

                                minimoAux (Node l pt r et) e pMin | e==et && (coord e pt) < (coord e pMin) = minimoAux l e pt
                                                                  | otherwise = minimoAux l e pMin
  
--4)
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p (Node Empty pt Empty e) | p==pt = Empty
                                   | otherwise = (Node Empty pt Empty e)

eliminar p (Node l pt Empty e) | p==pt = let max = maximo l e
                                            in (Node (eliminar max l) max Empty e)
                               | otherwise = (Node (eliminar p l) pt Empty 0)

eliminar p (Node l pt r@(Node _ ptr _ _) e) | p==pt = let min = minimo r e
                                                                in (Node l min (eliminar min r) e)
                                            | (coord e p) <= (coord e pt) = (Node (eliminar p l) pt r e)
                                            | otherwise = (Node l pt (eliminar p r) e)

-------------------------------5--------------------------------

type Rect = (Punto2d, Punto2d)

--Constructor de Rect (ver si hay que borrar)
punto2Rect :: (Punto2d,Punto2d) -> Rect
punto2Rect (x,y) = (x,y)

--Toma un punto p, un rect r y devuelve si la coord x de p es menor a las dos de r 
menorX :: Punto2d -> Rect -> Bool
menorX (P2d (x,_)) (P2d (x1,_),P2d (x2,_)) = x < x1 && x < x2

--Toma un punto p, un rect r y devuelve si la coord x de p es mayor a las dos de r 
mayorX :: Punto2d -> Rect -> Bool
mayorX (P2d (x,_)) (P2d (x1,_),P2d (x2,_)) = x > x1 && x > x2

--Toma un punto p, un rect r y devuelve si la coord y de p es menor a las dos de r 
menorY :: Punto2d -> Rect -> Bool
menorY (P2d (_,y)) (P2d (_,y1),P2d (_,y2)) = y < y1 && y < y2

--Toma un punto p, un rect r y devuelve si la coord y de p es mayor a las dos de r 
mayorY :: Punto2d -> Rect -> Bool
mayorY (P2d (_,y)) (P2d (_,y1),P2d (_,y2)) = y > y1 && y > y2

--a)
inRegion :: Punto2d -> Rect -> Bool
inRegion p rect | (menorX p rect || mayorX p rect || menorY p rect || mayorY p rect) = False
                | otherwise = True

--b)
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty rect           = []
ortogonalSearch (Node l pt r e) rect | inRegion pt rect = pt:((ortogonalSearch l rect) ++ (ortogonalSearch r rect)) 
				                     | (e == 0 && (menorX pt rect)) || (e == 1 && (menorY pt rect)) = ortogonalSearch r rect
                                     | (e == 0 && (mayorX pt rect)) || (e == 1 && (mayorY pt rect)) = ortogonalSearch l rect
                                     | otherwise = []

--ejemplos (BORRAR)
x = [P2d (2,3), P2d (5,4), P2d (7,2), P2d (9,6), P2d (4,7), P2d (8,1)]
y = fromList x
z = insertar (P2d (1.0,1.0)) y
v = eliminar (P2d (1.0,1.0)) z

x1 = punto2Rect (P2d (1.0,1.0),P2d (7.0,7.0))
y1 = ortogonalSearch y x1

p1 = P2d (0.0,0.0)
p2 = P2d (2.0,2.0)

pmed = mediana (sortPuntos (x) (mod 0 ((dimension p1) - 1)))
xpp = deletePunto pmed x
left = (minimoMediana xpp pmed (mod 0 (dimension p1)) )
