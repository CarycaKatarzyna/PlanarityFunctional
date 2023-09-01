import System.IO

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Graf przechowywany jako para ([lista wierzchołków], [lista krawędzi])
---------------------------------------------------------------------

data Graph a = Graph ([a], [(a, a)])
instance Eq a => Eq (Graph a) 
    where (Graph (v1, e1)) == (Graph (v2, e2)) = ((v1 == v2) && (e1 == e2))

---------------------------------------------------------------------
---------------------------------------------------------------------
--              Wczytanie danych oraz funkcja main
--------------------------------------------------------------------- 

readEdge :: String -> (Int, Int)
readEdge s = (x, y)
    where [xStr, yStr] = words s
          x = read xStr
          y = read yStr

readEdges :: String -> [(Int, Int)]
readEdges s = map readEdge (lines s)

main :: IO ()
main = do
    fileName <- getLine
    fileContent <- readFile fileName
    let edg = readEdges fileContent
        graph = graphFromEdges edg
    print (isPlanar graph)
    
---------------------------------------------------------------------
---------------------------------------------------------------------
--          Główna funkcja do sprawdzania plarności grafu  
---------------------------------------------------------------------

isPlanar :: Eq a => Graph a -> Bool 
isPlanar (Graph (nodes, edg)) 
    | ((length edg) <= 4) = True
    | ((length edg) > 3 * (length nodes) - 6) = False
    | (checkIfBipartite cfragmentsGraph == False) = False
    | (length cfragments <= 1) = True
    | otherwise = isPlanar (foldl unionGraph cycle partA) && isPlanar (foldl unionGraph cycle partB)
    where
        cycle = findCycle (Graph (nodes, edg))
        cfragments = findCfragments (Graph (nodes, edg)) cycle
        cfragmentsGraph = makeCfragmentsGraph (Graph (nodes, edg)) cycle cfragments
        (partA, partB) = splitIntoNonEmptyBiparts cfragmentsGraph (startColoring cfragmentsGraph)

---------------------------------------------------------------------
---------------------------------------------------------------------
--  Funkcje do podziału grafu na c-fragmenty względem ustalonego cyklu  
---------------------------------------------------------------------

makeCfragmentsGraph :: Eq a => Graph a -> Graph a -> [Graph a] -> Graph (Graph a)
makeCfragmentsGraph graph cycle cfragments = Graph (cfragments, [(x, y) | x <- cfragments, y <- cfragments, x /= y, cfragmentsInConfilict x y cycle])

findCfragments :: Eq a => Graph a -> Graph a -> [Graph a]
findCfragments (Graph (ver, edg)) (Graph (cver, cedg)) = 
    map (addEdges ver) (
            foldl (\restEdges newEdge -> 
                if elem newEdge (concat restEdges) 
                    then restEdges 
                    else [e | e <- edg, notElem e (concat restEdges), not (edgeInGraph (Graph (cver, cedg)) e), edgesInCommonCfragment (Graph ((difference ver cver), edg)) e newEdge] : restEdges
            ) [] (disjointEdges edg (Graph (cver, cedg)))
        )

edgesInCommonCfragment :: Eq a => Graph a -> (a, a) -> (a, a) -> Bool
edgesInCommonCfragment graph (v1, v2) (u1, u2) = (v1 == u1 && v2 == u2) || (v1 == u2 && v2 == u1) || pathInGraph graph (v1, u1) || pathInGraph graph (v1, u2) || pathInGraph graph (v2, u1) || pathInGraph graph (v2, u2)

contactPoints :: Eq a => Graph a -> Graph a -> [a]
contactPoints (Graph (ver, edg)) (Graph (cver, cedg)) = intersection ver cver

cfragmentsInConfilict :: Eq a => Graph a -> Graph a -> Graph a -> Bool
cfragmentsInConfilict (Graph (ver1, edg1)) (Graph (ver2, edg2)) (Graph (cver, cedg))
    | ((length (intersection (contactPoints (Graph (ver1, edg1)) (Graph (cver, cedg))) (contactPoints (Graph (ver2, edg2)) (Graph (cver, cedg))))) >= 3) = True
    | alternating cver ver1 ver2 = True
    | otherwise = False 

alternating :: Eq a => [a] -> [a] -> [a] -> Bool
alternating cver ver1 ver2 = (fst (foldl (\result newVert -> alternatingCases ((elem newVert ver1), (elem newVert ver2)) result) (0, 0) cver)) >= 4

alternatingCases :: (Bool, Bool) -> (Int, Int) -> (Int, Int)
alternatingCases (False, False) x = x
alternatingCases (True, True) (score, c) = ((score+1), 0)
alternatingCases (True, False) (score, 0) = ((score+1), 2)
alternatingCases (True, False) (score, 1) = ((score+1), 2)
alternatingCases (True, False) (score, 2)  = (score, 2)
alternatingCases (False, True) (score, 0) = ((score+1), 1)
alternatingCases (False, True) (score, 1)  = (score, 1)
alternatingCases (False, True) (score, 2)= ((score+1), 1)

---------------------------------------------------------------------
---------------------------------------------------------------------
--  Funkcje do podziału grafu na dwie części (jeśli jest dwudzielny)  
---------------------------------------------------------------------

findVertexColor :: Eq a => a -> [(a,Int)] -> Int
findVertexColor u ((v, c) : vs) = 
    if u == v 
        then c
        else findVertexColor u vs

changeVertexColor :: Eq a => a -> Int -> [(a,Int)] -> [(a,Int)]
changeVertexColor _ _ [] = []
changeVertexColor u newC ((v,c) : vs) = 
    if u == v 
        then [(v, newC)] ++ vs
        else [(v,c)] ++ changeVertexColor u newC vs

oppositeColor :: Int -> Int
oppositeColor 1 = 2
oppositeColor 2 = 1

colorV :: Eq a => Graph a -> a -> Int -> [(a,Int)] -> [(a,Int)]
colorV (Graph (ver, edg)) v c coloring =
    if findVertexColor v coloring == 0
        then foldl (\x y -> colorV (Graph (ver, edg)) y (oppositeColor c) x) (changeVertexColor v c coloring) (neighbors (Graph (ver, edg)) v)
        else coloring

startColoring :: Eq a => Graph a -> [(a,Int)]
startColoring (Graph (ver, edg)) = foldl (\x y -> colorV (Graph (ver, edg)) y 1 x) (emptyVertColors (Graph (ver, edg))) ver

neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph (_, edg)) v = [y | (x, y) <- edg, x == v]

emptyVertColors :: Eq a =>  Graph a ->[(a,Int)]
emptyVertColors (Graph (ver, edg)) = zip ver (replicate (length ver) 0)

isOppositeColor :: Eq a => Int -> [a] -> [(a,Int)] -> Bool
isOppositeColor c ver coloring = foldl (&&) True (map (\x -> (findVertexColor x coloring) == (oppositeColor c) ) ver)

checkColoring :: Eq a =>  Graph a -> [(a,Int)] -> Bool
checkColoring (Graph (ver, edg)) coloring = foldl (&&) True (map (\x -> isOppositeColor (findVertexColor x coloring) (neighbors (Graph (ver, edg)) x) coloring) ver)

checkIfBipartite :: Eq a =>  Graph a -> Bool
checkIfBipartite (Graph (ver, edg)) = checkColoring (Graph (ver, edg)) (startColoring (Graph (ver, edg))) 

splitIntoBiparts :: Eq a => Graph a -> [(a,Int)] -> ([a], [a])
splitIntoBiparts (Graph (ver, edg)) coloring = ([x | x <- ver, (findVertexColor x coloring) == 1], [x | x <- ver, (findVertexColor x coloring) == 2])

splitIntoNonEmptyBiparts :: Eq a => Graph a -> [(a,Int)] -> ([a], [a])
splitIntoNonEmptyBiparts (Graph (ver, edg)) coloring = 
    if length partA >= 2 && length partB == 0
        then (tail partA, [head partA])
        else (partA, partB) 
            where (partA, partB) = splitIntoBiparts (Graph (ver, edg)) coloring

---------------------------------------------------------------------
---------------------------------------------------------------------
--           Pomocnicze funkcje do operacji na grafach            
---------------------------------------------------------------------

findPaths :: Eq a => Graph a -> (a, a) -> [[a]]
findPaths (Graph (ver, edg)) (v1, v2) =
  if v1 == v2
    then [[v2]]
    else map ([v1] ++) (foldl (\x y -> x ++ (findPaths (Graph ((filter (/= v1) ver), edg)) (y, v2))) [] [x | x <- ver, edgeInGraph (Graph (ver, edg)) (v1, x)])

findCyclePath :: Eq a => Graph a -> [a]
findCyclePath (Graph (ver, edg)) = [snd (head edg)] ++ longestList (findPaths (Graph (ver, edg)) (head edg))

findCycle :: Eq a => Graph a -> Graph a
findCycle graph = makeGraphFromPath (findCyclePath graph)

makeGraphFromPath :: Eq a => [a] -> Graph a
makeGraphFromPath [x] = Graph ([x], [])
makeGraphFromPath (x:(y:xs)) = unionGraph (Graph ([x], [(x, y)])) (makeGraphFromPath ([y] ++ xs))

edgeInGraph :: Eq a => Graph a -> (a, a) -> Bool
edgeInGraph (Graph (_, [])) _ = False
edgeInGraph (Graph (ver, edg)) (v1, v2) = (elem (v1,v2) edg) || (elem (v2,v1) edg)

pathInGraph :: Eq a => Graph a -> (a, a) -> Bool
pathInGraph (Graph (ver, edg)) (v1, v2)
    | ((notElem v1 ver) || (notElem v2 ver)) = False
    | (v1 == v2) = True
    | otherwise = (foldl (||) False (map (\v -> pathInGraph (Graph (filter (/= v1) ver, edg)) (v, v2)) [v | v <- ver, edgeInGraph (Graph (ver, edg)) (v1, v)]))

unionGraph :: Eq a => Graph a -> Graph a -> Graph a
unionGraph (Graph (ver1 ,edg1)) (Graph (ver2, edg2)) = Graph ((unionList ver1 ver2), (unionList edg1 edg2))

graphFromEdges :: Eq a => [(a, a)] -> Graph a
graphFromEdges edg = Graph (removeDuplicates (concatMap (\(x, y) -> [x, y]) edg), edg)

addEdges :: Eq a => [a] -> [(a, a)] -> Graph a
addEdges ver edg = Graph ([x | x <- ver, elem x (map fst edg) || elem x (map snd edg)], edg)

disjointEdges :: Eq a => [(a,a)] -> Graph a -> [(a, a)]
disjointEdges e (Graph (ver, edg)) = (filter (\x -> (edgeInGraph (Graph (ver, edg)) x) == False) e)

---------------------------------------------------------------------
---------------------------------------------------------------------
--           Pomocnicze funkcje do operacji na listach             
---------------------------------------------------------------------

longestList :: Eq a => [[a]] -> [a]
longestList [] = []
longestList lists = foldr (\x y -> if length x > length y then x else y) [] lists

difference :: Eq a => [a] -> [a] -> [a]
difference xs1 xs2 = filter (\x -> notElem x xs2) xs1

unionList :: Eq a => [a] -> [a] -> [a]
unionList xs1 xs2 = xs1 ++ [x | x <- xs2, not (elem x xs1)]

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs1 xs2 = [x | x <- xs1, elem x xs2]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : (removeDuplicates (filter (/= x) xs))