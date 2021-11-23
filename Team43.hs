import DataFile 
------------------------------------
member x [] = False 
member x (y:ys)| x==y = True 
               | otherwise = member x ys 
-------------------------------------	
---wordToken
wordToken:: String -> [String] 			
wordToken1 [] l c = (l ++ [c])
wordToken1 (x:xs) l c |member x punct = wordToken1 xs (l ++ [c,[x]]) []
                      |x ==' ' =wordToken1 xs (l ++ [c]) []
                      |otherwise = wordToken1 xs l (c++[x])
filter1 [] l = l 					  
filter1 (x:xs) l | x == "" = filter1 xs l 
                 | otherwise = filter1 xs (l++[x]) 
wordToken x = filter1 (wordToken1 x [] []) []				 
---------------------------------------
---wordTokenList
wordTokenList :: [String] -> [String]
wordTokenList x = foldr (++) [] (map wordToken x)	
---------------------------------------
---uniqueBigrams
uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams1 [x1] l = l
uniqueBigrams1 (x1:x2:xs) l = uniqueBigrams1 (x2:xs) (l++[(x1,x2)])
uniqueBigrams x = remove (uniqueBigrams1 x []) []  
-------------------------------------
remove [] l = l 
remove (x1:xs) l | member x1 xs = remove xs l
                 |otherwise=remove xs (l++[x1])
-------------------------------------
---uniqueTrigrams
uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams1 [x1,x2] l = l
uniqueTrigrams1 (x1:x2:x3:xs) l = uniqueTrigrams1 (x2:x3:xs) (l++[(x1,x2,x3)])
uniqueTrigrams x = remove (uniqueTrigrams1 x []) [] 
------------------------------------- 
---bigramsFreq
bigramsFreq :: Num a => [String] -> [((String,String),a)]
getFreq1 x [] c = c 
getFreq1 x (y:ys) c | x==y = getFreq1 x ys (c+1)
                   | otherwise = getFreq1 x ys c 
bigramsFreq1 [] l a = l
bigramsFreq1 (x1:xs) l a = bigramsFreq1 xs (l++[(x1 ,(getFreq1 x1 a 0))]) a
bigramsFreq x = bigramsFreq1 (uniqueBigrams x) [] (uniqueBigrams1 x [])   
--------------------------------------- 
---trigramsFreq
trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq1 [] l a = l
trigramsFreq1 (x1:xs) l a = trigramsFreq1 xs (l++[(x1 ,(getFreq1 x1 a 0))]) a
trigramsFreq x = trigramsFreq1 (uniqueTrigrams x) [] (uniqueTrigrams1 x [])  
---------------------------------------
---getFreq
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq x2 [] = 0
getFreq x2 ((x1,b):xs) | x2 == x1 = b
                       | otherwise=getFreq x2 xs 
---------------------------------------
---generateOneProb
generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((x1,x2,x3),a) [] = 0
generateOneProb ((x1,x2,x3),a) ((y,b):ys) | (x1,x2) == y = a/b
                                          | otherwise = generateOneProb ((x1,x2,x3),a) ys
---------------------------------------
--- genProbPairs 
genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs1 [] b l = l
genProbPairs1 ((x1,x2):xs) b l = genProbPairs1 xs b (l++[(x1,(generateOneProb (x1,x2) b))])
genProbPairs x y = genProbPairs1 x y [] 
---------------------------------------	
---generateNextWord
generateNextWord :: (Ord a, Fractional a) => [String] -> [((String,String,String),a)] -> String
generateNextWord1 a [] l = l 
generateNextWord1 a (((x1,x2,x3),b):xs) l |((last (uniqueBigrams a)) == (x1,x2))&& b>0.03 = generateNextWord1 a xs (l++[x3])
                                          |otherwise=generateNextWord1 a xs l 
generateNextWord a b | (length (generateNextWord1 a b [])) == 0 = error "Sorry, it is not possible to infer from current database" 
                     | otherwise = (generateNextWord1 a b []) !! randomZeroToX ((length (generateNextWord1 a b []))-1)
----------------------------------------
---generateText
generateText :: String -> Int -> String
generateText1 a = generateNextWord (wordTokenList a) (genProbPairs (trigramsFreq (wordTokenList (docs)))   (bigramsFreq (wordTokenList (docs))))
generateText a 0 = a 
generateText a b = generateText (a++" "++(generateText1 [a])) (b-1)
-----------------------------------------

									  
				 