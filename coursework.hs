import Data.List


-- roman letters algorithm (Q1)

a :: [(String,Int)]
a = [ ("C",100), ("CD", 400), ("CM",900), ("D",500) , ("I", 1), ("IV",4), ("IX",9), 
      ("L",50), ("M", 1000), ("V",5), ("X",10), ("XC",90), ("XL",40)]


{- THIS FUNCTION CONVERTS ROMAN LITERALS TO INTEGER NUMBER.
   x, y - first two characters of the String
   IF  VALUE OF CHARACTER X IN THE DICTIONARY IS LESS THAN VALUE OF Y, 
   THEN WE CONCATENATE X AND Y AND SEARCH DICTIONARY FOR INT VALUE OF "XY".   
-}
   
romanToInt :: String -> Int
romanToInt [] = 0
romanToInt (x:y:xs) 
         | length (x:y:xs) == 1 = lookUp [x] a
         | lookUp [x] a < lookUp [y] a = lookUp ([x]++[y]) a + romanToInt xs 
         | otherwise = lookUp [x] a + romanToInt (y:xs)


{-THIS FUNCTION CONVERTS INTEGER NUMBER TO ROMAN NUMBER-}

intToRoman :: Int -> String
intToRoman 0 = []
intToRoman x = cSt x (reverse (Data.List.sort [y| (x,y) <- a]))


{- THIS FUNCTION SEARCHES THE  ROMAN LITERAL IN THE DICTIONARY AND GETS ITS INTEGER VALUE.

   x - String to search
   y -the dictionary/ list of (String,Int) tuples 
-}

lookUp :: String -> [(String,Int)] -> Int
lookUp x y = head [ b | (a,b) <-y, x == a]


{-THIS FUNCTION SEARCHES THE INTEGER VALUE IN THE DICTIONARY AND GETS THE STRING 
  CORRESPONDING TO THAT VALUE.
  
  x - Integer value to search
  y - the dictionary/ list of (String,Int) tuples
-}
  

lookUp2 :: Int -> [(String,Int)] -> String
lookUp2 x y = head [ a | (a,b) <-y, x == b]



{-THIS FUNCTION GETS THE STRING FROM DICTIONARY CORRESPONDING TO INTEGER NUMBER.
  x - original integer
  y - value from the list of dictionary values in descending order (starting from 1000)
  
  THE STRING VALUE OF Y IS REPEATED N TIMES, WHERE N IS A QUOTIENT FROM DIVISION OF X BY Y. 
  THEN Y*N IS SUBTRACTED FROM X TO GET THE REST OF THE INTEGER TO CONVERT.
 -}
   
cSt :: Int -> [Int] -> String
cSt 0 ys = []
cSt x [] = []
cSt x (y:ys) 
             | x `div` y > 0 =  concat (replicate (x `div` y) (lookUp2 y a))++ 
               cSt (x - y* (x `div` y))  ys
             | otherwise =  cSt x ys



-- alternade algorithm (Q3)

d :: [String]
d = [ "wit" , "ass", "shoe", "cold", "pie", "and", "bad" ,"or"]
        
{-THIS FUNCTION CHECKS WHETHER THE GIVEN WORD IS AN ALTERNADE.
   x - word String
   y - the dictionary of words
   [x!! y| y <-[0,2..(length x -1)]] - gets all characters of x at even indexes
   [x!!y| y<-[1,3..(length x -1)]]  - gets all characters of x at odd indexes 
-}
             
alternade :: String -> [String] ->Bool
alternade x [] = False
alternade x y =  ([x!! y| y <-[0,2..(length x -1)]] `elem` y)  &&  
                 ([x!!y| y<-[1,3..(length x -1)]] `elem` y)



-- suko puzzle solver (Q3)


suko :: (Int,Int,Int,Int) -> [[Int]]
suko (a,b,c,d) = [ x | x<- permutations [1..9], sum [x!!0, x!!1, x!!3, x!!4] == a && 
                                                sum [x!!1,x!!2, x!!4, x!!5] == b && 
                                                sum [x!!3,x!!4, x!!6, x!!7] == c && 
                                                sum [x!!4,x!!5, x!!7, x!!8] == d]




--words search algorithm (Q4) 

{- THIS FUNCTION CREATES A GRID OF DETERMINED DIMENSIONS, WHERE:
    x - width of the grid
    y -  the length of the grid.
    z - String of all characters supposed to  be in the grid
    
    IT CONSISTS OF (CHAR,INT) TUPLES WHERE:
    Char - character from the String of characters
    Int - is the row number
 -}
 
grid :: Int->Int->String ->[(Char,Int)]
grid x y [] = []
grid x y z = zip z rows 
             where rows = concat [replicate x i| i<-[0..y]]
             
             
{- THIS FUNCTION CREATES A GRID, BY DIVIDING THE STRING OF LETTERS FOR THE GRID INTO
   SEPERATE STRINGS.
   x - number of letters/characters to take
   z - String of all characters for the grid  
-}  
        
grid2 :: Int-> String -> [String]
grid2 x [] =[]
grid2 x z = [Data.List.take x z ]++ grid2  x (Data.List.drop x z)  
             

{-CREATE LISTS OF TUPLES CORRESPONDING TO DIAGONALS IN THE GRID.

  i - list of tuples created by grid function
  (x:xs) - list of columns idices
  y - step between the characters, i.e width of the grid
  z - number of characters in the grid, i.e grid length 
-}
       
listDiagonals :: [(Char, Int)]->[Int]->Int -> Int ->[[(Char,Int)]]
listDiagonals i [] y z = []
listDiagonals i (x:xs) y z = [i!! l | l<-[x,y..z]] : listDiagonals i xs (y+1) z



{-  THIS FUNCTION CERATE A STRING WITH WHITE SPACES BETWEEN DEFFERENT DIAGONAL WORDS,
   BY COMPARING ROW VALUES OF EACH TUPLE IN THE LIST OF TUPLES-}

diagonal :: [(Char, Int)] -> String
diagonal [] = []
diagonal ((a,b):xs)
            |length xs ==1 && (b < snd (head xs))= [a]++[fst (head xs)]  
            | b < snd (head xs) = a:diagonal xs
            |otherwise = a:' ':diagonal xs
  
   

       
{-THIS FUNCTION SEARCHES THE GRID OF LETTERS FOR WORDS IN DICTIONARY AND PROVIDES 
  THEIR LOCATION IN THE GRID. 
  x - width of the grid
  y - length of the grid
  z - String of letters of the grid
  i - dictionary of words
-}


wordSearch :: Int ->Int->String -> [String] ->[String]
wordSearch x y z [] =[]
wordSearch x y z i = 
                     [j++ " right"| j<-i , isInfixOf j z] ++
                     [j ++ " left" | j<-i , isInfixOf j (reverse z)] ++
                     [j ++ " down" | j<-i , k<- transpose (grid2 x z), isInfixOf j k] ++
                     [j++ " up" | j<- i,  k<- transpose (grid2 x z), isInfixOf j (reverse k)] ++
                     [j++ " down-left" | j<-i, k<-concat (a), isInfixOf j k] ++
                     [j++ " up-right" | j<-i, k<-concat (b), isInfixOf j (reverse k)]++
                     [j++ " down-right" | j<-i, k<- concat (c), isInfixOf j k] ++ 
                     [j++ " up-left" | j<-i, k<- concat (d), isInfixOf j (reverse k)]
                     
                     where a = map (words) (map (diagonal) (listDiagonals (grid x y z) [1..(x-1)] x (x*y -2)))
                           b = map (words)(map (diagonal) (listDiagonals (grid x y z) [1..(x-1)] x (x*y -2) ))
                           c = map (words)(map (diagonal) (listDiagonals (grid x y (concat [ reverse b| b<-grid2 x z])) [1..(x-1)] x (x*y -2)))
                           d = map (words) (map (diagonal) (listDiagonals (grid x y (concat [ reverse b| b<-grid2 x z])) [1..(x-1)] x (x*y -2)))


dict_words :: [String]
dict_words = ["UPGRADE", "DUST", "WARES", "GRADE", "DEEP", "DEEPE", "GAN", "ERA", "RAW", "WIT", "LAW", "FLOPPY", "SOFTWARE", "HARDDRIVE"]

g :: String
g = "IUPGRADEEPEQYTDZMTZVNRXSYVCECTIWALZRPCPGERSWGCREPGLUDVDUCFNSONTDJRRWDFOYLVRGAXAIYFKZFAUHBGSXTELIHEGSPKHW" ++
     "YPOCTESZEBABIDKYNZWTUROHOIPKMXTGEADGAVLUTESSRMEMORYODIQDTROMTKSLIRCTLAPTOPOX"

main :: IO()
main = print (wordSearch 12 15 g dict_words)
     
