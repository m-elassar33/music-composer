import MusicResources
import Data.Function (on)
import Data.List (sortBy)

subListCount _ [] = 0
subListCount _ [a] = 0
subListCount (a:b:c) (x:y:z)| a==x&&b==y = 1 + subListCount (a:b:c) z
                            | otherwise=subListCount (a:b:c) (y:z)
subListCountAll list [] = 0
subListCountAll list (x:xs) = subListCount list x + subListCountAll list xs

pairGenerator a b | not(subListCountAll (a:b:[]) training==0) = (subListCountAll (a:b:[]) training,b)
		  | otherwise=(0,'A')

listGenerator a []=[]
listGenerator a (x:xs)  |not((pairGenerator a x)==(0,'A'))=(pairGenerator a x) :(listGenerator a xs)
						|otherwise=listGenerator a xs

sortMyList list = reverse(sortBy (compare `on` fst) list)

mySortedList a list = sortMyList (listGenerator a list)

listGeneratorAll [] _ = []
listGeneratorAll (x:xs) list = (x,mySortedList x list) :listGeneratorAll xs list

makeStatsList :: [(Char,[(Int,Char)])]
makeStatsList = listGeneratorAll chars chars

getList a [] = []
getList a ((x,y):xs) = if x==a then y else getList a xs

repeatInsertNtimes 0 _ = []
repeatInsertNtimes n a = a:repeatInsertNtimes (n-1) a

tempList [] = []
tempList ((x,y):xs) = (repeatInsertNtimes x y)++tempList xs

getRandomly [] = 'A'
getRandomly list = list!!(randomZeroToX ((length list)-1))

composeOneChar a = getRandomly (tempList (getList a makeStatsList))

composeHelper a 0 = []
composeHelper a n | composeOneChar a == 'A' = []
	  	  | otherwise = temp:(composeHelper (temp) (n-1)) where temp = composeOneChar a

compose :: Char -> Int -> [Char]
compose a 0 = []
compose a n =if composeOneChar a == 'A' then [] else a:(composeHelper a (n-1))