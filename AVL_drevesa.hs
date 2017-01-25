-- AVL drevesa

{-  AVL drevo je uravnoteženo dvojiško iskalno drevo, torej takšno iskalno
drevo, v katerem se pri vsakem vozlišču globini levega in desnega poddrevesa
razlikujeta za največ 1. Izumila sta ga sovjetska matematika Adelson-Velskii in
Landis v šestdesetih letih. Podatkovni tip AVLDrevo je že delno implementiran.
Ker si ne moremo privoščiti, da bi globino poddreves računali vsakič znova, te
podatke računamo sproti in hranimo v vozliščih drevesa (konstruktor ima torej
dodaten parameter). -}

import System.Environment

data AVLDrevo a = Prazno | Sestavljeno Int (AVLDrevo a) a (AVLDrevo a) deriving (Show)

-- here's a drawing function, which, combined with [putStrLn], lets you visualise a tree
data S = L | R
draw :: Show a => AVLDrevo a -> String
draw t = "\n" ++ draw' Nothing t 0 ++ "\n"
  where
    draw' _ Prazno _ = []
    draw' dir (Sestavljeno _ l v r) d =
      draw' (Just R) r (d+1) ++ node dir ++ draw' (Just L) l (d+1)
      where
        node dir' = padding d ++
          case dir' of
            Nothing -> ""
            Just L -> "\\- "
            Just R -> "/- "
          ++ show v ++ "\n"
        padding n = replicate (n*4) ' '


prazno :: AVLDrevo a
prazno = Prazno

-- Sestavi pomožni konstuktor [avldrevo], ki iz treh parametrov (levo, x, desno)
-- sestavi AVLDrevo. V gornji konstruktor je torej potrebno dodati le še globino
-- drevesa.

avldrevo :: AVLDrevo a -> a -> AVLDrevo a -> AVLDrevo a
avldrevo _levo x _desno = Sestavljeno (1 + (max (visina _levo) (visina _desno))) _levo x _desno
 

-- Zdaj sestavi še funkcijo [visina], ki iz AVL drevesa prebere njegovo višino.
visina :: AVLDrevo a -> Int
visina Prazno = 0
visina (Sestavljeno v _levo x _desno) = v

-- Implementiraj funkcijo [vsebuje], ki ugotovi, če AVL drevo vsebuje element.
vsebuje :: (Ord a) => AVLDrevo a -> a -> Bool
vsebuje = undefined

-- AVL drevesa so uravnotežena. Implementiraj pomožno funkcijo [razlika], ki
-- izračuna razliko višin levega in desnega poddrevesa danega AVL drevesa.
razlika :: AVLDrevo a -> Int
razlika Prazno = 0
razlika (Sestavljeno _v _levo x _desno) = abs(visina _levo - visina _desno)

-- Kadar drevo ni uravnoteženo, ga lahko popravimo z levimi oziroma desnimi
-- rotacijami. Implementiraj funkciji [rotL] in [rotD], ki izvedeta eno levo
-- oziroma desno rotacijo na AVL drevesu.
rotD :: AVLDrevo a -> AVLDrevo a
rotD Prazno = Prazno 
rotD r@(Sestavljeno _v Prazno _x _desno) = r 
rotD (Sestavljeno _v (Sestavljeno _v1 levo1 x1 desno1) x desno)  = Sestavljeno _v levo1 x1 (Sestavljeno _v1 desno1 x desno)

rotL :: AVLDrevo a -> AVLDrevo a
rotL Prazno = Prazno 
rotL (Sestavljeno _v levo1 x1 (Sestavljeno _v1 desno1 x desno)) = (Sestavljeno _v (Sestavljeno _v1 levo1 x1 desno1) x desno)

-- Implementiraj funkcijo [uravnotezi], ki dano drevo uravnotezi. Predpostavi,
-- da je dano drevo "skoraj" uravnoteženo: dobili smo ga tako, da smo nekemu AVL
-- drevesu dodali ali odstranili eno vozlišče.
uravnotezi :: (Ord a) => AVLDrevo a -> AVLDrevo a
uravnotezi Prazno = Prazno
uravnotezi d@(Sestavljeno _v levo@(Sestavljeno _v1 levo1 x1 desno1) x desno@(Sestavljeno _v2 levo2 x2 desno2))
     |(razlika d == 2 && (visina desno) > (visina levo) && (visina desno2) > (visina levo2)) = rotL d
	 |(razlika d == 2 && (visina desno) > (visina levo)) = rotL (Sestavljeno _v levo x (rotD desno)) 
	 
         -- |(visina desno) > (visina levo) 
		  --   | (visina desno2) > (visina levo2) = rotL d
			-- | otherwise = rotL (Sestavljeno _v levo x (rotD desno))
         |otherwise
		     | (visina desno1) > (visina levo1) = rotD (Sestavljeno _v (rotL levo) x desno)
			 | otherwise = rotD d 
	 |otherwise = Sestavljeno _v (uravnotezi levo) x (uravnotezi desno)

-- Implementiraj funkcijo [dodaj], ki AVL drevesu doda element. Pri tem
-- upoštevaj, da bo morda potrebno drevo še uravnotežiti.
dodaj :: (Ord a) => AVLDrevo a -> a -> AVLDrevo a
dodaj = undefined

-- Ko je funkcija [dodaj] implementirana, lahko sestavimo še funkcijo [izSeznama],
-- ki iz seznama sestavi AVL drevo.
izSeznama :: (Ord a) => [a] -> AVLDrevo a
izSeznama = undefined

-- Implementiraj funkcijo [najboljLevi], ki vrne najbolj levi element
-- danega AVL drevesa.
najboljLevi :: (Ord a) => AVLDrevo a -> Maybe a
najboljLevi Prazno = Nothing
najboljLevi (Sestavljeno 1 Prazno x desno) = Just x 
najboljLevi (Sestavljeno _v levo x desno) = najboljLevi levo 

-- Implementiraj funkcijo [odstrani], ki iz AVL drevesa odstrani element.
levi :: AVLDrevo a -> a 
levi (Sestavljeno 1 Prazno x desno) = x 
levi (Sestavljeno _v levo x desno) = levi levo


odstrani :: (Ord a, Eq a) => AVLDrevo a -> a -> AVLDrevo a
odstrani (Sestavljeno _v Prazno x desno) y 
    |x==y = desno 
odstrani (Sestavljeno _v levo x desno) y
    |x==y = (Sestavljeno _v levo (levi desno) (odstrani (desno) (levi desno))) 
	|y < x = (Sestavljeno _v (odstrani levo y) x desno) 
	|otherwise = (Sestavljeno _v levo x (odstrani desno y))
	
	