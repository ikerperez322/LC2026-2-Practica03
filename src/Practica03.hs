module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Cons True) = (Cons True)
fnn (Cons False) = (Cons False)
fnn (Var p) = (Var p)
fnn (Not (Cons True)) = (Not (Cons True))
fnn (Not (Cons False)) = (Not (Cons False))
fnn (Not (Var p)) = (Not (Var p))
fnn (Not (Not f1)) = fnn f1
fnn (Not (And f1 f2)) = (Or (negar f1) (negar f2))
fnn (Not (Or f1 f2)) = (And (negar f1) (negar f2))
fnn (Or f1 f2) = (Or (fnn f1) (fnn f2))
fnn (And f1 f2) = (And (fnn f1) (fnn f2))
fnn (Impl f1 f2) = (Or (negar f1) (fnn f2))
fnn (Syss f1 f2) = (fnn (And (Impl f1 f2) (Impl f2 f1)))


--Función auxiliar para Ejercicio 1
negar :: Prop -> Prop
negar (Cons True) = (Cons False)
negar (Cons False) = (Cons True)
negar (Var p) = (Not (Var p))
negar (Not f1) = f1
negar (Or f1 f2) = (And (negar f1) (negar f2))
negar (And f1 f2) = (Or (negar f1) (negar f2))
negar (Impl f1 f2) = (And f1 (negar f2))
negar (Syss f1 f2) = (negar (And (Impl f1 f2) (Impl f2 f1)))


--Ejercicio 2
fnc :: Prop -> Prop
fnc (Cons a) = (Cons a)
fnc (Var p) = (Var p)
fnc (Not (Var p)) = (Not (Var p))
fnc (And f1 f2) = (And (fnc f1) (fnc f2))
fnc (Or f1 f2) = distr (fnc f1) (fnc f2)
fnc a = fnc (fnn a)

--Función auxiliar para Ejercicio 2
distr :: Prop -> Prop -> Prop
distr (Cons a) (Cons b) = (Or (Cons a) (Cons b))
distr (Cons a) (Var p) = (Or (Cons a) (Var p))
distr (Var p) (Cons a) = (Or (Var p) (Cons a))
distr (Var p) (Var q) = (Or (Var p) (Var q))
distr (Not (Var p)) (Var q) = (Or (Not (Var p)) (Var q))
distr (Var p) (Not (Var q)) = (Or (Var p) (Not (Var q)))
distr (Not (Var p)) (Not (Var q)) = (Or (Not (Var p)) (Not (Var q)))
distr f1 (And f21 f22) = (And (distr f1 f21) (distr f1 f22))
distr (And f11 f12) f2 = (And (distr f11 f2) (distr f12 f2))
distr (Or f11 f12) f2 = (Or (Or f11 f12) f2)
distr f1 (Or f21 f22) = (Or f1 (Or f21 f22))

-- (Or (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q"))))

-- (And (Or (Or (Var "p") (Var "q"))(And (Not (Var "p")) (Not (Var "q")))) (Or (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q")))))

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (Cons a) = [[(Cons a)]]
clausulas (Var p) = [[(Var p)]]
-- clausulas (Or (Var p) (Var q)) = if (Var p) == (Var q) then [[(Var p)]] else [[(Var p), (Var q)]]
-- clausulas (Or (Not(Var p)) (Not(Var q))) = if (Not(Var p)) == (Not(Var q)) then [[(Not(Var p))]] else [[(Not(Var p)), (Not(Var q))]]
clausulas (Or f1 f2) = if f1 == f2 then [quitadisyuncion f1] else [quitadisyuncion f1 ++ quitadisyuncion f2]
clausulas (And f1 f2) = clausulas f1 ++ clausulas f2

--Función auxiliar para Ejercicio 1 (clausulas)
quitadisyuncion :: Prop -> [Prop]
quitadisyuncion (Cons a) = [(Cons a)]
quitadisyuncion (Var p) = [(Var p)]
quitadisyuncion (Not (Var p)) = [(Not (Var p))]
quitadisyuncion (Or f1 f2) = [f1, f2]


--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion [] [] = []
resolucion [] c = c
resolucion c [] = c
resolucion [f1] [(Not f2)] = if f1 == f2 then [] else [f1, f2]
-- resolucion [f1] [f2] = [f1, f2]
resolucion [f1] xs = hayContraria f1 xs
-- resolucion (x:xs) (y:ys) = if sonContrarias x y then xs ++ ys else resolucion xs (y:ys)
-- resolucion (x:xs) ys = if (longitud (hayContraria x ys)) < longitud ys then xs ++ hayContraria x ys else x:resolucion xs ys
-- resolucion (x:xs) ys = if (longitud (hayContraria x ys)) < longitud ys then construyeListaSinRepeticion construyeSinRepeticion (regresaCabeza (xs ++ hayContraria x ys)) xs ++ hayContraria x ys else construyeListaSinRepeticion construyeSinRepeticion (regresaCabeza (x:resolucion xs ys)) (x:resolucion xs ys)
resolucion (x:xs) ys = if (longitud (hayContraria x ys)) < longitud ys then construyeListaSinRepeticion construyeSinRepeticion (regresaCabeza (xs ++ hayContraria x ys)) xs ++ hayContraria x ys else construyeListaSinRepeticion construyeSinRepeticion x (x:resolucion xs ys)

--Función auxiliar para Ejercicio 2 (Devuelve la lista de literales sin la literal contraria del primer argumento)
hayContraria :: Literal -> [Literal] -> [Literal]
hayContraria _ [] = error "No hay literal contraria en una lista vacía"
hayContraria a [b] = if sonContrarias a b then [] else [b]
hayContraria a (x:xs) = if sonContrarias a x then xs else x:hayContraria a xs

--Función auxiliar para Ejercicio 2
sonContrarias :: Literal -> Literal -> Bool
sonContrarias p1 (Not p2) = if p1 == p2 then True else False
sonContrarias (Not p1) p2 = if p1 == p2 then True else False
sonContrarias _ _ = False

-- --Función auxiliar para Ejercicio 2
-- eliminaRepetidos :: (Eq a) => [a] -> [a]
-- eliminaRepetidos [] = []
-- eliminaRepetidos [b] = [b]
-- eliminaRepetidos [a,b] = if a == b then [a] else [a,b]
-- eliminaRepetidos (x:y:ys) = if x == y then eliminaRepetidos (x:ys) else x:eliminaRepetidos (y:ys)


--Función auxiliar para Ejercicio 2
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

------------------------------------------------------

--Función auxiliar para Ejercicio 2
construyeSinRepeticion :: (Eq a) => a -> [a] -> [a]
construyeSinRepeticion a [] = [a]
construyeSinRepeticion a [b] = if a == b then [a] else [a,b]
construyeSinRepeticion a (x:xs) = if elementoDe a (x:xs) == True then construyeSinRepeticion x xs else a:x:xs

--Función auxiliar para Ejercicio 2
construyeListaSinRepeticion :: (a -> [a] -> [a]) -> a -> [a] -> [a]
construyeListaSinRepeticion f a [] = f a []
construyeListaSinRepeticion f a (x:xs) = f a (construyeListaSinRepeticion f x xs)

--Función auxiliar para Ejercicio 2
regresaVariables :: Prop -> [String]
regresaVariables (Cons _) = []
regresaVariables (Var a) = [a]
regresaVariables (Not a) = regresaVariables a
regresaVariables (Or a b) = regresaVariables a ++ regresaVariables b
regresaVariables (And a b) = regresaVariables a ++ regresaVariables b
regresaVariables (Impl a b) = regresaVariables a ++ regresaVariables b
regresaVariables (Syss a b) = regresaVariables a ++ regresaVariables b

--Función auxiliar para Ejercicio 2
regresaCabeza :: [a] -> a
regresaCabeza [] = error "No se puede obtener la cabeza de una lista vacía."
regresaCabeza (x:_) = x

--Función auxiliar para Ejercicio 2
elementoDe :: (Eq a) => a -> [a] -> Bool
elementoDe _ [] = False
elementoDe x [y] = if x == y then True else False
elementoDe y (x:xs) = if y == x then True else elementoDe y xs

---------------------------------------------------------------------------




{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente [] _ = False
hayResolvente _ [] = False
hayResolvente (x:xs) (y:ys) = if sonContrarias x y then True else (hayResolvente (x:xs) ys) || (hayResolvente xs (y:ys))


--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion f1 = resolvente (clausulas (fnc f1))
-- saturacion f1
--   | hayClausulaVacia (funcionR (clausulas (fnc f1))) = False
--   | funcionR (clausulas (fnc f1)) == clausulas (fnc f1) = True
--   | otherwise = saturacion (funcionR (funcionR (clausulas (fnc f1))))

--Devuelve todos las resoluciones de la lista de clausulas recibida concatenada con la lista original
funcionR :: [Clausula] -> [Clausula]
funcionR [] = []
funcionR [c1] = [c1]
funcionR [c1, c2] = if hayResolvente c1 c2 then [c1,c2] ++ [resolucion c1 c2] else [c1,c2]
-- funcionR (x:y:xs) = if hayResolvente x y then [x] ++ [resolucion x y] ++ funcionR (y:xs) else funcionR [x] ++ (y:xs)
-- funcionR xs = xs ++ [resolucion x y | x <- xs, y <- xs, x /= y, hayResolvente x y]
-- funcionR (x:xs) = (x:xs) ++ (construyeListaSinRepeticion construyeSinRepeticion x [resolucion x y | x <- (x:xs), y <- (x:xs), x /= y, hayResolvente x y])
funcionR (x:xs) = construyeListaSinRepeticion construyeSinRepeticion x ((x:xs) ++ ([resolucion x y | x <- (x:xs), y <- (x:xs), x /= y, hayResolvente x y]))

hayClausulaVacia :: [Clausula] -> Bool
hayClausulaVacia [] = False
hayClausulaVacia [a] = if a == [] then True else False 
hayClausulaVacia (x:xs) = if x == [] then True else hayClausulaVacia xs

resolvente :: [Clausula] -> Bool
resolvente xs
  | hayClausulaVacia (funcionR xs) = False
  | funcionR xs == xs = True
  | otherwise = resolvente (funcionR xs)
-- [[Var "p", Var "q"],[Not (Var "r"), Var "s"],[Var "t", Not (Var "u")]]
-- [[ Not (Var "p"), Var "p", Var "q" ], [ Not (Var "p"), Var "p", Var "q" ], [ Var "p", Var "q", Not (Var "p") ], [ Var "p", Var "q", Not (Var "q") ]]

-- [[Var "p", Var "q"], [Not (Var "p"), Var "r"]]

-- [[Var "p", Var "q"], [Not (Var "p"), Var "r"], [Not (Var "q"), Var "s"]]

