module Practica03 where

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]

-- Ejercicio 1
-- Devuelve una lista con todas las variables que aparecen en una proposición lógica, sin repetir ninguna.
variables :: Prop -> [String]
variables (Var p) = [p]
variables (Cons _) = []
variables (Not p) = variables p
variables (And p q) = eliminarDuplicados (variables p ++ variables q)
variables (Or p q)  = eliminarDuplicados (variables p ++ variables q)
variables (Impl p q) = eliminarDuplicados (variables p ++ variables q)
variables (Syss p q) = eliminarDuplicados (variables p ++ variables q)

-- Ejercicio 2
-- Evalúa una proposición lógica según un estado dado.
-- Un estado es una lista con las variables que son verdaderas.
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var x) i = pertenece x i
interpretacion (Cons x) _ = x
interpretacion (Not x) i = not (interpretacion x i)
interpretacion (And x y) i = interpretacion x i && interpretacion y i
interpretacion (Or x y) i = interpretacion x i || interpretacion y i
interpretacion (Impl x y) i = not (interpretacion x i) || interpretacion y i
interpretacion (Syss x y) i = interpretacion (Impl x y) i && interpretacion (Impl y x) i

-- Ejercicio 3
-- Genera todas las combinaciones posibles de valores de verdad para las variables de una proposición.
estadosPosibles :: Prop -> [Estado]
estadosPosibles x = conjuntoPotencia (variables x) 

-- Ejercicio 4
-- Devuelve los estados donde una proposición es verdadera.
modelos :: Prop -> [Estado]
modelos x = [e | e <- estadosPosibles x, interpretacion x e]

-- Ejercicio 5
-- Verifica si dos proposiciones son equivalentes, es decir, si tienen el mismo valor de verdad en todos los estados posibles.
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p q = siempreEsValido (Syss p q) (conjuntoPotencia (eliminarDuplicados (variables p ++ variables q)))

-- Ejercicio 6
-- Comprueba si una proposición es una tautología, es decir, si es verdadera en todos los estados posibles.
tautologia :: Prop -> Bool
tautologia f = siempreEsValido f (estadosPosibles f)

-- Ejercicio 7
-- Verifica si una proposición es consecuencia lógica de un conjunto de proposiciones.
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica xs x = tautologia (Impl (listaConjuncion xs) (x)) 

--Funciones auxiliares
conjuntoPotencia :: [a] -> [[a]]
-- Genera todas las posibles sublistas (subconjuntos) de una lista dada.
-- Es decir, el conjunto potencia de una lista.
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

eliminarDuplicados :: Eq a => [a] -> [a]
-- Elimina los elementos repetidos de una lista, preservando el primero que aparece.
-- Si un elemento ya está más adelante en la lista, se omite.
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = if (pertenece x xs) then (eliminarDuplicados xs) else (x : eliminarDuplicados xs)

pertenece :: Eq a => a -> [a] -> Bool
-- Devuelve True si el elemento dado está presente en la lista.
-- Equivalente a la función estándar `elem`, pero implementada manualmente.
pertenece _ [] = False
pertenece x (y:ys) = if (x == y) then (True) else (pertenece x ys)

siempreEsValido :: Prop -> [Estado] -> Bool
-- Comprueba si una proposición es verdadera en todos los estados de una lista.
-- Si encuentra un estado en que la proposición es falsa, devuelve False.
siempreEsValido _ [] = True
siempreEsValido f (x:xs) = if (interpretacion f x) then (siempreEsValido f xs) else (False)

listaConjuncion :: [Prop] -> Prop
-- Toma una lista de proposiciones y las combina todas mediante el conectivo "y" (∧), formando una sola gran conjunción.
-- Se usa en `consecuenciaLogica` para representar la conjunción de premisas.
listaConjuncion [] = Cons True
listaConjuncion [x] = x
listaConjuncion (x:xs) = And x (listaConjuncion xs)