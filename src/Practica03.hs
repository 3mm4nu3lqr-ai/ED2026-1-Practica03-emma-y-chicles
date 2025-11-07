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
variables :: Prop -> [String]
variables (Var p) = [p]
variables (Cons _) = []
variables (Not p) = variables p
variables (And p q) = eliminarDuplicados (variables p ++ variables q)
variables (Or p q)  = eliminarDuplicados (variables p ++ variables q)
variables (Impl p q) = eliminarDuplicados (variables p ++ variables q)
variables (Syss p q) = eliminarDuplicados (variables p ++ variables q)

-- Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var x) i = pertenece x i
interpretacion (Cons x) _ = x
interpretacion (Not x) i = not (interpretacion x i)
interpretacion (And x y) i = interpretacion x i && interpretacion y i
interpretacion (Or x y) i = interpretacion x i || interpretacion y i
interpretacion (Impl x y) i = not (interpretacion x i) || interpretacion y i
interpretacion (Syss x y) i = interpretacion (Impl x y) i && interpretacion (y x) i

-- Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles x = conjuntoPotencia (variables x) 

-- Ejercicio 4
modelos :: Prop -> [Estado]
modelos x = [e | e <- estadosPosibles p, interpretacion p e]

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p q = esVerdaderaEnTodos (Syss p q) (conjuntoPotencia (eliminarDuplicados (variables p ++ variables q)))

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia f = siempreEsValido f (estadosPosibles f)

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined

--Funciones auxiliares
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = if (pertenece x xs) then (eliminarDuplicados xs) else (x : eliminarDuplicados xs)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = if (x == y) then (True) else (pertenece x ys)

siempreEsValido :: Prop -> [Estado] -> Bool
siempreEsValido _ [] = True
siempreEsValido f (x:xs) = if (interpretacion f x) then (siempreEsValido f xs) else (False)

listaConjuncion :: [Prop] -> Prop
listaConjuncion [] = Cons True
listaConjuncion [x] = x
listaConjuncion (x:xs) = And x (listaConjuncion xs)