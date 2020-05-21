import Text.Show.Functions()
import Data.Char
import Data.List (genericLength)

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type AutoBool = Auto -> Bool
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show


{-Saber el costo de reparación de un auto
si la patente tiene 7 dígitos, es $ 12.500
si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
que es $ 3.000 * la longitud para las patentes que terminen en 4
o $ 20.000 para el resto de las patentes
de lo contrario, se le cobra $ 15000
-}

autoDeTomy :: Auto
autoDeTomy = Auto  "DJV214"  [ 0.5, 0.1, 0, 0.2 ] 1500 90 (10, 2, 2014) 

autoDeAlan :: Auto
autoDeAlan = Auto  "AA7664NB"  [ 0.6, 0.1, 0, 0.2 ] 1500 90 (5, 1, 2020) 

--Punto 1
tamañoPatente :: Auto -> Int
tamañoPatente unAuto= length (patente unAuto)

saberCosto :: Auto -> Int 
saberCosto unAuto 
            | (estaEntreDJyNB unAuto) = calculoPatental unAuto 
            | tamañoPatente unAuto > 7 = 12500
            | otherwise = 15000


empiezaConEaM :: AutoBool
empiezaConEaM unAuto = elem (primeraLetra unAuto) ['E' .. 'M']

empiezaConN :: AutoBool
empiezaConN unAuto = elem (take 2 (patente unAuto)) ["NA" , "NB"]

empiezaConD ::  AutoBool
empiezaConD unAuto = (primeraLetra unAuto) == 'D' && elem (segundaLetra unAuto) [ 'J' .. 'Z'] 

segundaLetra :: Auto -> Char
segundaLetra unAuto = head (tail (patente unAuto))

primeraLetra :: Auto -> Char
primeraLetra unAuto = head (patente unAuto)

estaEntreDJyNB :: AutoBool
estaEntreDJyNB unAuto=  empiezaConEaM unAuto  || empiezaConN unAuto  || empiezaConD unAuto
            
terminaEn4 :: AutoBool
terminaEn4 unAuto = last (patente unAuto) == '4'

calculoPatental:: Auto -> Int
calculoPatental unAuto
        | terminaEn4 unAuto = 3000 * (tamañoPatente unAuto)
        | otherwise = 20000

-- Parte 1) Auto peligroso (integrante a)
--Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de la primera llanta es mayor a 0.5

esAutoPeligroso :: AutoBool
esAutoPeligroso = (> 0.5).head.desgasteLlantas 


--Parte 2) Necesita revisión (integrante b)
--Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último arreglo fue realizado en el año 2015 ó antes.

necesitaRevision :: AutoBool 
necesitaRevision= (>= 2015).anio.ultimoArreglo 

devuelveString :: Auto -> String
devuelveString unAuto 
    |  necesitaRevision unAuto = "No Necesita revision"
    |  otherwise = "Necesita revision"
