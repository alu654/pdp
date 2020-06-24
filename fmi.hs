import Text.Show.Functions()

data Pais = Pais{
ingresoPercaPita :: Float,
sectorPúblico :: Float,
sectorPrivado:: Float,
recursosNaturales ::[String],
deuda :: Float
}deriving (Show)

-- es de 4140 u$s, la población activa del sector público es de 400.000, 
--la población activa del sector privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.

namibia :: Pais
namibia = Pais 4140 4000000 650000 ["Mineria","Ecoturismo"] 50000000

--prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)


cambiarDeuda :: Float -> Pais -> Pais
cambiarDeuda n unPais = unPais{deuda = n}

cambiarPerCapita :: Float -> Pais -> Pais
cambiarPerCapita n unPais = unPais{deuda = n}

cambiarSectorPublico :: Float -> Pais -> Pais
cambiarSectorPublico n unPais = unPais{sectorPúblico = n}

cambiarSectorPrivado :: Float -> Pais -> Pais
cambiarSectorPrivado n unPais = unPais{sectorPrivado = n}

prestamo :: Pais -> Float-> Pais
prestamo unPais n = cambiarDeuda (deuda unPais + n * 1.5) unPais

{-reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en
 el sector público y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

-}

reduccion :: Float -> Pais -> Pais
reduccion n unPais |sonMasDe100 unPais = cambiarSectorPublico (sectorPúblico unPais - n) . cambiarPerCapita(ingresoPercaPita unPais - n*0.2)$ unPais
                   |otherwise = cambiarSectorPublico (sectorPúblico unPais - n) . cambiarPerCapita(ingresoPercaPita unPais - n*0.15)$ unPais

sonMasDe100 :: Pais -> Bool
sonMasDe100  = (>100).ingresoPercaPita 



{-darle a una empresa afín al FMI la explotación de alguno de los recursos naturales,
 esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. 
 No considerar qué pasa si el país no tiene dicho recurso.
-}


vincentin :: String -> Pais -> Pais
vincentin recursoASacar unPais = cambiarDeuda (deuda unPais - 2000000) . eliminarUnRecurso recursoASacar  $ unPais


eliminarUnRecurso :: String -> Pais -> Pais
eliminarUnRecurso  recurso unPais = unPais{recursosNaturales = filter(/= recurso).recursosNaturales $ unPais}

{-establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita 
multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. 
Evitar la repetición de código.-}


blindaje :: Pais -> Pais
blindaje unPais =  cambiarDeuda (deuda unPais + pbi unPais) . cambiarSectorPublico(sectorPúblico unPais - 500) $ unPais

pbi :: Pais-> Float
pbi unPais = ingresoPercaPita unPais * (sectorPrivado unPais + sectorPúblico unPais )

type Receta= Pais -> Pais


modelarReceta :: Receta
modelarReceta unPais = cambiarDeuda (deuda unPais + 2000000) . eliminarUnRecurso "Mineria"  $ unPais


-- Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.


paisesQueZafan :: [Pais]-> [Pais]
paisesQueZafan unPais = filter (any(=="Petroleo").recursosNaturales) $ unPais

--Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.

sumaDeDeuda :: [Pais] -> Float
sumaDeDeuda unPais = sum.map deuda $ unPais

