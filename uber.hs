import Text.Show.Functions()


data Persona = Persona{
edad:: Int,
listaElem :: [String],
cantidadDeExperiencia :: Int
}deriving(Show)

data Criatura = Criatura{
peligroso:: Int,
deshacerse:: [Debilidad]
}deriving(Show)

type Debilidad = Persona -> Bool

{-El siempredetras: la peligrosidad de esta criatura legendaria es 0, ya que no le hace nada a la 
persona que está acechando, es tan inofensivo que nunca nadie pudo afirmar que estaba siendo acechado. 
Sin embargo, no hay nada que se pueda hacer para que te deje en paz.
-}

siempredetras :: Criatura
siempredetras = Criatura 0 []

gnomos:: Criatura
gnomos = Criatura 1024 [tieneItem "Soplar"]

fantasma :: Criatura
fantasma = Criatura 60 [menosDe13 , tieneItem "disfraz oveja"]

cambiarPeligrosidad :: Int -> Criatura -> Criatura
cambiarPeligrosidad n unaCriatura = unaCriatura{peligroso = n}

cambiarcantidadDeExperiencia:: Int -> Persona -> Persona
cambiarcantidadDeExperiencia n unaPersona = unaPersona{cantidadDeExperiencia = cantidadDeExperiencia unaPersona + n}

juan :: Persona
juan = Persona 12 ["soplador", "disfraz oveja"] 100


{-Los gnomos: individualmente son inofensivos, pero se especializan en atacar en grupo. 
La peligrosidad es 2 elevado a la cantidad de gnomos agrupados. Una persona puede deshacerse de un 
grupo de gnomos si tiene un soplador de hojas entre sus ítems.
-}

tieneItem:: String -> Persona -> Bool
tieneItem elemento unaPersona = elem(elemento) (listaElem unaPersona)

menosDe13 :: Debilidad
menosDe13 unaPersona = (<13).edad $ unaPersona

{-Los fantasmas: se categorizan del 1 al 10 dependiendo de qué tan poderosos sean, y el nivel de peligrosidad es 
esa categoría multiplicada por 20. Cada fantasma tiene un asunto pendiente distinto, con lo cual se debe indicar para cada uno qué tiene que cumplir
 la persona para resolver su conflicto.
-}


{-Hacer que una persona se enfrente a una criatura, que implica que si esa persona puede deshacerse de ella gane tanta experiencia 
como la peligrosidad de la criatura, o que se escape (que le suma en 1 la experiencia, porque de lo visto se aprende) en caso de que no pueda deshacerse de ella.-}

enfrentamiento ::  Persona -> Criatura -> Persona
enfrentamiento  unaPersona unaCriatura 
                    | desahcerseCriat (deshacerse unaCriatura) unaPersona= cambiarcantidadDeExperiencia (peligroso unaCriatura) unaPersona
                    | otherwise = cambiarcantidadDeExperiencia (1) unaPersona

desahcerseCriat:: [Debilidad] -> Persona -> Bool
desahcerseCriat listaDeb unaPersona = not.any(False==). map(tieneDebilidad unaPersona) $ listaDeb 


tieneDebilidad:: Persona ->  Debilidad -> Bool
tieneDebilidad unaPersona unaDebilidad = unaDebilidad unaPersona



--Determinar cuánta experiencia es capaz de ganar una persona luego de enfrentar sucesivamente a un grupo de criaturas.

sumaExperiencia :: Persona->Int->Persona
sumaExperiencia unaPersona cantExperiencia = unaPersona{cantidadDeExperiencia = cantidadDeExperiencia unaPersona + cantExperiencia}

experienciaGanada ::  Persona-> [Criatura] -> Int
experienciaGanada  unaPersona  criaturas = sum.map (cantidadDeExperiencia.enfrentamiento unaPersona)  $ criaturas


