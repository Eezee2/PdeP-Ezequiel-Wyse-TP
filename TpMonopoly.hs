import Text.Show.Functions()
--import Data.List (genericLength)

main :: IO ()
main = return ()

data Jugador = UnJugador {
  nombre :: String,
  cantidadDinero :: Int,
  tactica :: String,
  propiedades ::[Propiedad],
  acciones :: [Accion]
} deriving (Show)

type Propiedad = (String,Int)
type Accion = (Jugador->Jugador)

suTactica::Jugador->String
suTactica (UnJugador _ _ laTactica _ _) = laTactica

carolina:: Jugador
carolina = UnJugador "Carolina" 500 "Accionista" [("",0)] [pasarPorElBanco]

manuel:: Jugador
manuel = UnJugador "Manuel" 500 "Oferente singular" [("",0)] [pasarPorElBanco]

pasarPorElBanco:: Jugador->Jugador
pasarPorElBanco unJugador = ((modificarDinero 40).(modificarTactica "Comprador compulsivo")) unJugador

enojarse::Jugador->Jugador
enojarse unJugador = ((modificarDinero 50).(agregarAccion gritar)) unJugador

gritar::Jugador->Jugador
gritar unJugador = unJugador {nombre= "AHHHH" ++ nombre unJugador}

esGanador::Jugador->Bool
esGanador unJugador = any (==(suTactica unJugador)) ["Oferente singular","Accionista"]

subastar::Jugador->Propiedad->Jugador
subastar unJugador propiedadSubastada  | esGanador unJugador = ganarPropiedad unJugador propiedadSubastada
                                       | otherwise = unJugador

ganarPropiedad::Jugador->Propiedad->Jugador
ganarPropiedad unJugador propiedadSubastada = (modificarDinero (-(snd propiedadSubastada)).(agregarPropiedad propiedadSubastada)) unJugador

cobrarAlquileres::Jugador->Jugador
cobrarAlquileres unJugador= modificarDinero (totalACobrar (map snd (propiedades unJugador))) unJugador

totalACobrar:: [Int]->Int
totalACobrar listaValoresDePropiedades = (cuantasPropiedades (<150) listaValoresDePropiedades)*10 + (cuantasPropiedades (>=150) listaValoresDePropiedades)*20

cuantasPropiedades::(Int->Bool)->[Int]->Int
cuantasPropiedades condicion lista = (length (filter condicion lista))

pagarAAccionistas::Jugador->Jugador
pagarAAccionistas unJugador  | (suTactica unJugador) == "Accionista" = modificarDinero 200 unJugador
                             | otherwise                             = modificarDinero (-100) unJugador

hacerBerrinchePor::Propiedad->Jugador->Jugador
hacerBerrinchePor propiedadNecesitada unJugador = puedeComprarla propiedadNecesitada (elBerrinche unJugador)

puedeComprarla::Propiedad->Jugador->Jugador
puedeComprarla propiedad unJugador | leAlcanza unJugador propiedad = ganarPropiedad unJugador propiedad
                                   | otherwise = hacerBerrinchePor propiedad unJugador

leAlcanza::Jugador->Propiedad->Bool
leAlcanza unJugador propiedad=  (snd propiedad) <= (cantidadDinero unJugador)

elBerrinche::Jugador->Jugador
elBerrinche unJugador= ((modificarDinero 10).(agregarAccion gritar)) unJugador

ultimaRonda:: Jugador->Accion
ultimaRonda unJugador= foldr1 ((.)) (acciones unJugador)

juegoFinal::Jugador->Jugador->Jugador
juegoFinal unJugador otroJugador |  cuantoDineroFinal unJugador > cuantoDineroFinal otroJugador = unJugador
                                 | otherwise = otroJugador

cuantoDineroFinal:: Jugador->Int
cuantoDineroFinal unJugador= (cantidadDinero.aplicarAcciones) unJugador

aplicarAcciones::Jugador->Jugador
aplicarAcciones unJugador=(ultimaRonda unJugador) unJugador

modificarDinero::Int->Jugador->Jugador
modificarDinero monto unJugador = unJugador {cantidadDinero = ((cantidadDinero unJugador) +monto)}

modificarTactica:: String->Jugador->Jugador
modificarTactica nuevaTactica unJugador = unJugador {tactica= nuevaTactica}

agregarAccion:: Accion->Jugador->Jugador
agregarAccion nuevaAccion unJugador = unJugador {acciones= acciones unJugador ++ [nuevaAccion]}

agregarPropiedad:: Propiedad->Jugador->Jugador
agregarPropiedad nuevaPropiedad unJugador = unJugador {propiedades = propiedades unJugador ++ [nuevaPropiedad]}



--stack ghci D:\Descargas\Facu2020\Paradigmas\Tps\TP3\probando.hs
