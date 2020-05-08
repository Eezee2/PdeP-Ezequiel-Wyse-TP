import Text.Show.Functions()
--import Data.List (genericLength)

main :: IO ()
main = return ()

data Jugador = UnJugador {
  nombre :: String,
  cantidadDinero :: Int,
  tactica :: String,
  propiedades ::[Propiedad],
  acciones :: [(Jugador->Jugador)]
} deriving (Show)

type Propiedad = (String,Int)

suTactica::Jugador->String
suTactica (UnJugador _ _ laTactica _ _) = laTactica

carolina:: Jugador
carolina = UnJugador "Carolina" 500 "Accionista" [("",0)] [pasarPorElBanco]

manuel:: Jugador
manuel = UnJugador "Manuel" 500 "Oferente singular" [("",0)] [pasarPorElBanco]

pasarPorElBanco:: Jugador->Jugador
pasarPorElBanco unJugador = unJugador {cantidadDinero = cantidadDinero unJugador +40, tactica= "Comprador compulsivo"}

enojarse::Jugador->Jugador
enojarse unJugador = unJugador {cantidadDinero = cantidadDinero unJugador +50, acciones= acciones unJugador ++ [gritar]}

gritar::Jugador->Jugador
gritar unJugador = unJugador {nombre= "AHHHH" ++ nombre unJugador}


subastar::Jugador->Propiedad->Jugador
subastar unJugador propiedadSubastada  | (suTactica unJugador) == "Oferente singular" = ganarPropiedad unJugador propiedadSubastada
                                       | (suTactica unJugador) == "Accionista" = ganarPropiedad unJugador propiedadSubastada 
                                       | otherwise = unJugador

ganarPropiedad::Jugador->Propiedad->Jugador
ganarPropiedad unJugador propiedadSubastada = unJugador {cantidadDinero = cantidadDinero unJugador -(snd propiedadSubastada), propiedades = propiedades unJugador ++ [propiedadSubastada]}

cobrarAlquileres::Jugador->Jugador
cobrarAlquileres unJugador= unJugador {cantidadDinero = cantidadDinero unJugador + propiedadBarata (map snd (propiedades unJugador))}

propiedadBarata:: [Int]->Int
propiedadBarata listaValoresDePropiedades = (cuantasPropiedadesBaratas listaValoresDePropiedades)*10 + (cuantasPropiedadesCaras listaValoresDePropiedades)*20

cuantasPropiedadesBaratas::[Int]->Int
cuantasPropiedadesBaratas lista = (length (filter (<150) lista))

cuantasPropiedadesCaras::[Int]->Int
cuantasPropiedadesCaras lista = (length (filter (>=150) lista))

pagarAAccionistas::Jugador->Jugador
pagarAAccionistas unJugador  | (suTactica unJugador) == "Accionista" = unJugador {cantidadDinero = cantidadDinero unJugador +200}
                             | otherwise                             = unJugador {cantidadDinero = cantidadDinero unJugador -100}


--sumar10::Jugador->Jugador
--sumar10 unJugador = unJugador {cantidadDinero = cantidadDinero unJugador +10}
--sumar20::Jugador->Jugador
--sumar20 unJugador = unJugador {cantidadDinero = cantidadDinero unJugador +20}

--subastar::Jugador->Propiedad->Jugador
--subastar (UnJugador _ _ "Oferente singular" _ _)  = ganarPropiedad unJugador propiedadSubastada
--subastar (UnJugador _ _ "Accionista" _ _)         = ganarPropiedad unJugador propiedadSubastada
--subastar (UnJugador _ _ _ _ _)                    = unJugador





--stack ghci D:\Descargas\Facu2020\Paradigmas\Tps\TP3\TpMonopoly.hs