module Library where
import PdePreludat
import Data.Char (isLower)

type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

-- 1) Saber la calificación de un canción, que equivale a la cantidad de letras minúsculas 
-- (sin espacios, números ni caracteres especiales) de la canción, más 10.

calificacion :: Cancion -> Number
calificacion = (+10) . length . filter esMinuscula

-- Version usando mayor composicion
calificacion' :: Cancion -> Number
calificacion' = (+10) . length . filter (`elem` ['a'..'z'])

-- Version usando mayor composicion (mas reducida aun)
calificacion'' :: Cancion -> Number
calificacion'' = (+10) . length . filter isLower

esMinuscula :: Char -> Bool
esMinuscula letra = letra `elem` ['a'..'z']

esMinuscula'' :: Char -> Bool
esMinuscula'' = isLower 

--esMinuscula' :: Char -> Bool
--esMinuscula' = `elem` ['a'..'z']

-- 2) Averiguar si es exitoso un artista, lo que sucede cuando la suma de las calificaciones
-- buenas de las canciones de un artista es mayor a 50 (son buenas las que tienen calificacion mayor a 20)

esExitoso :: Artista -> Bool
esExitoso = (>50) . sum . map calificacion . filter esBuenaCancion . canciones 
-- 1ero. Filtro las canciones buenas
-- 2dos. Hago una lista de sus calificaciones
-- 3ero. Hago una sumatoria de esas calificaciones
-- 4rto. Evaluo si dicha sumatoria es > 50

esBuenaCancion :: Cancion -> Bool
esBuenaCancion = (>20) . calificacion

-- 3) Obtener todos los artistas exitosos, a partir de un conjunto de artistas.

artistasExitosos :: [Artista] -> [Artista]
artistasExitosos = filter esExitoso

-- 4) Hacer todo lo anterior en una función definida en una sola línea, sin definir funciones auxiliares

artistasExitosos' :: [Artista] -> [Artista]
artistasExitosos' = filter ((>50) . sum . map ((+10) . length . filter (`elem` ['a'..'z'])) . filter ((>20) . calificacion) . canciones)