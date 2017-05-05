
import Data.List
import Data.Char
import System.IO


--Hay que mencionar que el archivo de frecuencias de la RAE utilizado ha sido uno sin tildes en sus palabras, ya que producía funcionamientos erróneos en la práctica. El archivo utilizado está adjuntado en el zip.


--Modo de funcionamiento: ejecutas el procedimiento "main" y desde ahí ya se muestra un menu con las opciones posibles a realizar en el que para salir debes introducir un 0(o cualquier numero mayor que 6).

 --traducciones = [ a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z]
 --traducciones = ['o','n','m','l','k','j','i','h','g','f','z','y','x','w','v','u','t','s','r','q','p','e','d','c','b','a']
 --traducciones = ['z','y','x','w','v','j','i','h','g','f','e','d','c','b','a','u','t','s','r','q','p','o','n','m','l','a']
 --Solucion del texto "zyxwvjihgfedcbautsrqponmla"
 
 
 -- En esta práctica he intentado crear una serie de herramientas para poder desencriptar un texto cifrado.Se aportan algunos procedimientos que ayudan a la posible desencriptación del texto crifrado por parte del usuario,
 -- aunque también tiene métodos que desencriptan gran parte del texto(no totalmente) de forma automatizada. El usuario podrá ver el texto codificado tal cual, se podrá ver el texto traducido en función
 -- de la traducción actual definida, se podrá cambiar la traducción actual y, para el texto dado por el profesor, se podrá ver cuántas letras están bien asignadas en la codificación actual definida. Las traducciones usadas 
 -- en esta práctica son arrays de carácteres, de forma que una posible traducción es el mismo abecedario "abcdefghijklmnopqrstuvwxyz"(la letra en la posicion 0 del array es la que le correspondería a la A, y la letra 
 -- en la posicion 25 del array correspondería a la traduccion de la Z) o cualquier otra permutacion o variación
 -- Dicho esto, hay tres formas de intentar encontrar una posible correspondencia(solución) de letras:
 
 
 -- 1ª: Analizando la apariencia de letras en las palabras de la RAE y en el texto, de modo que si, por ejemplo, la letra que mas aparece es la K y la letra que mas aparece en el diccionario de la RAE
 -- 	es la A, pues , en teoría, la K se corresponde con la A. Esto se hace letra a letra, y el método no es bueno, puesto que sólo consigue decodificar correctamente 4 letras en el texto codificado dado. 
 
 -- 2ª: Realizando permutaciones en una traduccion dada inicialmente, quedándose con la permutación de letras que derive en un texto decodificado con más palabras recogidas en la RAE.
 --		De forma más detallada: de parte de una posible solucion inicial(por defecto "abcdefghijklmnopqrstuvwxyz") que se podrá elegir y se va realizando una permutación de las letras, es decir, la primera permutacion 
 --		utilizando "abcdefghijklmnopqrstuvwxyz" por defecto es "bcdefghijklmnopqrstuvwxyza". El método realizará 26 permutaciones y se quedará con la "solución" que, una vez decodificado el texto con dicha decodifación, obtenga mayor 
 --		número de palabras recogidas en la RAE. En este caso, utilizando por defecto "abcdefghijklmnopqrstuvwxyz", obtiene un resultado que tiene correctamente 1 letra nada más. (Esto podría mejorarse de la siguiente forma: 
 --		en vez de tener como criterio de "mejor solución" el número de palabras que hay en la RAE con esa posible solució, sería obviamente mucho mejor quedarte con la solución que tenga mayor número de palabras correctamente asignadas.
 --		El motivo por el cual no he realizado esto así es porque no sería un método válido para cualquier texto, ya que no tendremos la solución de letras de todos los textos, mientras que de este texto si que ha sido previamente hallada.)
 
 -- 3ª: Este último método de análisis del texto proporcionado es el más eficiente(obtiene el mejor resultado). Este método consiste en lo siguiente: dada una longitud prefijada, se filtran palabras de dicha longitud tanto
 --		del texto como de la RAE y, se coge únicamente la primera palabra de dicha longitud y se la "numera", esto es, redefinir una palabra en forma de numero(por ejemplo, la palabra "hola" se numera "1234", mientras que la palabra
 --     "privacidad" se numera "1234563757"). Una vez tenemos dicha palabra numerada, cogemos las palabras de la RAE de la longitud prefijada y se las numera. Ahora se filtra con las palabras de forma que tengan la misma numeración que
 --     que la palabra del texto escogida. Finalmente, puesto que obtenemos dos palabras idénticas, asumimos que son la misma palabra y se asignan las codificaciones.
 --		En el método declarado en el Main de la práctica lo que se hace es aplicar de forma encadenada este proceso varias veces, en concreto se aplica para palabras de longitud 7, 6, 8,9,10,11 y 12, en ese orden encadenando los resultados obtenidos 
 --		en cada uno de los análisis. De esta forma, obtenemos una asignación de letras con 17 de 26 bien asignadas en un tiempo muy aceptable(alrededor de 0.25 segundos, según el procesador que ejecute el programa).
 --     
 -- Explicado esto, cada función está explicada brevemente por encima de la misma.
 
 
--Funcion que lee de la entrada estándar(teclado) una linea ylo devuelve en forma de entero. Falla si la entrada introducida no es un número
getInt:: IO Int
getInt = do
 x<- getLine
 return (read x::Int)

--Dada una ruta de archivo (FIlePath) lee el archivo y devuelve una lista con las palabras del texto
leeArchivoConWords :: FilePath -> IO [String]
leeArchivoConWords fileName = do
 x <- readFile fileName
 return (words x)

--Dada una ruta de archivo (FilePath) lee el archivo y devuelve el texto en forma de IO String
leeArchivoSinWords :: FilePath -> IO String
leeArchivoSinWords fileName = do
 x <- readFile fileName
 return x
 
--Esta función es equivalente a la funcion getChar. El motivo de la existencia y uso de esta función se debe a que usando getChar, leía el carácter introducido y además el salto de linea('\n'), de modo que creaba fallos en el menú.
leeLetra :: IO Char
leeLetra = do
 z<-getLine
 return (z!!0)
 
 
--Dado un index, una letra y una traduccion, lo que hace esta función es cambiar la letra existente en el index por la letra introducida como parámetro. 
cambiaLetra :: Monad m => Int -> a -> [a] -> m [a]
cambiaLetra index letra t = do
 let x=splitAt index t
 let parte1= fst x
 let y= drop 1 (snd x)
 let parte2=letra:y
 let final=concat [parte1,parte2]
 return final
--Esta función se encarga de la modificación de una letra por otra en la traduccion actual. 
cambiaTraduccion :: [Char] -> IO [Char]
cambiaTraduccion t = do
 putStr "¿Que letra quieres cambiar?"
 x<-leeLetra
 putStr "¿Por que letra quieres cambiarla?"
 y <-leeLetra
 nueva <- cambiaLetra ((ord x)-97) y t 
 return nueva


main = do
 mainAux ['a'..'z'] 
   
mainAux traducciones = do
 putChar '\n'
 putStr "Traducción actual : "
 print traducciones
 print "Que deseas hacer?"
 print "1-Mostrar Texto original"
 print "2-Mostrar Texto segun traducciones actuales"
 print "3-Cambiar traducciones actuales"
 print "4-Analisis del texto"
 print "5-Comprobar el numero de letras correctamente asignadas"
 print "6-Muestra el numero de palabras existentes en la RAE con esta codificacion"
 print "0-Salir"
 x<- getInt
 if x==1 then do
                 muestraTextoOriginal
                 mainAux traducciones
         else if x==2 then do 
                             archivo <- leeArchivoConWords "cod.txt"
                             muestraTexto archivo traducciones
                             mainAux traducciones
                      else if x==0 then return ()
                                   else if x==3 then do
                                                       traduccionesNuevas <- ofreceCambiarTraducciones traducciones
                                                       mainAux traduccionesNuevas
                                                else if x==4 then do
                                                                     traduccionesNuevas <- menuAnalisis
                                                                     mainAux traduccionesNuevas
                                                             else if x==5 then do
                                                                                 putStr "El número de coincidencias es: "
                                                                                 x<-numeroDeCoincidenciasCodificacionCorrecta traducciones
                                                                                 print x
                                                                                 mainAux traducciones
                                                                          else if x==6 then do 
                                                                                              x<-numeroPalabrasExistentes traducciones
                                                                                              putStr "El numero de palabras con esta traduccion es: "
                                                                                              print x
                                                                                              mainAux traducciones
                                                                                       else return ()







															 
menuAnalisis= do
 print "Que tipo de analisis quieres realizar?"
 print "1-Asignando codificacion por maximos"
 print "2-Asignando codificacion por permutacion de una codificacion(a escoger)"
 print "3-Buscando codificacion mediante longitudes de palabras"
 x<- getInt
 if x==1 then do 
                t <- (asignaCodificacionPorMaximos)
                return t
         else if x==2 then do 
                             t <- (menuPermutacion)
                             return t
                      else do
                             t1 <- (buscaCodificacionMedianteLongitudesPalabra ['a'..'z'] 6)							 
                             t2<- (buscaCodificacionMedianteLongitudesPalabra t1 8)
                             t3 <- (buscaCodificacionMedianteLongitudesPalabra t2 9)
                             t4 <- (buscaCodificacionMedianteLongitudesPalabra t3 10)
                             t <- (buscaCodificacionMedianteLongitudesPalabra t4 11)
                             return t
		 
		 
menuPermutacion = do
 print "Aqui puedes elegir entre la codificacion a permutar por defecto(['a'..'z']) o una propia.Este procedimiento puede llegar a tardar varios minutos."
 print "1-Por Defecto"
 print "2-Una Propia(debe contener 26 letras, en caso contrario se volverá a preguntar)"
 x<-getInt
 if x==1 then do 
                resultado <- (buscaCodificacionMediantePermutacion ['a'..'z'] 0)
                return resultado
         else do 
                t <- (pideCodificacion)
                resultado <- (buscaCodificacionMediantePermutacion t 0)
                return resultado
 

 
 
 --Funcion que se encarga de pedir una posible traduccion del texto. Cabe mencionar que dicha traduccion debe estar compuesta por 26 letras, de forma que mientras tenga más o menos letras de 26 seguirá pidiendo una nueva traducción. 
pideCodificacion :: IO String
pideCodificacion = do
 t<-getLine
 if (length t) /= 26 then do
                            print "La codificacion introducida no tiene 26 letras."
                            resultado <-(pideCodificacion)
                            return resultado
                     else return t
 
 
 --Esta función llama a muestraTraducciones, y después a cambiaTraduccion, devolviendo la nueva traduccion cambiada por el usuario.
ofreceCambiarTraducciones :: [Char] -> IO [Char]
ofreceCambiarTraducciones traducciones = do
 muestraTraducciones 0 traducciones
 nuevaTraduccion <- cambiaTraduccion traducciones
 return nuevaTraduccion
 
 
 
--Esta función se encarga de mostrar las actuales traducciones.
muestraTraducciones :: Int -> [Char] -> IO ()
muestraTraducciones letra [] =return ()
muestraTraducciones letra (x:xs) = do
  putStr "La letra "
  putStr (show (chr (97+letra)))
  putStr " se corresponde con " 
  putChar x
  putChar '\n'
  muestraTraducciones (letra+1) xs
  
  
  
  
--Esta funcion muestra el texto original codificado. 
muestraTextoOriginal :: IO ()
muestraTextoOriginal = do
 texto <- leeArchivoSinWords "cod.txt"
 print texto
 return ()
 
 
 
 
-- Esta funcion se encarga de mostrar el texto decodificandolo en funcion de una traduccion dada.Llama a muestraPalabra procesando cada palabra del texto. ys es la traduccion dada
muestraTexto :: [[Char]] -> [Char] -> IO ()
muestraTexto [] ys = return ()
muestraTexto (x:xs) ys = do
 muestraPalabra x ys
 putChar ' '
 muestraTexto xs ys
 
 
 
 
--Esta funcion recibe una palabra y la procesa letra a letra mostrando la dicha letra de forma decodificada en función de la traduccion dada. 
muestraPalabra :: [Char] -> [Char] -> IO (IO ())
muestraPalabra [] ys = return (putChar '\n')
muestraPalabra (x:xs) ys = do
 if (x < 'a') || (x > 'z') then putChar x
             else putChar (ys !! ((ord x)-97))
 
 muestraPalabra xs ys
 
 
 
 
--Esta funcion comentada es la que se encargaría, en teoría, de quitar los acentos de las palabras de la RAE, sin embargo, no funciona bien y no quita los acentos por algún problema ajeno al propio procedimiento. 
--quitaAcentos [] = []
--quitaAcentos (x:xs)
-- | x=='á' = 'a':(quitaAcentos xs)
-- | x=='é' = 'e':(quitaAcentos xs)
-- | x=='í' = 'i':(quitaAcentos xs)
-- | x=='ó' = 'o':(quitaAcentos xs)
-- | x=='ú' = 'u':(quitaAcentos xs)
-- | x=='ü' = 'u':(quitaAcentos xs)
-- | otherwise = x:(quitaAcentos xs)






--Dadas dos listas de letras(pueden ser dos palabras, aunque no necesariamente) se va asignando en la traduccion una letra a otra. Por ejemplo: si recibe la palabra "qvlv" y la palabra "todo", lo que hará esta funcion es
-- asignar que la letra 'q' se corresponde con la letra 't', la 'v' con la 'o' y la 'l' con la 'd'. Devolverá las traducciones cambiadas. 
asignaCodificacion :: Monad m => [Char] -> [a] -> [a] -> m [a]
asignaCodificacion [] [] t = return t
asignaCodificacion (x:xs) (y:ys) t = do
 nuevoT <- (cambiaLetra ((ord x)-97) y t)
 result <- asignaCodificacion xs ys nuevoT
 return result
 
 
 
 
--Esta funcion llama a otra función auxiliar que realizará la numeración de la palabra recibida
numeraPalabra :: [Char] -> [Char]
numeraPalabra xs = numeraPalabra2 xs '1'

--Esta funcion se encarga de numerar la palabra recibida. Para ello, procesa la palabra letra a letra y para cada carácter que es letra(y no numero,obviamente) le asigna el número que se va llevando por parámetro y además para el resto de la palabra
-- cambia la letra por el numero en cuestion. 
numeraPalabra2 :: [Char] -> Char -> [Char]
numeraPalabra2 [] _ = []
numeraPalabra2 (x:xs) n 
 |x >= 'a' && x <= 'z' = n:(numeraPalabra2 (map (\y -> if y==x then n else y) xs) (chr ((ord n)+1)))
 |otherwise = x:(numeraPalabra2 xs n)

 
 
 

--Esta funcion se encarga de calcular coincidencias de palabras mediante la numeracion de las mismas. Recibe una lista de palabras , otra lista de palabras  y lo que hace es numerar cada palabra y quedarse con las que son iguales(realmente coge la primera palabra de la primera lista y luego ya filtra la segunda lista)
calculaCoincidencias:: (Monad m, Eq a) =>[(a, [Char])] -> [(a, [a1])] -> [a1] -> m [a1]
calculaCoincidencias [] rae traducciones = do
 return traducciones
calculaCoincidencias (x:texto) rae traducciones = do
 let filtradas= filter (\y -> (fst x)==(fst y)) rae
 if (length filtradas) > 0 then do
                                      let arg1= snd x
                                      let primeroRAE =  filtradas !! 0
                                      let arg2= snd primeroRAE
                                      traduccionesAux <- (asignaCodificacion arg1 arg2 traducciones)
                                      resultado <- (calculaCoincidencias texto rae traduccionesAux)
                                      return resultado
                               else do
                                      resultado <- (calculaCoincidencias texto rae traducciones)
                                      return resultado
									  
									  
									  
									  
									  
									  
									  
									  
									  
--Esta funcion se encarga de, dada una longitud y una traduccion actual, buscar palabras de la longitud dada en el texto y en la rae y luego encontrar palabras que tengan la misma numeracion.Una vez tengan dos la misma numeracion, se asume que son la misma palabra.
buscaCodificacionMedianteLongitudesPalabra:: [Char] -> Int -> IO [Char]
buscaCodificacionMedianteLongitudesPalabra traduccion longitud = do
 rae <- parseaRAE
 let palabras = map fst rae
 let palabrasLongitud = filter (\x -> (length x) == longitud) palabras
 let palabrasRAENumeradas = map numeraPalabra palabrasLongitud
 let palabrasRAENumeradasYPalabras = zip palabrasRAENumeradas palabrasLongitud
 texto <- leeArchivoConWords "cod.txt"
 let palabrasTextoLongitud = filter (\x -> (length x) == longitud) texto
 let palabrasTextoNumeradas = map numeraPalabra palabrasTextoLongitud
 let palabrasTextoNumeradasYPalabras = zip palabrasTextoNumeradas palabrasTextoLongitud
 let primeraPalabra = palabrasTextoNumeradasYPalabras !! 0
 traducciones2 <- (calculaCoincidencias [primeraPalabra] palabrasRAENumeradasYPalabras traduccion)
 return traducciones2
 

 
 
 
 
--Esta funcion se encarga de asignar una traduccion por maximos, es decir, la letra que mas aparezca en el texto codificado será asignada por la letra que mas aparezca en la RAE, y así sucesivamente con todas las letras en orden
asignaCodificacionPorMaximos :: IO [Char]
asignaCodificacionPorMaximos = do
 frecuenciasRAE <- calculaFrecuenciasRAE
 frecuenciasTexto <- calculaFrecuenciasTexto
 let rae = map (fst) frecuenciasRAE
 let texto =map (fst) frecuenciasTexto
 resultado <- (asignaCodificacion texto rae ['a'..'z'])
 return resultado

 
 
 
--Devuelve el número de coincidencias entre una traduccion dada y la traduccion correcta del texto codificado dado por el profesor
numeroDeCoincidenciasCodificacionCorrecta:: Monad m => [Char] -> m Int
numeroDeCoincidenciasCodificacionCorrecta traduccion =  do
 let correcta=['z','y','x','w','v','j','i','h','g','f','e','d','c','b','a','u','t','s','r','q','p','o','n','m','l','a']
 let unificacion = zipWith (==) traduccion correcta
 let lista = (filter (True ==) unificacion)
 return (length lista)

 
 
 
 
 
 
--Realiza una permutación de la traduccion: "abcdefghijklmnopqrstuvwxyz" --> "bcdefghijklmnopqrstuvwxyza" 
permutaTraduccion :: [a] -> [a]
permutaTraduccion (x:traduccion) =  concat [traduccion,[x]]

--Esta funcion se encarga de realizar la permutacion sobre la traduccion recibida por parámetro y se queda con la traduccion que, una vez decodificado el texto con dicha traduccion, tenga mayor numero de palabras recogidas en la RAE. 
buscaCodificacionMediantePermutacion:: (Ord a, Num a) => [Char] -> a -> IO [Char]
buscaCodificacionMediantePermutacion traduccion numero = do
 if numero < 26 then do
                       numMax <- (numeroPalabrasExistentes traduccion)
                       posibleTraduccion <- (buscaCodificacionMediantePermutacion (permutaTraduccion traduccion) (numero+1))
                       num <- (numeroPalabrasExistentes posibleTraduccion)
                       if (numMax >= num) then return traduccion
                       else return posibleTraduccion
                      -- return resultado
                else return traduccion
				
				
				
				
--Funcion que recibe una palabra y una lista de palabras y devuelve True si la palabra existe en la RAE
perteneceRAE :: Eq a => a -> [a] -> Bool
perteneceRAE palabra rae= elem palabra rae
 
 
 
--Devuelve la letra asignada en la traduccion(decodificada).
devuelveLetraCodificada :: Char -> [Char] -> Char
devuelveLetraCodificada letra traduccion 
 | letra >='a' && letra <= 'z' = traduccion !!((ord letra)-97)
 | otherwise = letra
 
 
 
 
 
 
--Dada una traduccion, calcula el número de palabras del texto utilizando la traduccion que existen en la RAE
numeroPalabrasExistentes :: [Char] -> IO Int
numeroPalabrasExistentes traduccion = do
 texto <- leeArchivoSinWords "cod.txt"
 tuplas <- parseaRAE
 let rae = map (fst) tuplas
 let textoAplicado = map (\x -> (devuelveLetraCodificada x traduccion)) texto 
 let palabrasExistentes = filter (\z -> perteneceRAE z rae) (words textoAplicado)
 return (length palabrasExistentes)

 
 
 
--Esta funcion se encarga de devolver una lista de tuplas [(Char,Int)] que indica el numero de veces que aparece cada letra en el diccionario de la RAE
calculaFrecuenciasRAE :: IO [(Char, Int)]
calculaFrecuenciasRAE = do
 tuplas <- parseaRAE
 let segundaParte = map (fst) tuplas
 let segundaParteJunta = concat segundaParte
 let frecs = map (\x -> length(filter (x==) segundaParteJunta)) ['a'..'z']
 
 return (sortBy ordena (zip ['a'..'z'] frecs))
 
 
 
--Esta funcion se encarga de parsear la entrada de las frecuencias de la RAE, devolviendo una lista de tuplas de la siguiente forma [([Char],Int)], o [(palabra,frecuencia)], es decir, cada palabra junto con la frecuencia con la que aparece según la RAE.
--Por motivos de eficiencia, sólo se leen las 25.000 primeras lineas de la RAE, puesto que en el caso de leer 100.000 o las mas de 700.000 lineas que hay, algunos procedimientos del programa tardarían muchos minutos en procesar el texto y aún más en realizar tu propia tarea. 
parseaRAE :: IO [(String, String)]
parseaRAE = do
 texto <- leeArchivoSinWords "RAE.txt"
 let lineasFinal = drop 5 (lines texto)
 let lineasSeleccionadas = take 25000 lineasFinal --No se pueden leer todas las lineas del texto porque tarda bastante....
 let palabras = map ((!! 1).words) lineasSeleccionadas
 let frecuencias = map ((!! 2).words) lineasSeleccionadas
 return (zip palabras frecuencias)
 
 
--Funcion auxiliar utilizada en el metodo calculaFrecuenciasTexto y en calculaFrecuenciasRAE
ordena :: Ord a => (a2, a) -> (a1, a) -> Ordering
ordena x y = compare (snd y) (snd x)

--Esta funcion se encarga de devolver una lista de tuplas [(Char,Int)] que indica el numero de veces que aparece cada letra en el texto
calculaFrecuenciasTexto :: IO [(Char, Int)]
calculaFrecuenciasTexto = do
 texto <-leeArchivoSinWords "cod.txt"
 let frecs= map (\x -> length(filter (x==) (map toLower texto))) ['a'..'z']
 return (sortBy ordena (zip ['a'..'z'] frecs))
 
 
 
 
 
 
 
 
 
 