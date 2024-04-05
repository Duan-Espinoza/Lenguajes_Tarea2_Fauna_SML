(*
2019079490
1 semestre 2024
Tarea 2
lenguajes de programación
Duan Antonio Espinoza
Modulo analizador de fauna sml
*) 

(* obtenerEntradaUsuario
   Entrada: Ninguna
   Salida: Cadena de texto
   Restricciones: Ninguna
   Objetivo: Solicita al usuario una cadena de texto desde la entrada estándar.

   Esta función ya se encuentra en creador, pero por problemas de importación, se vuelve a agregar
*)
fun obtenerEntradaUsuario () =
    Option.getOpt (TextIO.inputLine TextIO.stdIn, "");

(* dividirCadena
   Entrada: 
   sep -> carácter donde se realizará la división, 
   s -> cadena de texto a dividir
   Salida: Lista de cadenas de caracteres
   Restricciones: sep debe ser de tipo char y s debe ser de tipo string
   Objetivo: Recibe una cadena de texto s y la divide en cada aparición de sep.
   Esta función ya se encuentra en creador, pero por problemas de importación, se vuelve a agregar
*)
fun dividirCadena sep s =
    String.tokens (fn c => c = sep) s;



(* eliminarSaltosDeLinea
   Entrada: s -> una cadena de caracteres con saltos de línea
   Salida: una cadena de caracteres sin saltos de línea
   Restricciones: s debe ser de tipo string
   Objetivo: Elimina los saltos de línea de una cadena de caracteres.
   Esta función ya se encuentra en creador, pero por problemas de importación, se vuelve a agregar
*)
fun eliminarSaltosDeLinea s =
    String.translate (fn #"\n" => "" | c => String.str c) s;



(*
fun imprimirListaEspecimenes(lista) = let
    (* Imprimir encabezado de la tabla *)
    val _ = print "\n| RANKING AVISTAMIENTOS |    CLASE     |            ORDEN            |           ESPECIE           |    ALTURA_LARGO    |       PESO      |\n"
            ^ "|-------------------------------------------------------------------------------------------------------------------------------------|\n"
    
    (* Función auxiliar para imprimir cada especimen *)
    fun imprimirEspecimen(specimen) =
        let
            (* Función auxiliar para formatear cada campo del especimen *)
            fun formatField(field) = Format.format "%-24s" [Format.STR field]
            
            val [ranking, clase, orden, especie, altura, peso] = specimen
            val _ = print "| " 
                    ^ formatField ranking ^ " | "
                    ^ formatField clase ^ " | "
                    ^ formatField orden ^ " | "
                    ^ formatField especie ^ " | "
                    ^ formatField altura ^ " | "
                    ^ formatField peso ^ " |\n"
        in
            ()
        end
    
    (* Iterar sobre cada especimen en la lista e imprimirlo *)
    val _ = List.app imprimirEspecimen lista
    
    in 
        (* Devolver un valor para cumplir con la sintaxis de la función *)
        ()
    end;


*)

(* imprimirListaEspecimenes
   Entrada: lista de especímenes
   Salida: ninguna (imprime en consola)
   Restricciones: la lista debe cumplir con la estructura esperada
   Objetivo: Recibe una lista de especímenes y la imprime en consola con formato de tabla
   Uso de Format
*)

fun imprimirListaEspecimenes(lista) = let
    val _ = print "\nRANKING VISTO |\t\tCLASE |\t\t\tORDEN |\t\t\t  ESPECIE |\t    ALTURA_LARGO |\t\t    PESO |\n\
                    \___________________________________________________________________________________________________________________________________________"
    val _ = List.map(fn x =>
                print ("\n" ^Format.format "%21s" [Format.STR (List.nth(x,0))]  ^
                      " |\t"^Format.format "%13s" [Format.STR (List.nth(x,1))]  ^
                      " |\t"^Format.format "%21s" [Format.STR (List.nth(x,2))]  ^
                      " |\t"^Format.format "%25s" [Format.STR (List.nth(x,3))]  ^
                      " |\t"^Format.format "%16s" [Format.STR (List.nth(x,4))]  ^
                      " |\t"^Format.format "%16s" [Format.STR (List.nth(x,5))]  ^ " |"
                )
            ) lista
    in 
        [[""]]
    end
;



(* dividirCadenaPorComas
   Entrada: cadena de texto
   Salida: lista de cadenas de caracteres
   Restricciones: la cadena debe ser de tipo string
   Objetivo: Recibe una cadena de texto y la divide en subcadenas en cada aparición de la coma (',').
   nil: lista vacía
*)
fun dividirCadenaPorComas(nil) = nil
|   dividirCadenaPorComas(x::xs) =  String.tokens (fn c => c = #",") x::dividirCadenaPorComas(xs)
;


(* leerLineasArchivo
   Entrada: Ninguna
   Salida: Lista de cada línea del archivo
   Restricciones: El archivo indicado debe existir
   Objetivo: Solicita al usuario el nombre del archivo, lo abre y devuelve una lista donde cada elemento es una línea del archivo.
*)
fun leerLineasArchivo () = let 
    val nombreArchivo =  eliminarSaltosDeLinea(obtenerEntradaUsuario())
    val canalEntrada = TextIO.openIn nombreArchivo 
    fun leerLineas canal = 
        case TextIO.inputLine canal of 
            SOME linea => eliminarSaltosDeLinea(linea) :: leerLineas canal 
          | NONE      => [] 
    in 
        leerLineas canalEntrada before TextIO.closeIn canalEntrada 
    end


(* eliminarElementoEnIndice
   Entrada: Una lista x y el índice del elemento a eliminar
   Salida: La lista x sin el elemento indicado 
   Restricciones: El índice debe ser válido
   Objetivo: Eliminar el elemento en la posición indicada de la lista.
*)
fun eliminarElementoEnIndice ([], _) = []
  | eliminarElementoEnIndice (x::xs, 1) = xs
  | eliminarElementoEnIndice (x::xs, k) = x :: eliminarElementoEnIndice (xs, k - 1)


(* imprimirEspeciesRangoAscendente
   Entrada: Lista de especímenes, valor inicial y final del rango 
   Salida: Imprime en consola la lista calculada
   Restricciones: Los índices deben ser válidos de acuerdo a la lista
   Objetivo: Imprime en consola los especímenes ordenados ascendentemente dentro del rango indicado.
*)
fun imprimirEspeciesRangoAscendente(lista) = let
    (* Solicita al usuario la posición de inicio del rango *)
    fun solicitarInicio () = let
        val _ = print "\nInicio: "
        val inicio = valOf(Int.fromString(eliminarSaltosDeLinea(obtenerEntradaUsuario())))
        in
            if inicio < 0 then 0
            else inicio
        end

    val inicio = solicitarInicio()

    (* Solicita al usuario la posición final del rango *)
    fun solicitarFinal () = let
        val _ = print "\nFinal: "
        val final = valOf(Int.fromString(eliminarSaltosDeLinea(obtenerEntradaUsuario())))
        in
            if final > List.length(lista) then List.length(lista)
            else final
        end

    val final = solicitarFinal()
    val res = List.drop(List.take(lista, final), inicio)
    val _ = imprimirListaEspecimenes(res)
    in
        res
    end


(* imprimirEspeciesAltura
   Entrada: Lista de especímenes y altura a superar
   Salida: Imprime en consola la lista calculada
   Restricciones: El valor ingresado en altura debe ser numérico
   Objetivo: Filtra todos los especímenes cuya altura/largo sea mayor que el valor indicado y los imprime en una tabla.
*)
fun imprimirEspeciesAltura(lista) = let
    (* Solicita al usuario la altura a filtrar *)
    val _ = print "\nAltura por filtrar: "
    val altura = valOf(Real.fromString(eliminarSaltosDeLinea(obtenerEntradaUsuario())))

    (* Filtra los especímenes que superan la altura indicada *)
    val filtrados = List.filter(fn x => valOf(Real.fromString(List.nth(x, 4))) > altura) lista

    (* Ordena la lista filtrada por altura/largo *)
    val res = ListMergeSort.sort (fn(x,y)=> valOf(Real.fromString(List.nth(x, 4))) <
                                    valOf(Real.fromString(List.nth(y, 4)))) filtrados

    (* Imprime la lista de especímenes filtrados *)
    val _ = imprimirListaEspecimenes(res)
in
    res
end;

(********************************)
(* imprimirEspeciesPorRanking
   Entrada: Lista de especímenes y posición del ranking
   Salida: Imprime en consola la lista calculada
   Restricciones: El valor del ranking debe ser un número entero
   Objetivo: Filtra los especímenes que coinciden con el ranking indicado y los imprime en una tabla.
*)
fun imprimirEspeciesPorRanking(lista) = let
    (* Solicita al usuario la posición del ranking *)
    val _ = print "\nPosición del ranking: "
    val posicion = valOf(Int.fromString(eliminarSaltosDeLinea(obtenerEntradaUsuario())))

    (* Filtra los especímenes que tienen el ranking indicado *)
    val filtrados = List.filter(fn x => valOf(Int.fromString(hd x)) = posicion) lista

    (* Imprime la lista de especímenes filtrados *)
    val _ = imprimirListaEspecimenes(filtrados)
in
    filtrados
end;


(********************************)
(* imprimirEspeciesPorClase
   Entrada: Lista de especímenes y clase a filtrar
   Salida: Imprime en consola la lista calculada
   Restricciones: Ninguna
   Objetivo: Solicita al usuario una clase y luego imprime una tabla con todos los especímenes que pertenecen a esa clase.
*)
fun imprimirEspeciesPorClase(lista) = let
    (* Solicita al usuario la clase a filtrar *)
    val _ = print "\nClase a filtrar: "
    val clase = eliminarSaltosDeLinea(obtenerEntradaUsuario())

    (* Filtra los especímenes que pertenecen a la clase indicada *)
    val filtrados = List.filter(fn x => 
                                String.map(fn y => Char.toUpper(y)) (List.nth(x,1)) = 
                                String.map(fn z => Char.toUpper(z)) clase
                            ) lista

    (*El uso de map en sml es para ka transformación de listas*)
    (* Imprime la lista de especímenes filtrados *)
    val _ = imprimirListaEspecimenes(filtrados)
in
    filtrados
end;

(********************************)
(* imprimirEspeciesPorOrden
   Entrada: Lista de especímenes y orden a filtrar
   Salida: Imprime en consola la lista calculada
   Restricciones: Ninguna
   Objetivo: Solicita al usuario un orden y luego imprime una tabla con todos los especímenes que pertenecen a ese orden.
*)
fun imprimirEspeciesPorOrden(lista) = let
    (* Solicita al usuario el orden a filtrar *)
    val _ = print "\nOrden a filtrar: "
    val orden = eliminarSaltosDeLinea(obtenerEntradaUsuario())

    (* Filtra los especímenes que pertenecen al orden indicado *)
    val filtrados = List.filter(fn x => 
                                String.map(fn y => Char.toUpper(y)) (List.nth(x,2)) = 
                                String.map(fn z => Char.toUpper(z)) orden
                            ) lista

    (*El uso de map en sml es para ka transformación de listas*)
    (* Imprime la lista de especímenes filtrados *)
    val _ = imprimirListaEspecimenes(filtrados)
in
    [[Int.toString(List.length(filtrados))]]
end;


(********************************)
(* eliminarRepeticionesPorEspecie
   Entrada: Lista de especímenes
   Salida: Lista sin valores repetidos según el índice 3 (especie)
   Restricciones: La lista debe tener al menos 4 elementos
   Objetivo: Recibe una lista de especímenes y elimina todas aquellas repeticiones según la especie.
*)
fun eliminarRepeticionesPorEspecie [] = []
  | eliminarRepeticionesPorEspecie (x::xs) = 
    x :: eliminarRepeticionesPorEspecie (
      List.filter (fn y => 
        String.map (fn y => Char.toUpper(y)) (List.nth(y,3)) <> 
        String.map (fn z => Char.toUpper(z)) (List.nth(x,3))
      ) xs
      (*El uso de map en sml es para ka transformación de listas*)
    );



(* eliminarRepetidos
   Entrada: Lista de elementos
   Salida: Lista sin elementos repetidos
   Restricciones: Ninguna
   Objetivo: Elimina los elementos repetidos de una lista.
*)
fun eliminarRepetidos [] = []
  | eliminarRepetidos (x::xs) = 
    x :: eliminarRepetidos (List.filter (fn y => y <> x) xs);



(************)

(* generarResumen
   Entrada: Lista de especímenes
   Salida: Impresión del resumen en consola
   Restricciones: Ninguna
   Objetivo: Imprime en consola un resumen de la información contenida en la lista de especímenes.
*)
fun generarResumen (lista) =
    let
        val cantEspecies = eliminarRepeticionesPorEspecie(lista)
        val especieTamanioNombre = ListMergeSort.sort (fn(x,y)=> String.size(List.nth(x,3)) <
                                            String.size(List.nth(y,3))) lista
        val especiesPeq = List.filter(fn x => 0.0 <= valOf(Real.fromString(List.nth(x,4)))
                                            andalso valOf(Real.fromString(List.nth(x,4))) <=2.5) lista
        
        val especiesMed = List.filter(fn x => 2.6 <= valOf(Real.fromString(List.nth(x,4)))
                                            andalso valOf(Real.fromString(List.nth(x,4))) <=5.0) lista
    
        val especiesGrandes = List.filter(fn x => valOf(Real.fromString(List.nth(x,4))) > 5.0) lista
    
        val espXClase = eliminarRepetidos( List.map (
            fn x => [List.nth(x,1)] @ 
                    [Int.toString(List.length(
                        List.filter(fn y => List.nth(y,1) = List.nth(x,1)) lista
                    ))]
            )lista)
    
        val _ = print(
            "\nCantidad de especies: " ^ Int.toString(List.length(cantEspecies)) ^
            "\nEspecie con el nombre más largo: " ^ List.nth(hd especieTamanioNombre,3) ^
            "\nCantidad de especies pequeñas: " ^ Int.toString (List.length(especiesPeq)) ^
            "\nCantidad de especies medianas: " ^ Int.toString (List.length(especiesMed)) ^
            "\nCantidad de especies grandes: " ^ Int.toString (List.length(especiesGrandes)) ^
            "\n\nCantidad de especies por clase\n"
        )
    
        val _ = print "\n\tClase |\t\tCantidad |\n_____________________________________________________"
        val _ = List.map(fn x =>
                    print ("\n" ^ Format.format "%13s" [Format.STR (List.nth(x,0))] ^
                            " |\t" ^ Format.format "%16s" [Format.STR (List.nth(x,1))] ^ " |")
                ) espXClase
    
        val _ = print"\n"
    in
        [[""]]
        (*El uso de map en sml es para ka transformación de listas*)
    end;



(*fun generarResumen(lista) =
    let
        (* Función auxiliar para contar especies por clase *)
        fun contarPorClase([], clase, acc) = acc
          | contarPorClase((_, clase' :: _, _, _, _, _) :: xs, clase, acc) =
              if clase = clase' then contarPorClase(xs, clase, acc + 1)
              else contarPorClase(xs, clase, acc)

        (* Número total de especies *)
        val totalEspecies = length lista

        (* Lista de nombres de especies *)
        val nombresEspecies = List.map (fn (_, _, _, especie, _, _) => especie) lista

        (* Especie con el nombre más largo *)
        val especieLarga = List.max (List.map String.size nombresEspecies)

        (* Especies clasificadas por tamaño *)
        val (pequenas, medianas, grandes) =
            List.foldl (fn (_, _, _, _, peso, _) => fn (peqs, meds, grands) =>
                case Real.fromString peso of
                    SOME pesoNum =>
                        if pesoNum <= 2.5 then (peqs + 1, meds, grands)
                        else if pesoNum <= 5.0 then (peqs, meds + 1, grands)
                        else (peqs, meds, grands + 1)
                  | NONE => (peqs, meds, grands)) (0, 0, 0) lista

        (* Lista de clases y cantidad de especies por clase *)
        val clasesYCantidades = List.map (fn (_, clase :: _, _, _, _, _) =>
            (clase, contarPorClase(lista, clase, 0))) lista

        (* Impresión del resumen *)
        val _ = print(
            "\nCantidad total de especies: " ^ Int.toString totalEspecies ^
            "\nEspecie con el nombre más largo: " ^ Int.toString especieLarga ^
            "\nCantidad de especies pequeñas: " ^ Int.toString pequenas ^
            "\nCantidad de especies medianas: " ^ Int.toString medianas ^
            "\nCantidad de especies grandes: " ^ Int.toString grandes ^
            "\n\nCantidad de especies por clase\n"
        )

        val _ = print "\n\tClase |\t\tCantidad |\n_____________________________________________________"
        val _ = List.app (fn (clase, cantidad) =>
            print ("\n" ^ Format.format "%13s" [Format.STR clase] ^
                   " |\t" ^ Format.format "%16s" [Format.INT cantidad] ^ " |")
        ) clasesYCantidades

        val _ = print "\n"
    in
        [[""]]
    end;
*)


(********************************)

(* menuPrincipal
   Entrada: Lista de especímenes
   Salida: Llamada a la función escogida por el usuario
   Restricciones: El usuario debe elegir una opción válida del menú
   Objetivo: Despliega un menú al usuario para que este elija qué operación llevar a cabo.
*)
fun menuPrincipal (lista) = let
    val menu = print "\n\n\n1. Mostrar especies en rango ascendente\n\
                     \2. Mostrar detalles de especies por altura\n\
                     \3. Mostrar especies por ranking\n\
                     \4. Mostrar especies por clase\n\
                     \5. Mostrar especies por orden\n\
                     \6. Generar resumen\n\
                     \7. Salir\n\n\
                     \Escoja una opción: "
    val opcion = eliminarSaltosDeLinea(obtenerEntradaUsuario())        
    in 
        if opcion = "1" then
            imprimirEspeciesRangoAscendente(lista)
        else if opcion = "2" then
            imprimirEspeciesAltura(lista)
        else if opcion = "3" then
            imprimirEspeciesPorRanking(lista)
        else if opcion = "4" then
            imprimirEspeciesPorClase(lista)
        else if opcion = "5" then
            imprimirEspeciesPorOrden(lista)
        else if opcion = "6" then
            generarResumen(lista)
        else if opcion = "7" then OS.Process.exit(OS.Process.success)
        else menuPrincipal(lista)
    end;



(********************************)

(* main
   Entrada: Ninguna
   Salida: Ejecución del programa
   Restricciones: Ninguna
   Objetivo: Solicitar al usuario la ruta del archivo y luego iniciar el programa.
*)
fun main () = let
    (* Solicitar al usuario la ruta del archivo *)
    val _ = print"Ingrese la ruta del archivo: \t"
    
    (* Abrir el archivo y eliminar la primera línea (encabezado) *)
    val lista = eliminarElementoEnIndice (leerLineasArchivo(), 1)

    (* Convertir la lista en una lista de listas para el ordenamiento *)
    val lista2 = [lista]

    (* Ordenar la lista por el primer elemento de cada sublista (ranking) de forma descendente *)
    val order = ListMergeSort.sort (fn(x,y) => valOf(Int.fromString (hd x)) >
                                              valOf(Int.fromString(hd y))) lista2
in 
    (* Bucle infinito para imprimir el menú y ejecutar las opciones *)
    while true do (
        menuPrincipal(order)
    )
end;





(*Ideas/Pruebas*)
(*
(*
Tarea 2
lenguajes de programación
Duan Espinoza
2019079490
*)

open Order;
open IntRedBlackSet;



(* Función para obtener una línea de texto del usuario *)
fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");


(* Función para dividir una cadena de texto en una lista de cadenas *)
fun splitString separator text = String.tokens (fn c => c = separator) text;


(* Función para eliminar saltos de línea de una cadena de texto *)
fun removeNewlines text = String.translate (fn #"\n" => "" | c => String.str c) text;



fun compareByRating ((_, _, _, _, rating1, _), (_, _, _, _, rating2, _)) =
    case Real.fromString rating1 of
        SOME r1 =>
            (case Real.fromString rating2 of
                SOME r2 => if r1 > r2 then GREATER else LESS
              | NONE => LESS)
      | NONE => LESS



(* Función de ordenación personalizada *)
fun customSort compareFunc lst =
    let
        fun insertionSort [] = []
          | insertionSort (x::xs) =
            let
                fun insert (y, []) = [y]
                  | insert (y, z::zs) =
                    if compareFunc (x, z) = LESS then
                        y::x::z::zs
                    else
                        z::insert(y, zs)
            in
                insert (x, insertionSort xs)
            end
    in
        insertionSort lst
    end;


(* Función para limpiar un archivo (crear uno nuevo) *)
fun clearFile path = let
    val fd = TextIO.openOut path
    val _ = TextIO.output(fd, "NOMBRE,PARADIGMA,PERMITE_DESARROLLO_MOVIL,PERMITE_DESARROLLO_WEB,RATING_PRINCIPAL,RATING_EMPLEOS\n")
    val _ = TextIO.closeOut fd
in
    ()
end;




(* Llama a la función customSort con los datos como argumento *)
val sortedData = customSort compareByRating data



(* Función para verificar si una cadena es un entero *)
fun isInteger str = case Int.fromString str of
    NONE => false
  | _ => true;


(* Función para verificar si una cadena es un número real *)
fun isReal str = case Real.fromString str of
    NONE => false
  | _ => true;



(* Función auxiliar para verificar si una cadena está compuesta solo de espacios en blanco *)
fun isWhitespace str =
    case String.tokens Char.isSpace str of
        [] => true
      | _  => false;


(* Función para solicitar un valor no numérico al usuario *)
fun requestNonNumericValue prompt = let
    val _ = print (prompt ^ ": ")
    val value = removeNewlines (getLine ())
in
    if String.size value = 0 orelse isWhitespace value then
        value
    else
        if isInteger value orelse isReal value then
            let
                val _ = print "\nIngrese un valor no numérico válido."
            in
                requestNonNumericValue prompt
            end
        else
            value
end;


(* Función para solicitar un valor numérico al usuario *)
fun requestNumericValue prompt = let
    val _ = print (prompt ^ ": ")
    val value = removeNewlines (getLine ())
in
    if isInteger value orelse isReal value then
        value
    else
        let
            val _ = print "\nIngrese un valor numerico valido."
        in
            requestNumericValue prompt
        end
end;


(* Función para agregar un nuevo lenguaje al índice *)
fun addLanguageToIndex path = let
    val fd = TextIO.openAppend path

    val name = requestNonNumericValue "Ingrese el nombre del lenguaje"
    val paradigm = requestNonNumericValue "Ingrese el paradigma del lenguaje"
    val allowsMobileDevelopment = requestNonNumericValue "¿Permite desarrollo movil? (si/no)"
    val allowsWebDevelopment = requestNonNumericValue "¿Permite desarrollo web? (si/no)"
    val principalRating = requestNumericValue "Ingrese el rating principal"
    val jobRatings = requestNumericValue "Ingrese el rating por empleos"

    val _ = TextIO.output(fd, name ^ "," ^ paradigm ^ "," ^ allowsMobileDevelopment ^ "," ^ allowsWebDevelopment ^ "," ^ principalRating ^ "," ^ jobRatings ^ "\n")
    val _ = TextIO.closeOut fd
in
    ()
end;


(* Función para mostrar el menú principal *)
fun printMainMenu () = let
    val _ = print "\nMenu Principal:\n"
    val _ = print "a. Mostrar top (ascendente por posición)\n"
    val _ = print "b. Mostrar top por rating por empleos (ascendente por rating empleo)\n"
    val _ = print "c. Detalles de lenguajes web\n"
    val _ = print "d. Detalles de lenguajes móviles\n"
    val _ = print "e. Cantidad de lenguajes por paradigma\n"
    val _ = print "f. Resumen\n"
    val _ = print "0. Salir\n"
in
    ()
end;



(* Función para procesar la opción seleccionada *)
fun processOption path option = case option of
    "1" => addLanguageToIndex path
  | "2" => clearFile path
  | "0" => TextIO.output(TextIO.stdOut, "Gracias por usar el programa. ¡Hasta luego!\n")
  | _ => TextIO.output(TextIO.stdOut, "Opcion no valida. Por favor, seleccione una opcion valida.\n");


(* ... Código previo ... *)

(* Función para obtener la ruta del archivo del índice desde el usuario *)
fun getPathFromUser() =
    let
        val _ = print "Ingrese la ruta del archivo del indice: "
    in
        removeNewlines (getLine ())
    end;



(* Función para manejar la opción a: Mostrar top (ascendente por posición) *)
fun handleOptionA data =
    let
        
        val sortedData = customSort compareByRating data
        (* Mostrar el resultado en forma tabulada *)
        val _ = print "Top (ascendente por posición):\n"
        val _ = print "Posición | Nombre de Lenguaje | Rating Principal\n"
        val _ = print "--------------------------------------------\n"
        fun printLanguage (position, (name, _, _, _, rating, _)) =
            print (Int.toString position ^ "\t|\t" ^ name ^ "\t|\t" ^ rating ^ "\n")
        val _ = ListPair.appi printLanguage (sortedData, List.tabulate (length sortedData, fn i => i + 1))
    in
        ()
    end;



(* Función para manejar la opción b: Mostrar top por rating por empleos (ascendente por rating empleo) *)
fun handleOptionB data =
    let
        
        val sortedData = customSort compareByRating data
        (* Mostrar el resultado en forma tabulada *)
        val _ = print "Top por Rating por Empleos (ascendente por rating empleo):\n"
        val _ = print "Nombre de Lenguaje | Rating por Empleos\n"
        val _ = print "-------------------------------------\n"
        fun printLanguage (name, rating) =
            print (name ^ "\t|\t" ^ rating ^ "\n")
        val _ = List.app printLanguage sortedData
    in
        ()
    end;



(* Función para manejar la opción c: Detalles de lenguajes web *)
fun handleOptionC data =
    let
        
        val webLanguages = filterLanguagesByCategory data "web"
        (* Mostrar los detalles de lenguajes web en forma tabulada *)
        val _ = print "Detalles de Lenguajes Web:\n"
        val _ = print "Nombre de Lenguaje | Paradigma | Rating Principal | Rating por Empleos\n"
        val _ = print "-------------------------------------------------------------\n"
        fun printLanguage (name, paradigm, _, _, rating1, rating2) =
            print (name ^ "\t|\t" ^ paradigm ^ "\t|\t" ^ rating1 ^ "\t|\t" ^ rating2 ^ "\n")
        val _ = List.app printLanguage webLanguages
    in
        ()
    end;





(* Función para manejar la opción d: Detalles de lenguajes móviles *)
fun handleOptionD data =
    let
        
        val mobileLanguages = filterLanguagesByCategory data "movil"
        (* Mostrar los detalles de lenguajes móviles en forma tabulada *)
        val _ = print "Detalles de Lenguajes Móviles:\n"
        val _ = print "Nombre de Lenguaje | Paradigma | Rating Principal | Rating por Empleos\n"
        val _ = print "-------------------------------------------------------------\n"
        fun printLanguage (name, paradigm, _, _, rating1, rating2) =
            print (name ^ "\t|\t" ^ paradigm ^ "\t|\t" ^ rating1 ^ "\t|\t" ^ rating2 ^ "\n")
        val _ = List.app printLanguage mobileLanguages
    in
        ()
    end;




(* Función para manejar la opción e: Cantidad de lenguajes por paradigma *)
fun handleOptionE data =
    let
        val paradigm = requestNonNumericValue "Ingrese el paradigma a buscar"
        (* Filtra los lenguajes por paradigma *)
        val filteredLanguages = filterLanguagesByParadigm data paradigm
    in
        (* Muestra la información resultante de manera tabulada *)
        val _ = print ("Lenguajes en el paradigma '" ^ paradigm ^ "':\n")
        val _ = print "Nombre de Lenguaje | Paradigma | Rating Principal | Rating por Empleos\n"
        val _ = print "-------------------------------------------------------------\n"
        fun printLanguage (name, paradigm, _, _, rating1, rating2) =
            print (name ^ "\t|\t" ^ paradigm ^ "\t|\t" ^ rating1 ^ "\t|\t" ^ rating2 ^ "\n")
        val _ = List.app printLanguage filteredLanguages
    end;




(* Función para manejar la opción f: Resumen *)
fun handleOptionF data =
    let
        
        val highestMainRatingLanguage = getLanguageWithHighestMainRating data
        (* Obtener el lenguaje con el rating principal más bajo *)
        val lowestMainRatingLanguage = getLanguageWithLowestMainRating data
        (* Obtener el paradigma con más lenguajes *)
        val (paradigmWithMostLanguages, count) = getParadigmWithMostLanguages data
        (* Mostrar el resumen *)
        val _ = print "Resumen:\n"
        val _ = print "-------------------------------------------------------------\n"
        val _ = print "Lenguaje con el Rating Principal más Alto: " ^ #1 highestMainRatingLanguage ^ "\n"
        val _ = print "Rating Principal más Alto: " ^ #5 highestMainRatingLanguage ^ "\n"
        val _ = print "Lenguaje con el Rating Principal más Bajo: " ^ #1 lowestMainRatingLanguage ^ "\n"
        val _ = print "Rating Principal más Bajo: " ^ #5 lowestMainRatingLanguage ^ "\n"
        val _ = print "Paradigma con más Lenguajes: " ^ paradigmWithMostLanguages ^ "\n"
        val _ = print "Cantidad de Lenguajes en ese Paradigma: " ^ Int.toString count ^ "\n"
    in
        ()
    end;



(* Función para manejar la opción 0: Salir *)
fun handleOption0 () =
    let
        val _ = TextIO.output (TextIO.stdOut, "Gracias por usar el programa. ¡Hasta luego!\n")
    in
        (* Termina el programa *)
        raise Empty
    end;




(* Función para cargar los datos desde un archivo CSV *)
fun loadDataFromFile path =
    let
        val file = TextIO.openIn path
        val header = TextIO.inputLine file;  (* Leer la primera línea (encabezado) y descartarla *)
        val lines = TextIO.inputAll file;    (* Leer todas las líneas restantes *)
    in
        TextIO.closeIn file;
        (* Dividir las líneas en una lista de registros *)
        map (splitString ",") (String.tokens (fn c => c = #"\n") lines)
    end;

(* Función para obtener el lenguaje con el rating principal más alto *)
fun getLanguageWithHighestMainRating data =
    let
        fun maxByMainRating ([], currentMax) = currentMax
          | maxByMainRating (((_, _, _, rating1), (_, _, _, rating2)) :: rest, currentMax) =
            if Real.fromString rating1 > Real.fromString rating2 then
                maxByMainRating (rest, rating1)
            else
                maxByMainRating (rest, rating2)
    in
        maxByMainRating (data, "0.0")
    end;

(* Función para obtener el lenguaje con el rating principal más bajo *)
fun getLanguageWithLowestMainRating data =
    let
        fun minByMainRating ([], currentMin) = currentMin
          | minByMainRating (((_, _, _, rating1), (_, _, _, rating2)) :: rest, currentMin) =
            if Real.fromString rating1 < Real.fromString rating2 then
                minByMainRating (rest, rating1)
            else
                minByMainRating (rest, rating2)
    in
        minByMainRating (data, "10.0") (* Inicializar con un valor alto para que se reemplace *)
    end;

(* Función para obtener el paradigma con más lenguajes *)
fun getParadigmWithMostLanguages data =
    let
        
        fun countParadigms ([], counts) = counts
          | countParadigms (((_, paradigm, _, _), rest) :: xs, counts) =
            countParadigms (xs, if IntMap.member (counts, paradigm) then IntMap.add (counts, paradigm, 1) else counts)
        (* Encontrar el paradigma con la cuenta máxima *)
        fun findMaxParadigm counts = let
            fun maxPair ((paradigm1, count1), (paradigm2, count2)) =
                if count1 > count2 then
                    (paradigm1, count1)
                else
                    (paradigm2, count2)
        in
            IntMap.fold maxPair counts ("", 0)
        end
    in
        findMaxParadigm (countParadigms (data, IntMap.empty))
    end;

(* Función para filtrar lenguajes por paradigma *)
fun filterLanguagesByParadigm data paradigm =
    List.filter (fn ((_, p, _, _), _) => p = paradigm) data;

(* Función para filtrar lenguajes por categoría (desarrollo web o móvil) *)
fun filterLanguagesByCategory data category =
    List.filter (fn ((_, _, mobile, web, _, _), _) => (category = "web" andalso web = "si") orelse (category = "movil" andalso mobile = "si")) data;






(* Función principal *)
fun main () =
    let
        val _ = print "Bienvenido al Analizador de Índice IEEE Spectrum.\n"
        val path = getPathFromUser ()
        val data = loadDataFromFile path
    in
        (* El programa continuará ejecutándose hasta que el usuario elija salir (opción 0) *)
        while true do
            let
                val _ = printMainMenu ()
                val option = requestNonNumericValue "Ingrese la opción deseada"
            in
                case option of
                    "a" => handleOptionA data
                  | "b" => handleOptionB data
                  | "c" => handleOptionC data
                  | "d" => handleOptionD data
                  | "e" => handleOptionE data
                  | "f" => handleOptionF data
                  | "0" => handleOption0 ()
                  | _   => TextIO.output (TextIO.stdOut, "Opción no válida. Por favor, seleccione una opción válida.\n")
            end
        end;






(* Ejecutar la función main para iniciar el programa *)
val _ = main ();

*)