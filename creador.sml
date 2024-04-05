(*Ideas/Pruebas*)
(*

(*
Tarea 2
lenguajes de programación
Duan Espinoza
2019079490
*)

(* Función para obtener una línea de texto del usuario *)
fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");


(* Función para dividir una cadena de texto en una lista de cadenas *)
fun splitString separator text = String.tokens (fn c => c = separator) text;


(* Función para eliminar saltos de línea de una cadena de texto *)
fun removeNewlines text = String.translate (fn #"\n" => "" | c => String.str c) text;


(* Función para limpiar un archivo (crear uno nuevo) *)
fun clearFile path = let
    val fd = TextIO.openOut path
    val _ = TextIO.output(fd, "NOMBRE,PARADIGMA,PERMITE_DESARROLLO_MOVIL,PERMITE_DESARROLLO_WEB,RATING_PRINCIPAL,RATING_EMPLEOS\n")
    val _ = TextIO.closeOut fd
in
    ()
end;


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
    val _ = print "1. Agregar un nuevo lenguaje al indice\n"
    val _ = print "2. Limpiar el indice\n"
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

(* Función para manejar la opción 1: Agregar un nuevo lenguaje al índice *)
fun handleOption1 path = addLanguageToIndex path;

(* Función para manejar la opción 2: Limpiar el índice *)
fun handleOption2 path = clearFile path;

(* Función para manejar la opción 0: Salir del programa *)
fun handleOption0 () = TextIO.output (TextIO.stdOut, "Gracias por usar el programa. ¡Hasta luego!\n");

(* Función principal del programa *)
fun main () =
    let
        val _ = print "Bienvenido al Gestor de Lenguajes.\n"
        val path = getPathFromUser ()
    in
        clearFile path; (* Limpiar el archivo del índice al inicio *)
        while true do
            let
                val _ = printMainMenu ()
                val option = requestNumericValue "Ingrese el numero de opcion deseada"
            in
                case option of
                    "1" => handleOption1 path
                  | "2" => handleOption2 path
                  | "0" => handleOption0 ()
                  | _   => TextIO.output (TextIO.stdOut, "Opcion no valida. Por favor, seleccione una opcion valida.\n")
            end
        end;


(* Ejecutar la función main para iniciar el programa *)
val _ = main ();


*)

















(*
2019079490
1 semestre 2024
Tarea 2
lenguajes de programación
Duan Antonio Espinoza
Modulo creador de fauna sml
*)  

(* obtenerEntradaUsuario
   Entrada: Ninguna
   Salida: Cadena de texto
   Restricciones: Ninguna
   Objetivo: Solicita al usuario una cadena de texto desde la entrada estándar.
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
*)
fun dividirCadena sep s =
    String.tokens (fn c => c = sep) s;


(* eliminarSaltosDeLinea
   Entrada: s -> una cadena de caracteres con saltos de línea
   Salida: una cadena de caracteres sin saltos de línea
   Restricciones: s debe ser de tipo string
   Objetivo: Elimina los saltos de línea de una cadena de caracteres.
*)
fun eliminarSaltosDeLinea s =
    String.translate (fn #"\n" => "" | c => String.str c) s;


(* crearYLimpiarIndice
   Entrada: ruta -> la ruta del archivo del índice
   Salida: ninguna
   Restricciones: ninguna
   Objetivo: Crea un archivo de índice si no existe y lo limpia, restableciéndolo con encabezados predeterminados.
*)
fun crearYLimpiarIndice (ruta) = let
    val fd = TextIO.openOut ruta
    val _ = TextIO.output(fd, "RANKING AVISTAMIENTOS,CLASE,ORDEN,ESPECIE,ALTURA_LARGO,PESO") 
            handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
in
    ()
end;

(* esEntero
   Entrada: s -> una cadena de caracteres
   Salida: true si s representa un entero, false si no
   Restricciones: s debe ser de tipo string
   Objetivo: Verifica si una cadena de caracteres representa un entero.
*)
fun esEntero s = 
    case Int.fromString(s) of
        NONE => false
      | _ => true;

(* esReal
   Entrada: s -> una cadena de caracteres
   Salida: true si s representa un número real, false si no
   Restricciones: s debe ser de tipo string
   Objetivo: Verifica si una cadena de caracteres representa un número real.
*)
fun esNumeroReal s = 
    case Real.fromString(s) of
        NONE => false
      | _ => true;


(* solicitarEntradaNoNumerica
   Entrada: s -> descripción del valor que se está solicitando
   Salida: valor ingresado por el usuario
   Restricciones: ninguna
   Objetivo: Solicita al usuario un valor hasta que ingrese una cadena de texto no numérica.
*)
fun solicitarEntradaNoNumerica (s) = let
    val _ = print ("\n" ^ s ^ ": ")
    val valor = eliminarSaltosDeLinea (obtenerEntradaUsuario())
in 
    if (esEntero valor orelse esNumeroReal valor) orelse String.size valor = 0 then 
        let 
            val _ = print "\nIngrese un valor no numérico!" 
        in 
            solicitarEntradaNoNumerica s 
        end
    else 
        valor
end;


(* solicitarNumero
   Entrada: s -> descripción del valor que se está solicitando
   Salida: valor solicitado por el usuario
   Restricciones: ninguna
   Objetivo: Solicita al usuario un valor hasta que ingrese un número.
*)
fun solicitarNumero (s) = let
    val _ = print ("\n" ^ s ^ ": ")
    val num = eliminarSaltosDeLinea (obtenerEntradaUsuario())
in 
    if esEntero num orelse esNumeroReal num
    then num
    else
        let 
            val _ = print "\nIngrese un valor válido!" 
        in 
            solicitarNumero s 
        end
end;


(*(* agregarEspecimenNuevo
   Entrada: ruta -> la ruta del archivo donde se agregará la entrada
   Salida: ninguna
   Restricciones: ninguna
   Objetivo: Solicitar al usuario los valores necesarios para agregar un nuevo especimen al índice y agregarlo al archivo.
*)
fun agregarEspecimenNuevo (ruta) = let
    val archivo = TextIO.openAppend ruta

    (* Función auxiliar para solicitar al usuario una entrada no numérica *)
    fun solicitarEntradaNoNumerica(mensaje) =
        let
            val _ = print ("\n" ^ mensaje ^ ": ")
            val entrada = StringCvt.scanString TextIO.stdIn
        in
            (case entrada of
                SOME valor => valor
              | NONE => (
                    print "\nError: Entrada inválida.";
                    solicitarEntradaNoNumerica mensaje
                  )
            )
        end

    (* Solicitar al usuario el ranking del especimen *)
    val solicitarRank = fn () =>
        let
            val _ = print "\nRanking: "
            val num = TextIO.inputLine TextIO.stdIn
        in 
            if String.isSubstringOf "#" num
            then (
                print "\nError: No se permiten caracteres especiales.";
                solicitarRank ()
            )
            else
                num
        end

    val rank = solicitarRank ()
    val clase = solicitarEntradaNoNumerica "Clase"
    val orden = solicitarEntradaNoNumerica "Orden"
    val especie = solicitarEntradaNoNumerica "Especie"
    val altura = solicitarEntradaNoNumerica "Altura"
    val peso = solicitarEntradaNoNumerica "Peso"
    
    val _ = TextIO.output(archivo, "\n" ^ rank ^ "," ^ clase ^ "," ^ orden ^ "," ^ especie ^ "," ^ altura ^ "," ^ peso) 
            handle e => (TextIO.closeOut archivo; raise e)
    
    val _ = TextIO.closeOut archivo
in
    ()
end;
*)



(* agregarNuevoEspecimen
   Entrada: ruta -> la ruta del archivo donde se agregará la entrada
   Salida: ninguna
   Restricciones: ninguna
   Objetivo: Solicita al usuario los valores necesarios para añadir un nuevo especimen al índice y lo agrega al archivo.
*)
fun agregarNuevoEspecimen (ruta) = let
    val fd = TextIO.openAppend ruta
   
    (* solicitarRank: Solicita al usuario el ranking del especimen *)
    fun solicitarRank () = let
        val _ = print ("\nRanking: ")
        val num = eliminarSaltosDeLinea (obtenerEntradaUsuario())
    in 
        if esEntero num
        then Int.toString (valOf (Int.fromString num))
        else
            let 
                val _ = print "\nIngrese un valor válido!" 
            in 
                solicitarRank () 
            end
    end

    val rank = solicitarRank ();
    val clase = solicitarEntradaNoNumerica "Clase";
    val orden = solicitarEntradaNoNumerica "Orden";
    val especie = solicitarEntradaNoNumerica "Especie";
    val altura = solicitarNumero "Altura";
    val peso = solicitarNumero "Peso";
    val _ = TextIO.output(fd, "\n" ^ rank ^ "," ^ clase ^ "," ^ orden ^ "," ^ especie ^ "," ^ altura ^ "," ^ peso) 
            handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
in
    ()
end;


(* menuPrincipal
   Entrada: ruta -> la ruta del archivo de índice
   Salida: ninguna
   Restricciones: ninguna
   Objetivo: Despliega un menú al usuario para que elija qué operación llevar a cabo.
*)
fun menuPrincipal (ruta) = let
    val opcionesMenu = ["1. Agregar especimen", 
                        "2. Limpiar indice", 
                        "3. Salir"]
    val menu = print ("\n\n\n" ^ String.concatWith "\n" opcionesMenu ^ "\n\nEscoja una opcion: ")
    val opcion = eliminarSaltosDeLinea (obtenerEntradaUsuario ())        
in 
    if opcion = "1" then
        agregarNuevoEspecimen ruta
    else if opcion = "2" then
        crearYLimpiarIndice ruta
    else if opcion = "3" then
        OS.Process.exit(OS.Process.success)
    else 
        menuPrincipal ruta
end;


(* main
   Entrada: ninguna
   Salida: ninguna
   Restricciones: ninguna
   Objetivo: Solicita al usuario la dirección del archivo y luego inicia el programa.
*)
fun main () = 
let
    (* Solicitar la ruta del archivo al usuario *)
    val _ = print"Ingrese la ruta del archivo: \t"
    val ruta = eliminarSaltosDeLinea (obtenerEntradaUsuario ())
in 
    (* Bucle principal: ejecutar el menú mientras el usuario no decida salir *)
    while true do (
        menuPrincipal ruta
    )
end;
