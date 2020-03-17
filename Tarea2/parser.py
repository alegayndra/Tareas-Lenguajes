# Implementación de un parser
# Reconoce expresiones mediante la gramática:
# EXP -> EXP op EXP | EXP -> (EXP) | cte
# la cual fué modificada para eliminar ambigüedad a:
# EXP  -> cte EXP1 | (EXP) EXP1
# EXP1 -> op EXP EXP1 | vacío
# los elementos léxicos (delimitadores, constantes y operadores)
# son reconocidos por el scanner
#
# Autor: Dr. Santiago Conant, Agosto 2014 (modificado Agosto 2015)

# A00822649 | Alberto García Viegas
# A01634310 | Diego Estrada Talamantes

import sys
import obten_token as scanner

# Empata y obtiene el siguiente token


def match(tokenEsperado):
    global token
    if token == tokenEsperado:
        token = scanner.obten_token()
    else:
        error("token equivocado")

# Función principal: implementa el análisis sintáctico


def parser():
    global token
    token = scanner.obten_token()  # inicializa con el primer token
    exp()
    if token == scanner.END:
        print(">>ENTRADA CORRECTA<<")
    else:
        error(">>ERROR SINTACTICO<<")

# Módulo principal


def exp():
    exp1()
    exp2()

# Módulo auxiliar para reconocimiento de expresiones


def exp1():
    if token == scanner.BKS:  # busca un backward slash para el caso de \var.
        match(token)
        match(scanner.VAR)
        match(scanner.PNT)
    elif token == scanner.LRP:  # busca que se abra un parentesis
        match(token)
        exp()
        match(scanner.RRP)
    # checa qu   e se tenga algún lexico aceptable para seguir leyendo
    elif token == scanner.VAR or token == scanner.INT or token == scanner.OPB:
        match(token)

# Módulo auxiliar que permite la recursión


def exp2():
    # checa que se tenga algún lexico aceptable para seguir leyendo
    if token == scanner.VAR or token == scanner.INT or token == scanner.OPB or token == scanner.LRP or token == scanner.BKS:
        exp()
        exp2()

# Termina con un mensaje de error


def error(mensaje):
    print("ERROR:", mensaje)
    sys.exit(1)
