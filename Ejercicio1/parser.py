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

#   A00822649 | Alberto García Viegas
#   A01634310 | Diego Estrada Talamantes

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
    asg()
    exp()
    if token == scanner.END:
        print("Expresion bien construida!!")
    else:
        error("expresion mal terminada")


def asg():
    if token == scanner.PLB:
        match(token)
        asg1()


def asg1():
    if token == scanner.ASG or token == scanner.OPB:
        match(token)
    else:
        error("falta operador")

# Módulo que reconoce expresiones


def exp():
    if token == scanner.PLB:
        match(token)
        if token == scanner.LRP:  # checar que haya un (
            match(token)
            arg()
            match(scanner.RRP)
        exp1()
    elif token == scanner.INT or token == scanner.FLT:
        match(token)  # reconoce Constantes
        exp1()
    elif token == scanner.LRP:
        match(token)  # reconoce Delimitador (
        exp()
        match(scanner.RRP)
        exp1()
    else:
        error("expresion mal iniciada")

# Módulo auxiliar para reconocimiento de expresiones


def exp1():
    if token == scanner.OPB:
        match(token)  # reconoce operador
        exp()
        exp1()


def arg():
    exp()
    arg1()


def arg1():
    if token == scanner.CMA:
        match(token)
        arg()

# Termina con un mensaje de error


def error(mensaje):
    print("ERROR:", mensaje)
    sys.exit(1)
