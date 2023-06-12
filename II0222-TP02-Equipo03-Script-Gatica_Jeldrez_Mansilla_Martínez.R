
# Asignatura: IIO222-1: Programación Aplicada
# Grupo 3
# EDA sobre valores hidrológicos en R. Elaborado por: Juan Gatica Díaz (20911094620), Bastián Jeldrez Calfiñir (21233158921),
# Jorge Mansilla Alvarado (20318536719) y Bryan Martínez Osorio (20978501321).
# Fecha de entrega: Mi 24-May-2023
#---------------------------------------------------------------------------------------------------------------------------------
# Variable con la dirección de los datos

wdirectory <- "C:/Users/basti/OneDrive/Documentos"
# Definir directorio de trabajo

setwd(wdirectory)

# Para este trabajo se utilizará el paquete zoo, xts, hydroTSM para instalar y llamar a los paquetes. 
# Cargar el script con las funciones

source("Funcionesv6.R")

verificar_paquete("zoo")
verificar_paquete("xts")
verificar_paquete("hydroTSM")

#Llamando a la función "carga_de_datos" se obtienen los datos que se encuentran.
# en los archivos cr2_prDaily_2018.txt y cr2_prDaily_2018_stations.txt.

Datos <- carga_de_datos("/cr2_prDaily_2018.txt")

estaciones <- carga_de_datos("/cr2_prDaily_2018_stations.txt")

# Pedirle al usuario la información del ID.

read_codigo <- readline(prompt = "¿En cuál estación desea ver los datos de precipitación? 
                             El ID debe tener el siguiente formato: <1000005>: ")

read_codigo <- verificar_codigo(read_codigo = read_codigo)
# Tabla con las precipitaciones y las fechas con el -9999.
nueva_tablaNA <- prec_fech(x = read_codigo)


# Tabla de precipitaciones y las fechas sin el -9999

nueva_tabla_filtrada <- prec_fechC(x = read_codigo)

read_codigo <- ajuste_codigo(codigo = read_codigo)

#número de estaciones

nestaciones <- nrow(estaciones)

#Fila en la que están todos los códigos de las estaciones.

fila_codigos <- colnames(Datos)


#Posición del código en la fila de los códigos

posicion_codigo <- which(fila_codigos == read_codigo)

# Función para definir secuencias temporales

# Función para definir secuencias temporales y generar gráficos

generacion_graficos <- graficos()
exportar_graficos <- exportar_graficospdf()
exportar_graficos

# Cálculo del rango intercuartil.
rango_intercuantil <- rang_inter(nueva_tabla_filtrada)

# Desviación estándar.
desviacion_estandar <- desviacion_est(nueva_tabla_filtrada)

# La medición de el valor mínimo, el primer cuartil, la media, el tercer cuartil y el valor máximo.
valores <- mediciones(nueva_tabla_filtrada)

# Cantidad de datos faltantes.
datos_faltantes <- datos_falt(read_codigo)
