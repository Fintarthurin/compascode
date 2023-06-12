#Equipo: n° 1 
#Profesor: Mauricio Zambrano 
#Alumnos: Xabier Sáez 
#         Sophia Torres
#         Ítalo Ettori 
#Matrícula: 21159884021  
#           20710702620 
#           21164420621 
#Código curso: IIO222-1 
#Fecha: 24-05-2023

maestra <- function(){
#Cargar paquetes necesarios.
cargar_librerias <- function(){
  if (!require(hydroTSM)) install.packages("hydroTSM")
  if (!require(zoo)) install.packages("zoo")
  if (!require(xts)) install.packages("xts")
  if (!require(dplyr)) install.packages("dplyr")
}
#Establecer directorio, y asignación de una variable para la lectura del archivo.
directorio_y_lectura <-function(){
  #Establecer directorio.
  workspace <- "C:/Users/Italo/OneDrive/Escritorio"
  datos <- paste0(workspace, "/cr2_prDaily_2018.txt")
  #Variable para la lectura del archivo.
  rdata <- read.csv(datos)
  
  return(list(workspace, datos, rdata))
}
#Se le pide al usuario que digite una ID existente.
revision_de_id <- function() { 
  ID_valido <- FALSE
  
  while (!ID_valido) {
    ID <- readline("Por favor, digite una ID: ")
    
    ID_filter <- ifelse(nchar(ID) == 8, "X",
                        ifelse(nchar(ID) == 7, "X0", 
                               ifelse(nchar(ID) == 6, "X", "")))
    
    ID_con_filtro <- paste0(ID_filter, ID)
    if (ID_con_filtro %in% colnames(rdata)){ 
      ID_valido <- TRUE
        } else {
      cat("El ID ingresado no es válido. Por favor, ingrese un ID válido.\n")
    }
  }
  return(ID_con_filtro)
}
#Se procesan los datos, cambiando el formato de fecha y estableciendo títulos de columnas.
preprocesar_datos <- function(rdata){ 
  #Eliminar valores faltantes
  rdata <- rdata[-c(1:14),]
  rdata <- rdata %>% select(codigo_estacion, ID)
  datos_no_registrados <- rdata[rdata == -9999] <- NA
  datos_no_registrados<-sum(is.na(rdata))
  rdata[, 1] <- as.Date(rdata[, 1], format = "%Y-%m-%d", na.rm = TRUE)
  rdata[rdata == -9999] <- NA   
  rdata <- na.omit(rdata)
  rdata[, 2] <- as.numeric(rdata[, 2])
  
  #Extraer fecha inicial y final.
  fecha_inicio <- rdata[1, 1]
  fecha_final <- rdata[nrow(rdata), 1]
  
  #Crear objeto zoo a partir de los datos.
  w <- zoo(rdata[, 2], rdata[, 1])
  
  #Cambiar el nombres de las columnas a fecha.
  names(rdata)[1] <- "fecha"
  names(rdata)[2] <- "datos_estacion"
  
  return(list(datos = rdata, datos_zoo = w, fecha_inicio = fecha_inicio,
              fecha_final = fecha_final, datos_no_registrados))
}
#Selección de la serie temporal que se desea analizar.
seleccion_opcion <- function() {
  opciones <- c("diaria", "mensual", "anual", "estacional", "Salir")
  seleccion <- select.list(opciones, title = "Elija de qué serie temporal quiere 
  obtener la cantidad de datos no registrados, desviación estándar, rango intercuartil,
                           los valores de los cuartiles, mínimo y máximo: ")
  
  if (seleccion == "Salir") {
    cat("Ha seleccionado salir. Fin del programa.\n")
    return(NULL)
  } else {
    if (seleccion == "diaria") {
      x <- w
    } else if (seleccion == "mensual") {
      x <-daily2monthly(w, FUN = mean)
    } else if (seleccion == "anual") {
      x <- daily2annual(w, FUN = mean) 
    } else if (seleccion == "estacional") {
      x <- seasonalfunction.default(w, pfreq = "seasonal", FUN = mean, na.rm = TRUE, 
                                    type = "default",
                                    season.names = c("Summer", "Autumn", "Winter", "Spring"))   
      } 
    return(x)
  }
}
#Calculo de la serie temporal escogida.
calculo_estadistico <- function(x) {
  desv_cd <- sd(x, na.rm = TRUE)
  range_cd <- IQR(x, na.rm = TRUE)
  summary_datos <- summary(x)
  resultados <- list(desv_cd_ = desv_cd, range_cd = range_cd,
                     summary_datos = summary_datos)
  return(resultados)
}
#Graficar serie temporal en un rango especifico, digitado por el usuario.
graficar_serie_temporal <- function() {
  opcion <- menu(c("salir", "rango de fechas"), title="¿Desea digitar un rango de fechas?: ")
  
  if (opcion == 1) {
    cat("Ha seleccionado salir. Fin del programa.\n")
    return(NULL)
    
  } else if (opcion == 2) {
    inicio <- readline("Por favor, ingrese la fecha de inicio (YYYY-MM-DD): ")
    final <- readline("Por favor, ingrese la fecha final (YYYY-MM-DD): ")
    
    #Verificar si se ingresaron las fechas.
    if (nchar(inicio) == 0 || nchar(final) == 0) {
      cat("No se ingresaron fechas. Fin del programa.\n")
      return(NULL)
    }
    
    #Convertir las cadenas de texto a objetos de fecha.
    fecha_inicio <- as.Date(inicio)
    fecha_final <- as.Date(final)
    
    #Filtrar los datos dentro del rango especificado.
    w_subset <- w[as.Date(index(w)) >= fecha_inicio & as.Date(index(w)) <= fecha_final]
    
    #Verificar si hay datos en el rango.
    if (is.null(w_subset) || length(w_subset) == 0) {
      cat("No hay datos disponibles en el rango especificado.\n")
      return(NULL)
    }
    
    titulo <- paste("Serie temporal diaria (", inicio, " - ", final, ")")
    
    hydroTSM::hydroplot(w_subset, pfreq = "o", Fun = sum, main = titulo,
                        ylab = "Datos Estación (mm/d)", xlab = "Días de Registro")
    
    return(w_subset)
  }
}
#Menú opciones de grafica de usuario.
menu_opciones <- function() {
  opciones <- c("diaria", "mensual", "anual", "estacional", "Salir")
  seleccion <- select.list(opciones, 
                           title = "Seleccione el Grafico de la serie temporal que desea visualizar: ")
  if (seleccion == "Salir") {
    cat("Ha seleccionado salir. Fin del programa.\n")
    return(NULL)
  } else {
    cat("La opción seleccionada es:", seleccion, "\n")
    if (seleccion == "diaria") {
      grafico <- hydroplot(w, pfreq = "o", FUN = sum,
                          main = "Serie temporal diaria",
                          ylab = "Datos Estación (mm/d)",
                          xlab = "Días de Registro",
                          col = "green")
    } else if (seleccion == "mensual") {
      grafico <- hydroplot(w, pfreq = "ma", FUN = sum,
                main = "Serie temporal mensual",
                ylab = "Datos Estación (mm/d)",
                xlab = "Meses de Registro",
                ptype = "ts",
                col = "blue")
    } else if (seleccion == "anual") {
      grafico <- hydroplot(w, pfreq = "o", FUN = sum,
                          main = "Serie Temporal Anual",
                          xlab = "Años", ylab = "Datos Estación (mm/d)",
                          ptype = "ts",
                          col = "orange")
    } else if (seleccion == "estacional") {
      grafico <- hydroplot(w, pfreq = "seasonal", FUN = mean,
                          stype = "default",
                          season.names = c("Summer", "Autumn", "Winter", "Spring"))
    }
    else if (seleccion== graficar_serie_temporal()){
      grafico <- graficar_serie_temporal()
      graficar_serie_temporal()
    }
    return(grafico)
  }
}
#Tipo de formato para guardar el grafico.
guardar_grafico <- function(grafico, w_subset) {
  formato <- select.list(c("png", "pdf", "salir"), title = "Seleccione el formato y el Gráfico que desea guardar:")
  
  if (formato == "Salir") {
    cat("Ha seleccionado salir. Fin del programa.\n")
    return(NULL)
  } else if (formato == "png") {
    nombre_archivo <- readline("Escriba el nombre del archivo de la serie temporal que desea guardar en formato png: ")
    png(paste0(workspace, "/", nombre_archivo, "_grafico.png"), width = 1500, height = 1500, res = 200)
    grafico <- menu_opciones()
    w_subset <- graficar_serie_temporal()#print(grafico)#    #print(w_subset)#
    dev.off()
  } else if (formato == "pdf") {
    nombre_archivo <- readline("Escriba el nombre del archivo de la serie temporal que desea guardar en formato pdf: ")
    pdf(paste0(workspace, "/", nombre_archivo, "_grafico.pdf"))
    grafico <- menu_opciones()
    w_subset <- graficar_serie_temporal()#revisar porque hace que muestre las opciones 2 veces     #print(grafico)#    #print(w_subset)#
    dev.off()
  }
}

################################################################################
#Llamado a las funciones y sus resultados.

#funcion_maestra<- function(){# #da error al cargar el codigo

cargar_librerias()
retornos <- directorio_y_lectura()
workspace <- retornos[[1]]
datos <- retornos[[2]]
rdata <- retornos[[3]]
ID <- revision_de_id()

columna <- which(colnames(rdata) == ID)
if(length(columna) > 0){
 valor1 <- as.character(rdata[8, columna])
 valor2 <- as.character(rdata[3, columna])
  cat("El nombre de la cuenca a la cual corresponde esta ID: ", ID, "es ",valor1, " de la ciudad de ", valor2,"\n") 
}else{
  cat("No se encontró ninguna coincidencia para el ID: ", ID, "\n")
}

resultados <- preprocesar_datos(rdata)
datos_no_registrados<-resultados[[5]]+1 
datos <- resultados$datos
w <- resultados$datos_zoo
fecha_inicio <- resultados$fecha_inicio
fecha_final <- resultados$fecha_final
x <- seleccion_opcion()
calculo <- calculo_estadistico(x)
#Mostrar los resultados en la pantalla
cat("Cantidad de datos no registrados:", datos_no_registrados, "\n")
cat("Desviación estándar:", calculo$desv_cd_, "\n")
cat("Rango intercuartílico:", calculo$range_cd, "\n")
cat("Resumen estadístico:\n")
print(calculo$summary_datos)

x11()
graficar_serie_temporal()
x11()
menu_opciones()
guardar_grafico()
}

maestra()
