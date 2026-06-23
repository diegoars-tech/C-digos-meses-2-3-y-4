library(igraph)
library(ggplot2)
 
###Funcion que separa en listas cada comunidad de cada ventana
obtenComunidadesPorVentana <- function(obj) {
  comunidadesVentana <- list()
  ids_comunidades <- unique(obj)
  
  for (j in ids_comunidades) {
    nombre <- as.character(j)          
    comunidadesVentana[[nombre]] <- list()
    t <- 1
    for (i in 1:296) {
      if (obj[i] == j) {
        comunidadesVentana[[nombre]][[t]] <- names(obj)[i]
        t <- t + 1
      }
    }
  }
  
  return(comunidadesVentana)
}
###
#funcion que devuelve el indice de Jaccard entre cada pareja de comunidades 
#de dos ventanas distintas. Descarta vértices que son su propia comunidad en
#t y se mantienen asi en t+1
jaccardentreVentanas <- function(ventanat, ventanatmas1){
  indicesJ <- list()
  y <- 1
  idst <- as.numeric(names(ventanat))
  idstmas1 <- as.numeric(names(ventanatmas1))
  for(i in 1:length(ventanat)){
    for(j in 1:length((ventanatmas1))){
      if(length(ventanat[[i]]) > 1 || length(ventanatmas1[[j]]) > 1){
        x <- length(intersect(ventanat[[i]], ventanatmas1[[j]])) / length(union(ventanat[[i]], ventanatmas1[[j]]))
        if(x != 0){
          v <- c(idst[i], idstmas1[j], x)
          indicesJ[[y]] <- v
          y <- y+1
        }
      }
    }
  }
  return (indicesJ)
}

###Funcion que identifica el indice más alto de una comunidad
#en t, con otra comunidad en t+1
#Adaptado a jaccardentreVentanas que descarta solitarios
comunidadEnTmas1 <- function(indices) {
  indicesAltos <- list()
  mejorVec <- indices[[1]]
  
  for (i in 1:(length(indices) - 1)) {
    y <- indices[[i]]
    z <- indices[[i + 1]]
    # Actualizar el mejor vector del grupo actual
    if (y[3] > mejorVec[3]) {
      mejorVec <- y
    }
    #Si cambia el grupo guardamos el mejor
    if (y[1] != z[1]) {
      indicesAltos <- c(indicesAltos, list(mejorVec))
      mejorVec <- z  #nuevo grupo con z
    }
  }
  #Verificar el último elemento contra el mejor identificado
  ultimo <- indices[[length(indices)]]
  if (ultimo[3] > mejorVec[3]) mejorVec <- ultimo
  indicesAltos <- c(indicesAltos, list(mejorVec))
  
  return(indicesAltos)
}

###Funcion que calcula la metrica basada en jaccardbestmatch y lo devuelve en
#un data frame. Trabaja con las ventanas en el formato de la funcion 
#"obtenComunidadesPorVentana"
calculajaccardbestmatch <- function(red_separada){
  listamayorJaccardtatmasuno  <- list()
  for(i in 1:(length(red_separada) - 1)){
    jac <- jaccardentreVentanas(red_separada[[i]], red_separada[[i + 1]])
    listamayorJaccardtatmasuno[[i]] <- comunidadEnTmas1( jac )
  }
  
  listamayorJaccardtmasunoat  <- list()
  for(i in 1:(length(red_separada) - 1 )){
    jac <- jaccardentreVentanas(red_separada[[i + 1]], red_separada[[ i ]])
    listamayorJaccardtmasunoat[[i]] <- comunidadEnTmas1( jac )
  }
  
  vector_fac <- c()
  for(i in 1: (length(red_separada) - 1) ){
    c <- 0
    for(j in 1: length(listamayorJaccardtatmasuno[[i]])){
      c <- c + listamayorJaccardtatmasuno[[i]][[j]][3]
    }
    precision <- c / length(listamayorJaccardtatmasuno[[i]])
    
    c2 <- 0
    for(j in 1: length(listamayorJaccardtmasunoat[[i]])){
      c2 <- c2 + listamayorJaccardtmasunoat[[i]][[j]][3]
    }
    recall <- c2 / length(listamayorJaccardtmasunoat[[i]])
    
    fac <- (2 * precision * recall)/(precision + recall)
    vector_fac <- c(vector_fac, fac)
  } 
  return(vector_fac) 
}

###Uso de funciones
#bajando en una lista las membresias con comunidades pequeñas
miLista  <- readRDS("Copia de vector_membresia_por_ventana_fil_080.rds")
listamembresias <- list()
for(i in 1:215){
  listamembresias[[i]] <- miLista[[i]]
}
#Preparando comunidades para calcular Jaccard
comunidadesporventana <- list()
for(i in 1:215 ){
  comunidadesporventana[[i]] <- obtenComunidadesPorVentana(listamembresias[[i]])
}
#Uso de la funcion final
sumaJactotal <- calculajaccardbestmatch(comunidadesporventana)
 
##calulando proporcion de  numero de vertices que eran su propia comunidad 
# y continuaron siendolo 
verticesSolitarios <- c()   
for(i in 1:(length(comunidadesporventana)-1)){
  c <- 0
  for(j in 1:length(comunidadesporventana[[i]])){
    for(k in 1:length(comunidadesporventana[[i+1]])){
      if(length(comunidadesporventana[[i]][[j]]) == 1 && length(comunidadesporventana[[i+1]][[k]]) == 1 ){
        x <- length(intersect(comunidadesporventana[[i]][[j]], comunidadesporventana[[i+1]][[k]]))
        if(x == 1){
          c <- c+1
        }
      }
    }
  }
  c <- c/296
  verticesSolitarios <- c(verticesSolitarios, c)
}


###
#plot de la metrica con los solitarios
dfec <- read.csv("fechas_fil_080.csv", stringsAsFactors = FALSE)
fechas <-  dfec[1:214, 2]
fechas2 <- as.Date(fechas)

df1 <- data.frame(tiempo = fechas2, 
                  valor = sumaJactotal, 
                  metrica = "Metrica")

#Para solitarios son 214
df2 <- data.frame(tiempo = fechas2, 
                  valor = verticesSolitarios, 
                  metrica = "Proporción vértices solitarios")

df_completo <- rbind(df1, df2)

ggplot(df1, aes(x = tiempo, y = valor, color = metrica)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = NULL, y = "Valor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###
# Proporcion de vertices no solitarios para cada ventana t
proporcionNoSolitarios <- list()   
for(i in 1:(length(comunidadesporventana))){
  c <- 0
  for(j in 1:length(comunidadesporventana[[i]])){
    if(length(comunidadesporventana[[i]][[j]])== 1){
      c <- c+1
    }
  }
  c <- (296-c)/296
  proporcionNoSolitarios[[i]] <- c
}

###
#Calculo de media geometrica de la proporcion entre ventanas
mediageometrica <- c()
for(i in 1: (length(comunidadesporventana)-1)){
  c <- sqrt(proporcionNoSolitarios[[i]] * proporcionNoSolitarios[[i+1]])
  mediageometrica <- c(mediageometrica, c)
}

###Metrica afectada por la media geometrica de proporcion de vertices
metricamodificada <- c()
for(i in 1: (length(comunidadesporventana)-1)){
  c <- sumaJactotal[[i]] * mediageometrica[i]
  metricamodificada <- c(metricamodificada, c)
}

#Para solitarios son 214
df3 <- data.frame(tiempo = fechas2, 
                  valor = metricamodificada, 
                  metrica = "Métrica modificada")

df_completo2 <- rbind(df1, df3)

ggplot(df3, aes(x = tiempo, y = valor, color = metrica)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = NULL, y = "Valor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




