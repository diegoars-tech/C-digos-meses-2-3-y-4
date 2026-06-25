###
#Función que busca de manera masiva todas las trayectorias de seguimiento de
#comunidades dado un umbral. Devuelve una lista de trayectorias. Recibe un 
#segundo umbral que compara la comunidad seguida en la ventana t, con la primera
calculatrayectorias <- function(umbral, listaBMJ, ventanas, umbral2){
  lista <- listaBMJ
  trayectorias <- list()
  for( i in  1: length(lista) ){
    for( j in 1:length(lista[[i]])){
      if(lista[[i]][[j]][1] != 0 && lista[[i]][[j]][3] > umbral ){
        x <- lista[[i]][[j]][2]
        c <- c(i, lista[[i]][[j]][1], x )
        if(i < length(lista)){
          for(k in (i+1): length(lista)){
            bandera <- TRUE
            for(l in 1: length(lista[[k]])){
              if(x == lista[[k]][[l]][1] && lista[[k]][[l]][3] > umbral ){
                comunidadOriginal <- ventanas[[i]][[j]]
                comunidadActual <- ventanas[[k]][[l]]
                jaccard <- length(intersect(comunidadOriginal, comunidadActual)) / length(union(comunidadOriginal, comunidadActual))
                if(jaccard >= umbral2){
                  x <- lista[[k]][[l]][2]
                  c <- c(c, x)
                  lista[[k]][[l]] <- c(0,0,0)
                  bandera <- FALSE
                  break
                }
              }
            }
            if(bandera){
              break
            }
          }
        }
        trayectorias <- c(trayectorias, list(c))
      }
    }
  }
  return(trayectorias)
}

###
#Función que busca de manera masiva todas las trayectorias de seguimiento de
#comunidades dado un umbral. Devuelve una lista de trayectorias
seguirTrayectorias <- function(listaSublistas, umbral) {
  trayectorias <- list()
  
  for (t in seq_along(listaSublistas)) {
    while (length(listaSublistas[[t]]) > 0) {
      
      vecActual <- listaSublistas[[t]][[1]]
      listaSublistas[[t]][[1]] <- NULL
      trayectoria <- c(t, vecActual[1])
      
      if (vecActual[3] >= umbral && t < length(listaSublistas)) {
        siguienteId <- vecActual[2]
        
        for (t2 in seq(t + 1, length(listaSublistas))) {
          
          posiciones <- which(
            sapply(listaSublistas[[t2]], function(v) v[1]) == siguienteId
          )
          
          # No encontrado: la comunidad ya fue consumida o no existe en esta ventana
          if (length(posiciones) == 0) break
          
          vecSig <- listaSublistas[[t2]][[posiciones[1]]]
          listaSublistas[[t2]][[posiciones[1]]] <- NULL
          
          trayectoria <- c(trayectoria, vecSig[1])
          
          # Umbral no superado: cortar cadena aquí
          if (vecSig[3] < umbral) break
          
          # Última ventana alcanzada con umbral superado:
          # agregar el j final (comunidad en t+1 que no aparece como i en ninguna sublista)
          if (t2 == length(listaSublistas)) {
            trayectoria <- c(trayectoria, vecSig[2])  
            break
          }
          
          siguienteId <- vecSig[2]
        }
      }
      
      trayectorias[[length(trayectorias) + 1]] <- trayectoria
    }
  }
  return(trayectorias)
}
###
#Funcion auxiliar de seguirTrayectorias que agrega el ultimo seguimiento 
#de las comunidades de la ultima ventana
ultimaComparacion <- function(trafilt, jaccards, umbral){
  trayectorias <- trafilt
  num <- length(jaccards)
  x <- length(jaccards[[num]])
  n <- (length(trayectorias)) - x
  
  for( i in n:length(trayectorias) ){
    y <- trayectorias[[i]]
    if(y[1] == num){
      for( j in 1:x){
        if(y[2] == jaccards[[num]][[j]][1] && jaccards[[num]][[j]][3] > umbral){
          y <- c(y ,jaccards[[num]][[j]][2] )
          trayectorias[[i]] <- y
        }
      }
    }
  }
  return(trayectorias)
}

###
#Funcion que determina si un seguimiento es sobre comunidades de igual o menos
#de n vertices. Si sí, los elimina de las trayectorias
descarte <- function(trayectorias, ventanas, n){
  modificaciones <- trayectorias
  for(i in 1:length(trayectorias)){
    c <- 0
    x <- trayectorias[[i]][1]
    y <- trayectorias[[i]][2]
    if(length(ventanas[[x]][[y]]) <= n ){
      for(j in  2:length(trayectorias[[i]]) ){
        b <- trayectorias[[i]][j]
        if(length(ventanas[[x]][[b]]) <= n){
          c <- c + 1
        }
        x <- x + 1
      }
      if( (length(trayectorias[[i]]) -1 ) == c ){
        modificaciones[[i]] <- NA
      }
    }
  }
  final <-  modificaciones[!is.na(modificaciones)]
  return(final)
}
###
#Funcion que realiza el ultimo filtro: Elimina las trayectorias que solo tienen
#longitud 1 (la comunidad solo aparece en una ventana)
seguimiento <- function(trayectorias, n){
  modificaciones <- trayectorias
  for(i in 1: length( trayectorias ) ) {
    
    if( length(trayectorias[[i]]) <= n){
      modificaciones[[i]] <- NA
    }
  }
  final <-  modificaciones[!is.na(modificaciones)]
  return(final)
}


#-----------------------------------------------------
#Asignamos el Archivo .rds generado con el script RastreoMasivo.R
setwd("C:/Users/IIEc/Documents/Codigos")
jaccards <- readRDS("jaccardstotales.rds")
ventanas <- readRDS("comunidadesporventana.rds")
#-----------------------Uso de funciones ------------------------------
mistrayectorias <- calculatrayectorias(.65, jaccards, ventanas, .33)
prueba5 <- seguirTrayectorias(jaccards, .65)


prueba6 <- seguimiento(prueba5, 2)
t2 <- seguimiento(mistrayectorias, 2)

t <- descarte(mistrayectorias, ventanas, 3)
prueba7 <- descarte(prueba6, ventanas, 3)

for(i in 1: length(prueba7)){
  print("---------")
  print( prueba7[[i]] )
  print(t[[i]])
  print("---------")
}

#-------------------------------------------------------------------------
###
#Funciones de Analisis de resultados
###
#Funcion que determina la ventana con más nacimientos de comunidades de interés
#(interés: Número de vértices no trivial y seguimiento mayor a 3 ventanas)
nacimientosPorVentana <- function(trayectoriasFiltradas, n){
  c <- 0
  v <- c()
  for(i in 1: (n-1)){
    for(j in 1: length(trayectoriasFiltradas)){
      if( trayectoriasFiltradas[[j]][1] == i  ){
        c <- c+1
      }
    }
    v <- c(v, c)
    c <-0
  }
  return(v)
}
#Funcion que determina la ventana con más nacimientos de comunidades de interés
#(interés: Número de vértices no trivial y seguimiento mayor a 3 ventanas)
muertesPorVentana <- function(trayectoriasFiltradas, n){
  c <- 0
  v <- c()
  for(i in 1: (n-1)){
    for(j in 1: length(trayectoriasFiltradas)){
      if( (trayectoriasFiltradas[[j]][1] + (length(trayectoriasFiltradas[[j]]) -1) ) == i  ){
        c <- c+1
      }
    }
    v <- c(v, c)
    c <-0
  }
  return(v)
}

muertes <- muertesPorVentana(mistrayectorias, 215)
nacimientos <- nacimientosPorVentana(mistrayectorias, 215)

library(ggplot2)
datos <- data.frame(tiempo = 1:214, 
                    valor = nacimientos, 
                    metrica = "Nacimientos")
datos2 <- data.frame(tiempo = 1:214, 
                     valor = muertes, 
                     metrica = "Muertes")

df_completo <- rbind(datos, datos2)
ggplot(df_completo, aes(x = tiempo, y = valor, color = metrica)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "Valor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

