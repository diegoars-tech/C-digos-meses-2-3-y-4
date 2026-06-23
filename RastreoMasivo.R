ventanas <- comunidadesporventana
###
#funcion que devuelve el indice de Jaccard entre cada pareja de comunidades 
#de dos ventanas distintas
jaccardentreVentanas2 <- function(ventanat, ventanatmas1){
  indices <- list()
  y <- 1
  for(i in 1:length(ventanat)){
    for(j in 1:length((ventanatmas1))){
      x <- length(intersect(ventanat[[i]], ventanatmas1[[j]])) / length(union(ventanat[[i]], ventanatmas1[[j]]))
      v <- c(i, j, x)
      indices[[y]] <- v
      y <- y+1
    }
  }
  indices1 <- list()
  z <- 1
  for(i in 1: length(indices)){
    if(indices[[i]][3] != 0 ){
      indices1[[z]] <- indices[[i]]
      z <- z+1
    }
  }
  return (indices1)
}
#prueba <- jaccardentreVentanas2(ventanas[[107]], ventanas[[108]])

#Funcion auxiliar que separa las comparaciones de una comunidad.
#Devuelve una lista de listas. Cada sublista contiene  comparaciones de una 
#comunidad contra todas en t+1
separaComunidad <- function(indices){
  keys <- sapply(indices, function(v) v[1])    
  split(indices, keys)                          
}
#prueba2 <- separaComunidad(prueba)

###Funcion que identifica el indice más alto de una comunidad
#en t, con otra comunidad en t+1
#numcom son el numero de comunidades en la ventana t
bestMatchFiltro <- function(indicesSeparados){
  indicesAltos <- list()
  y <- 1
  for(i in 1:length(indicesSeparados)){
    x <- 0
    for(j in 1: length(indicesSeparados[[i]])){
      if(x < indicesSeparados[[i]][[j]][3]){
        x <- indicesSeparados[[i]][[j]][3]
        indicesAltos[[y]] <- indicesSeparados[[i]][[j]]
      }
    }
    y <- y+1
  }
  return(indicesAltos)
}
#prueba3 <- bestMatchFiltro(prueba2)

###
#Funcion que hace el calculo BestMatch para todas las ventanas
#Hace uso de las funciones anteriores para todas las comparaciones entre ventanas

jaccardbestMatchMasivo <- function(ventanas){
  jaccardscompletos <- list()
  for(i in 1:(length(ventanas)-1)){
    aux <- jaccardentreVentanas2(ventanas[[i]], ventanas[[i+1]])
    aux2 <- separaComunidad(aux)
    jaccardscompletos[[i]] <- bestMatchFiltro(aux2)
  }
  return(jaccardscompletos)
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
#Funcion que determina si un seguimiento es sobre comunidades de un solo vertice
descarte <- function(trayectorias, ventanas){
  modificaciones <- trayectorias
  for(i in 1:length(trayectorias)){
    c <- 0
    x <- trayectorias[[i]][1]
    y <- trayectorias[[i]][2]
    if(length(ventanas[[x]][[y]]) <= 4 ){
      for(j in  2:length(trayectorias[[i]]) ){
        b <- trayectorias[[i]][j]
        if(length(ventanas[[x]][[b]]) <= 4){
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

####
#Funcion auxiliar que agrega el ultimo seguimiento de las comunidades de la ultima 
#ventana
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
#Función que determina la calidad de una trayactoria comparando Jaccards de la
#la comunidad en la primer ventana con las siguientes seguidas


###Uso de funciones -----------------------

prueba4 <- jaccardbestMatchMasivo(ventanas)

prueba5 <- seguirTrayectorias(prueba4, .7)

prueba6 <- descarte(prueba5, ventanas)

prueba8 <- ultimaComparacion(prueba6, prueba4, .7 )

prueba7 <- seguimiento(prueba8, 2)

length(prueba7)
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

muertes <- muertesPorVentana(prueba8, 215)
nacimientos <- nacimientosPorVentana(prueba8, 215)

#library(ggplot2)
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


