library(igraph)
library(ggplot2)
### Función que recibe un objeto membership de igraph y fusiona todas las
# comunidades con menos de `min_size` vértices en una sola comunidad,
# asignándoles el id de la primera comunidad pequeña encontrada.
fusionar_comunidades_pequeñas <- function(membresia, min_size) {
  
  # Tabla de frecuencias: cuántos vértices tiene cada comunidad
  tamaños <- table(membresia)
  
  # IDs de comunidades pequeñas (menos de min_size vértices)
  ids_pequeñas <- as.integer(names(tamaños[tamaños < min_size]))
  
  # Si no hay comunidades pequeñas, regresar el membership sin cambios
  if (length(ids_pequeñas) == 0) {
    return(membresia)
  }
  
  # ID común: el primero de los ids pequeños encontrados
  id_comun <- 100
  
  # Reasignar: todos los vértices en comunidades pequeñas reciben id_comun
  nueva_membresia <- membresia
  nueva_membresia[membresia %in% ids_pequeñas] <- id_comun
  
  return(nueva_membresia)
}

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
#de dos ventanas distintas
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
#numcom son el numero de comunidades en la ventana t
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
    
    # Si cambia el grupo (o es el último par), guardar el mejor
    if (y[1] != z[1]) {
      indicesAltos <- c(indicesAltos, list(mejorVec))
      mejorVec <- z  # iniciar nuevo grupo con z
    }
  }
  
  # Verificar el último elemento contra el mejor acumulado
  ultimo <- indices[[length(indices)]]
  if (ultimo[3] > mejorVec[3]) mejorVec <- ultimo
  indicesAltos <- c(indicesAltos, list(mejorVec))
  
  return(indicesAltos)
}

###Uso de funciones
#bajando en una lista las membresias con comunidades pequeñas
miLista  <- readRDS("C:/Users/IIEc/Documents/Copia de vector_membresia_por_ventana_fil_080.rds")
listamembresias <- list()
for(i in 1:215){
  listamembresias[[i]] <- miLista[[i]]
}
#Preparando comunidades para calcular Jaccard
comunidadesporventana <- list()
for(i in 1:215){
  comunidadesporventana[[i]] <- obtenComunidadesPorVentana(listamembresias[[i]])
}
#For para caclcular Jaccards de todas las ventanas con su siguiente
listaJaccards <- list()
for(i in 1:214){
  listaJaccards[[i]] <- jaccardentreVentanas(comunidadesporventana[[i]], comunidadesporventana[[i + 1]])
}

hola <- comunidadEnTmas1(listaJaccards[[61]])

#calculando jaccards totales por ventana
#excepto verices que eran su propia comunidad y continuaron siendolo 
#normalizado entre numero de ventanas
sumaJactotal <- c()   
for(i in 1:length(listaJaccards)){
  c <- 0
  if(length(listaJaccards[[i]]) > 0){
    for(j in 1:length(listaJaccards[[i]])){
      c <- c + listaJaccards[[i]][[j]][3]
    }
    c <- c/length(comunidadesporventana[[i]])
  }
  sumaJactotal <- c(sumaJactotal, c)
}

vectortiempos214 <- c()
for (i in 1:length(listaJaccards)) {
  vectortiempos214 <- c(vectortiempos214, i)
}

# Crear data frame
datos <- data.frame(
  tiempo = vectortiempos214,
  valor = sumaJactotal
)
ggplot(datos, aes(x = tiempo, y = valor)) +
  geom_line() +
  geom_point() +
  labs(x = "Fechas", y = "Valores")

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

vectortiempos215 <- c()
for (i in 1:length(comunidadesporventana)) {
  vectortiempos215 <- c(vectortiempos215, i)
}

# Crear data frame
datos <- data.frame(
  tiempo = vectortiempos214,
  valor = verticesSolitarios
)
ggplot(datos, aes(x = tiempo, y = valor)) +
  geom_line() +
  geom_point() +
  labs(x = "Fechas", y = "# de vértices")


#calculando numero de comunidades por ventana
numventanas <- c()
for (i in 1:length(comunidadesporventana)) {
  c <- length(comunidadesporventana[[i]])
  numventanas <- c(numventanas, c)
}

# Crear data frame
datos <- data.frame(
  tiempo = vectortiempos215,
  valor = numventanas
)
ggplot(datos, aes(x = tiempo, y = valor)) +
  geom_line() +
  geom_point() +
  labs(x = "Fechas", y = "# de Comunidades")



# Asumiendo que vectortiempos214 tiene fechas y misma longitud
df1 <- data.frame(tiempo = fechas2, 
                  valor = sumaJactotal, 
                  metrica = "Suma Jaccard normalizada")

# Para solitarios, necesitas un tiempo por cada punto (214 puntos)
df2 <- data.frame(tiempo = fechas2, 
                  valor = verticesSolitarios, 
                  metrica = "Proporción vértices solitarios")

df_completo <- rbind(df1, df2, df_nmi)

ggplot(df_completo, aes(x = tiempo, y = valor, color = metrica)) +
  geom_line() +
  geom_point() +
  labs(x = "Fechas", y = "Valor") +
  theme_bw()
 
 
ggplot(df_completo, aes(x = tiempo, y = valor, color = metrica)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(x = NULL, y = "Valor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


