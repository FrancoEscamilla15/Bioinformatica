#PROYECTO FINAL - Enrique Franco García - 259366

####PUNTOS POR CARRERA####
# Primer lugar: 25
# Segundo lugar: 18
# Tercer lugar: 15
# Cuarto lugar: 10
# Quinto lugar: 8
# Sexto lugar: 6
# Septimo lugar: 5
# Octavo lugar: 3
# Noveno lugar: 2
# Decimo lugar: 1


####INSTRUCCIONES####
#El primer paso escribir en la consola "diferencia", esto es para calcular la diferencia
#de puntos existente entre dos pilotos o equipos, se solicitaran los puntos de un piloto o equipo
#y posteriormente los del otro, en caso de no conocer los puntos estos se encuentran escritos en
#"puntos pilotos" y "puntos equipos", se realizara automaticamente la resta de los puntos y una vez 
#se tenga la diferencia se debe escribir "pilotos" o "equipos", según sea el caso necesario, para ambos
#casos primero se solicitara la diferencia de puntos previamente calculada y después se solicitaran
#los puntos que obtuvieron los pilotos o equipos en la ultima carrera, con esto se realizara la operación 
#correspondiente y se arrojara la nueva diferencia de puntos con los puntos obtenidos en la ultima carrera
####PUNTOS DE PILOTOS####
#Max Verstappen: 332.5
#Lewis Hamilton: 318.5
#Valtteri Bottas: 203
#Sergio Perez: 178
#Lando Norris: 151
#Charles Leclerc: 148
#Carlos Sainz: 139.5
#Daniel Ricciardo: 105
#Pierre Gasly: 92
#Fernando Alonso: 62
#Esteban Ocon: 50
#Sebastian Vettel: 42
#Lance Stroll: 26
#Yuki Tsunoda: 20
#George Russell: 16
#Kimi Raikkonen: 10
#Nicholas Latifi: 7
#Antonio Giovinazzi: 1


####PUNTOS DE EQUIPOS####
#Mercedes: 521.5
#Red_bull: 510.5
#Ferrari: 287.5
#McLaren: 256
#Alpine: 112
#Alpha Tauri: 112
#Aston Martin: 68
#Williams: 23
#Alfa Romeo: 11



####CALCULAR LA DIFERENCIA DE PUNTOS O EQUIPOS####
diferencia <- function(){
  piloto1 <- readline(prompt = "Ingresa los puntos de un primer piloto/equipo:  ")
  piloto1 <- as.numeric(piloto1)
  piloto2 <- readline(prompt = "Ingresa los puntos de un segundo piloto/equipo:  ")
  piloto2 <- as.numeric(piloto2)
  diferencia <- piloto1 - piloto2
  
  return(print(paste("La diferencia de puntos es igual a: ", 
                     diferencia)))
}
#Cree un ciclo for el cual preguntara la cantidad de puntos de ambos pilotos de interes para calcular
#la diferencia entre ambos, los nombre "piloto 1 y 2" y ambos los modificara para que fueran numericos 
#y de esta manera R no tuviera problema con realizar la operación, posteriormente cree un objeto
#que tuviera la operación de mi interes en este caso la resta de puntajes entre el piloto 1 y el 2
#para terminar el ciclo le pedí que imprimiera que la diferencia de puntos era igual al objeto que contenía
#mi operación

####DIFERENCIA DE PUNTOS DE PILOTOS####
pilotos <- function(){
  dif1 <- readline(prompt = "¿Cual es la diferencia de puntos entre tus pilotos?: ")
  dif1 <- as.numeric(dif1)
  pp1 <- readline(prompt = "¿Cuantos puntos sumo tu primer piloto en la ultima carrera?: ")
  pp1 <- as.numeric(pp1)
  pp2 <- readline(prompt = "¿Cuantos puntos sumo tu segundo piloto en la ultima carrera?: ")
  pp2 <- as.numeric(pp2)
  1 <- dif1 + pp1 - pp2
  print(return(print(paste("La diferencia de puntos ahora es: ", 1))))
}
#Para calcular la nueva diferencia de puntos entre pilotos nuevamente cree un ciclo for en el cual 
#en primer lugar se solicita la diferencia de puntos que hay entre pilotos y posteriormente cuantos puntos
#sumaron ambos pilotos en la ultima carrera, estos tres objetos los converti para que fueran numericos y R 
#no tuviera problema con realizar la operación. Cree un objeto llamado 1, el cual contiene la operación de mi interes
#que es dif1 (la diferencia de puntos que se solicito) mas los puntos que sumo el primer piloto menos los puntos
#que sumo el segundo piloto porque quiero calcular la diferencia de puntos. Por ultimo le pedí que imprimiera
#la diferencia de puntos a partir del objeto 1 que contiene la operación necesaria para saber esta diferencia.

####CALCULAR LOS PUNTOS TOTALES DEL EQUIPO####
sumatoria <- function(){
  puntos <- readline(prompt = "¿Cuantos puntos tiene tu equipo?: ")
  puntos <- as.numeric(puntos)
  pil1 <- readline(prompt = "Ingresa los puntos que gano el primer piloto del equipo: ")
  pil1 <- as.numeric(pil1)
  pil2 <- readline(prompt = "Ingresa los puntos que gano el segundo piloto del equipo: ")
  pil2 <- as.numeric(pil2)
  suma <- puntos + pil1 + pil2
  return(print(paste("Tu equipo tiene: ", suma)))
}
#En caso de que sea necesario conocer cual es la cantidad de puntos que tiene un equipo despues de una carrera
#se debe imprimir "sumatoria", este a paritr de un ciclo for preguntara los puntos del equipo y cuantos puntos sumaron los dos pilotos del
#equipo en la ultima carrera, sumará los puntos totales mas los puntos de ambos pilotos dando como resultado 
#la nueva cantidad de puntos del equipo después de una carrera.


####DIFERENCIA DE PUNTOS DE EQUIPOS####
equipos <- function(){
  dif2 <- readline(prompt = "¿Cual es la diferencia de puntos entre tus equipos?: ")
  dif2 <- as.numeric(dif2)
  e1 <- readline(prompt = "¿Cuantos puntos sumo tu primer equipo en la ultima carrera?: ")
  e1 <- as.numeric(e1)
  e2 <- readline(prompt = "¿Cuantos puntos sumo tu segundo equipo en la ultima carrera?: ")
  e2 <- as.numeric(e2)
  2 <- dif1 + e1 - e2
  print(return(print(paste("La diferencia de puntos ahora es: ", 2))))
}
#Al igual que cuando se quería calcular la diferencia de pilotos, cree un ciclo for que en primer lugar
#preguntara la diferencia de puntos entre dos equipos, este objeto se convirtio en numerico para después hacer la
#operación correspondiente, posteriormente se preguntaran los puntos que sumaron tanto el primer equipo como el segundo
#en la ultima carrera (ambos objetos también se convirtieron a numericos) y se creo un objeto que contenia
#la operación necesaria para calcular la nueva diferencia, esta es: el valor de la diferencia ingresada por el usuario
#mas los puntos sumados del primer equipo menos los puntos sumados del segundoe equipo, y por ultimo 
#R imprime esta operación de manera que da como resultado la nueva diferencia de puntos 