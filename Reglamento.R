#PROYECTO FINAL - Enrique Franco García - 259366

####OBJETIVO####
#Generar un programa que a partir de los estandares de medidas permitidos 
#por la FIA en cada carro de formula1, se pueda determinar si el carro esta cumpliendo
#o no con dicho reglamento, esto se va a lograr ingresando los distintos rangos
#como condiciones y solicitando al usuario que ingrese los datos que posee de su
#carro.

####PARAMETROS####
parametros <- function(){
####ALERONES####
####ALERON TRASERO####
  altura_alatrasera <- readline(prompt = "Ingresa la medida de la altura de tu aleron trasero: ")
  ancho_alatrasera <- readline(prompt = "Ingresa la medida del ancho de tu aleron trasero: ")
  longitud_alatrasera <- readline(prompt = "Ingresa la medida de la longitud de tu aleron trasero: ")

  altura_alat <- if (altura_alatrasera <= 670){
    print("El aleron trasero tiene la altura adecuada")
  } else if (altura_alatrasera >= 671){
    print("La altura del aleron trasero es ilegal, se debe pagar una multa de 50,000 euros y en caso del piloto haber obtenido alguna ventaja será descalificado")
  }
  ancho_alat <- if (ancho_alatrasera <= 1050){
    print("El aleron trasero tiene la anchura adecuada")
  }else if (ancho_alatrasera >=1051){
    print("El ancho del aleron trasero es ilegal, se debe pagar una multa de
          50,000 euros y en caso del piloto haber obtenido alguna ventaja será descalificado")
  }
  longitud_alat <- if (longitud_alatrasera <= 810){
    print("El aleron trasero tiene la longitud adecuada")
  }else if (longitud_alatrasera >= 811){
    print("La longitud del aleron trasero es ilegal, se debe pagar una multa de
          50,000 euros y en caso del piloto haber obtenido alguna ventaja será descalificado")
  }
  
####ALERON FRONTAL####
  altura_alafrontal <- readline(prompt = "Ingresa la medida de la altura de tu ala delantera: ")
  ancho_alafrontal <- readline(prompt = "Ingresa la medida del ancho de tu ala delantera: ")
  longitud_alafrontal <- readline(prompt = "Inrgesa la medida de la longitud de tu ala delantera: ")
  
  altura_alaf <- if (altura_alafrontal <= 300){
    print("El ala frontal tiene la altura adecuada")
  } else if (altura_alafrontal >=301){
    print("La altura del ala frontal es ilegal, se debe pagar una multa de
          50,000 euros y ser modificado de inmediato")
  }
  
  ancho_alaf <- if (ancho_alafrontal <= 2000){
    print("El ala frontal tiene el ancho adecuado")
  } else if (ancho_alafrontal >= 2001){
    print("El ancho del ala frontal es ilegal, se debe pagar una multa de 
          50,000 euros y ser modificado de inmediato")
  }
  
  longitud_alaf <- if (longitud_alafrontal <= 775){
    print("El ala frontal tiene la longitud adecuada")
  } else if (longitud_alafrontal >= 776){
    print("La longitud del ala frontal es ilegal, se debe pagar una multa de 
          50,000 euros y ser modificado de inmediato")
  }
  
####DRS####
  ancho_DRS <- readline(prompt = "Ingrese la medida del ancho del DRS: ")
  
  anchura_DRS <- if (ancho_DRS >= 1008 & ancho_DRS <= 1020){
    print("El DRS tiene la anchura adecuada")
  }else if (ancho_DRS <= 1007 | ancho_DRS >= 1021){
    print("La anchura del DRS es ilegal, el piloto será automaticamente descalificado
          de la carrera y además se establecera una multa al equipo sancionado")
  }
####CHASIS####    
  anchura_chasis <- readline(prompt = "Ingresa la anchura de tu chasis: ")
  altura_chasis <- readline(prompt = "Ingresa la altura de tu chasis: ")
  longitud_chasis <- readline(prompt = "Ingresa la longitud de tu chasis: ")
  
  anchura <- if (anchura_chasis <= 2000 ){
    print("El ancho del chasis esta dentro de los parametros")
  } else if (anchura_chasis >= 2001){
    print("El ancho del chasis es ilegal, se debe pagar una multa de 20,000
          euros además de que en caso de ser necesario, el piloto será descalificado
          de la carrera")
  }
  altura <- if (altura_chasis <= 950){
    print("La altura del chasis esta dentro de los parametros")
  } else if (altura_chasis > 950){
    print("La altura del chasis es ilegal, se debe pagar una multa de 20,000
          euros además de que en caso de ser necesario, el piloto será descalificado
          de la carrera")
  }
  longitud <- if (longitud_chasis <= 5000){
    print("La longitud del chasis esta dentro de los parametros")
  } else if (longitud_chasis >= 5001){
    print("La longitud del chasis es ilegal, se debe pagar una multa de 20,000
          euros además de que en caso de ser necesario, el piloto será descalificado
          de la carrera")
  }
####SISTEMA ELECTRICO####
  sistema_electrico <- readline(prompt = "Ingresa el diametro de tus LED's de la cabina del piloto: ")
  
  LED <- if (sistema_electrico >= 5 & sistema_electrico <= 10){
    print("El diametro del LED es el adecuado")
  }else if (sistema_electrico > 10 | sistema_electrico <5){
    print("El diametro del LED es ilegal y deben ser cambiados de inmediato, de lo contrario
          se establecera una multa")
  }
####LLANTAS####
  diametro_llantasseco <- readline(prompt = "Ingresa el diametro de tus llantas para pista seco: ")
  
  seco <- if (diametro_llantasseco >= 630 & diametro_llantasseco <= 660){
    print("El diametro de las llantas para pista seca esta dentro de los parametros")
  } else if (diametro_llantasseco >= 661 | diametro_llantasseco <= 629){
    print("El diametro de las llantas para pista seca esta fuera de los limites, 
          al ser todas proporcionadas con Pirelli se abrira investigación para
          conocer si existe alguna ayuda por parte de la compañia hacia el equipo")
  }
  
  diametro_llantasmojado <- readline(prompt = "Ingrese el diametro de tus llantas para pista mojada: ")

  mojado <- if (diametro_llantasmojado <= 670 & diametro_llantasmojado >= 650){
    print("El diamerto de las llantas para pista mojada esta dentro de los parametros")
  } else if (diametro_llantasmojado >= 671 | diametro_llantasmojado <= 649){
    print("El diametro de las llantas para pista mojada esta fuera de los limites, 
          al ser todas proporcionadas con Pirelli se abrira investigación para
          conocer si existe alguna ayuda por parte de la compañia hacia el equipo")
  }
  
  ancho_llantasdel <- readline(prompt = "Ingrese el ancho de sus llantas delanteras: ")
  ancho_llantastras <- readline(prompt = "Ingrese el ancho de sus llantas traseras: ")
  
  ancho <- if (ancho_llantasdel <= 305 & ancho_llantastras <= 405){
    print("El ancho de las llantas es el establecido en los parametros")
  } else if(ancho_llantasdel >= 306 | ancho_llantastras >= 406){
    print("El ancho de las llantas delanteras o traseras no cumple con los parametros, 
          al ser todas proporcionadas con Pirelli se abrira investigación para
          conocer si existe alguna ayuda por parte de la compañia hacia el equipo")
  }
}

#Cree un ciclo for que contuviera todos los parametros a evaluar, despues dentro de este ciclo 
#fui poniendo los parametros que me interesaban creandolos como objetos y de manera que solicitara al 
#usuario que ingresara sus valores, esto gracias a "readline(prompt=", una vez que creaba el objeto 
#que le iba a preguntal al usuario sus medidas con ayuda de if & else if, creaba los parametros que debían 
#de cumplir las medidas ingresadas por el usuario para que arrojara el mesaje de que sus medidas estaban bien 
#o eran ilegales segun fuera el caso. 
#Dentro de cada parametro se solicita una medida y esta medida es puesta a prueba con if & else if, de manera 
#que los parametros correctos ya estan establecidos en el ciclo if y deben ser cumplidos para que arroje el mensaje
#de que estab bien o en caso contrario pondrá que las medidas son ilegales.