
#pagina 30 comp r04 
setClass("Persona",
          slots = list(nombre = "character", edad = "numeric"),
          prototype=list(
            nombre = NA_character_,
            edad = NA_real_
          ))
setClass("Empleado",
          contains = "Persona",
          slots = list(jefe = "Persona"),
          prototype = list(
            jefe = new("Persona")
          ))
#crear objetos
Pilar <- new("Persona", nombre = "Pilar", edad = 40)
Ivan <- new("Empleado", nombre = "Ivan", edad = 35, jefe = Pilar)

#clase persona funcion persona
Persona <- function(nombre="NA", edad=NA) {
  edad <- as.double(edad)
  new("Persona", nombre=nombre, edad=edad)
}
Persona("Roberto")
#clase empleado funcion empleado
Empleado <- function(nombre, edad=NA,
                     jefe=Persona,nombrej="NA",edadj=NA){
  edad <- as.double(edad)
  new("Empleado", nombre=nombre, edad= edad, jefe=Persona(nombre=nombrej,edad=edadj))
}
Empleado("Oscar", edad=27)

setValidity("Persona", function(obj) {
  if (length(obj@nombre) != length(obj@edad)) {
    cat("ERROR!! ",obj@nombre, " y ", obj@edad, " deben tener la misma longitud no vectorial \n")
  } else {
    TRUE
  }
})

a=Persona("David", edad=c(21,24))

b=Persona("David", edad=c(21))
validObject(a)

validObject(b)

#funciones genericas

setGeneric(name= "Edad", def= function(x) standardGeneric("Edad"))
setGeneric(name= "Edad<-", def= function(x, value) standardGeneric("Edad<-"),
           signature = 'x')

setMethod(f = "Edad",
  signature = "Persona",
  definition = function(x) {
    return(x@edad)
  }
)
setMethod(f = "Edad<-",
  signature = "Persona",
  definition = function(x, value) { 
    x@edad <- value
    return(x)
  }
)
david <- Empleado(nombre= "David", edad=34)
