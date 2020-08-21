# Vamos a usar las siguientes librerias :
    #ggplot2
    #gganimate
# En el caso de no tenerlas , instalarlas con :
    # install.packages("ggplot2") ; install.packages("gganimate") ; install.packages("gifski") ; install.packages("png")
    #(Quitar el # delante de la primera linea.

# COn library , cargaremos los paquetes en la sesión :
library("ggplot2") ; library("gganimate") ; library(magrittr)

# Vamos a empezar a programar nuestras funciones y luego usaremos ejemplos númericos:
    # Para programar una función en R , debemos de usar el comando "function", dentro del parentesis mencionaremos
    # los argumentos de la función , se puede ver de forma similar a f(x,y) siendo x , y los argumentos de la función.
    # Dentro de los corchetes , le diremos a R que hacer con sus argumentos , es decir el cuerpo de la función.
TablassinPrecioDinamico <- function(t,Po,do,pd,oo,po) {
    #Siendo t la cantidad de periodos que queremos considerar, Po el valor inicial , do el termino independiente de la demanda
    # pd la pendiente de la curva de demanda. oo será el termino independiente de la función de oferta y po la pendiente de dicha curva.
    # En la primera linea de codigo mostramos una de las identidades del modelo , el precio de equilibrio y z el "b" de la ecuación en diferencia.
    Pe = (do+oo)/(pd+po) 
    z = (-po)/pd 
    tabla = NULL
    a = NULL
    i = NULL
        #Los objetos tabla , a , z e i los creamos pero los declaramos vacios. Ahora vamos a usar la función for, que lo que hace es 
        #generar operaciones 
    for (i in 0:t) {
       # Podemos ver que en las siguientes lineas que se definen las curvas de oferta , demanda y el precio en el periodo t. La solución obtenida
       # Esto lo haremnos únicamente para mostrar qué si el precio se determinara en el mismo periodo, el mercado no estaría en equilibrio,
       # Uno de los supuestos del modelo y además de violar la condición de equilibrio.
        Pt = Pe + (Po-Pe) * ((z)^(i))
        Qd = abs(do-(pd*Pt)) 
        Qs = abs((po*Pt) - oo)
        a = c(
            i,
            Qd,
            Qs,
            Pt
        )
        tabla = rbind.data.frame(
            tabla,
            a
        )
        names(tabla) = c(
            "Periodo",
            "CantidadDemandada",
            "CantidadOfrecida",
            "Precio en t"
        )
    }
    return(tabla)
}

TablaPrecioDinamico <- function(t,Po,do,pd,oo,po) {
    Pe = (do+oo)/(pd+po)
    z = (-po)/(pd)
    tabla = NULL
    a = NULL
    i = NULL  
    for (i in 0:t) {
        Pt = Pe + (Po-Pe) * ((z)^(i))
        Ptm1 = Pe + (Po-Pe) * ((z)^(i-1))
        Qd = abs(do - (pd*Pt))
        Qs = abs((po * Ptm1) - oo)
        a = c(
            i,
            Qd,
            Qs,
            Pt
        )
        tabla = rbind.data.frame(tabla,a)  
        names(tabla) = c(
            "Periodo",
            "CantidadDemandada",
            "CantidadOfrecida",
            "Precio en t")
    }
    return(tabla)
}

# Para crear la animación vamos a hacer uso de gganimate, vamos a crear una función
# Similar a la anterior, debemos de tener una tabla larga con una variable que indique
# el momento del tiempo o el momento en la animación en este caso (t) el tiempo :

Animacion <- function(t,Po,do,pd,oo,po){
    Pe = (do+oo)/(pd+po) ; z = (-po)/(pd) 
    tabla = NULL ; a = NULL ; i = NULL ; b = NULL
    # En el priodo incial existe una demanda del bien, pero los productores no tienen producción
    # para venderla en el mercado por lo que Qs = 0 y Qd dependerá del parametro incial.
    tabla = rbind.data.frame(
        tabla,
        c(
            0,
            0,
            Po
        ),
        c(
            0,
            abs(do-(pd*Po)),
            Po
        )
    )
    # Vamos a usar nuevamente el bucle for para generar por única vez la tabla
    for (i in 1:t) {
       Pt = Pe + (Po-Pe)*((z)^(i)) ; Ptm1 = Pe + (Po-Pe)*((z)^(i-1)) ; Qd = abs(do - (pd*Pt))
       Qs = abs((po * Ptm1)-oo) ; a = c(i,Qs,Ptm1) ; b = c(i,Qd,Pt) 
       tabla = rbind.data.frame(
           tabla,
           a,
           b
       )
       names(tabla) = c(
           "Periodo",
           "Cantidad",
           "Pt"
       )
       #La tabla tendrá la cantidad de filas igual a la cantidad de periodos de tiempo.
    }
    return(tabla)
}

# Las siguientes gráficas, estan en el plano (Q,P) lo cual debe de tener en consideración.
# Sobre las pendientes y relaciones entre los parametros de las curvas al tratarse de relaciones inversas.

##################################################################################################################################################
#                                              Telaraña Amortiguada b<1 (En la expresión de Pt,                                                  #
#                                              la pendiente de la curva de demanda es mayor en (P,Q))                                            #
##################################################################################################################################################

# El mercado nunca se equilibra en el mismo periodo de tiempo , podemos verlo con la siguiente tabla :

TablassinPrecioDinamicoE1 <- TablassinPrecioDinamico(
    t = 35,
    Po = 40,
    do = 12,
    pd = 0.3,
    oo = 5,
    po = 0.25
)

# La siguiente linea guarda la tabla que se genera con la función :
# La dejo comentada: 
# write.csv(round(TablaPrecioDinamicoE1,1),file = "TablaE1.csv",row.names = FALSE)

p1 <- seq(21,40,0.1) 

C1 <- cbind.data.frame(
    p1,
    qdd = 12 - ((0.3)*p1),
    qss = 0.25 * p1 - 5 
)

AnimacionE1 <- Animacion(
    t = 35,
    Po = 40,
    do = 12,
    pd = 0.3,
    oo = 5,
    po = 0.25
)

GE1 <- ggplot(
    AnimacionE1,
    aes(
        Cantidad,
        Pt
    )
) + 
geom_line(size = 1.5) +
geom_line( # Curva de demanda
    data = C1 ,
    aes(
        qdd,
        p1
    ),
    size = 2.7,
    colour = "green"
) +
geom_line( # Curva de Oferta
    data = C1 ,
    aes(
        qss,
        p1
    ),
    colour = "blue",
    size = 2.7
) + 
xlab("Cantidad") + ylab("Precio") +
theme_classic() + 
theme(legend.position = "none") +
# Animación con transition reveal :
transition_reveal(AnimacionE1$Periodo) + 
shadow_mark()
anim_save(
    GE1,
    filename = "GE1A.gif"
)
GE1

#Vamos a ver la evolución de los precios :

MovPE1 <- ggplot(
    AnimacionE1,
    aes(
        Periodo,
        Pt
    )
) +
geom_line(
    size = 2.7
) +
scale_x_discrete() +
xlab("") + ylab("Precio en el periodo t") +
theme_classic() + theme(legend.position = "none") +
transition_reveal(AnimacionE1$Periodo) + 
shadow_mark()
anim_save(
    MovPE1,
    filename = "MovPE1.gif"
)
MovPE1

      






