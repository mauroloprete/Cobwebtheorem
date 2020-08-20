# Vamos a usar las siguientes librerias :
    #ggplot2
    #gganimate
# En el caso de no tenerlas , instalarlas con :
    # install.packages("ggplot2") ; install.packages("gganimate") ; install.packages("gifski") ; install.packages("png")
    #(Quitar el # delante de la primera linea.

# COn library , cargaremos los paquetes en la sesión :
library("ggplot2");library("gganimate")

# Vamos a empezar a programar nuestras funciones y luego usaremos ejemplos númericos:
    # Para programar una función en R , debemos de usar el comando "function", dentro del parentesis mencionaremos
    # los argumentos de la función , se puede ver de forma similar a f(x,y) siendo x , y los argumentos de la función.
    # Dentro de los corchetes , le diremos a R que hacer con sus argumentos , es decir el cuerpo de la función.
TablassinPrecioDinamico <- function(t,Po,do,pd,oo,pd) {
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
        Tabla = rbind.data.frame(
            tabla,
            a
        )
        names(tabla) = c(
            "Periodo",
            "CantidadDemandada",
            "CantidadOfrecida",
            "Precio en t"
        )
        return(tabla)
    }
}

TablaPrecioDinamico <- function(t,Po,do,pd,oo,pd) {
    Pe = (do+oo)/(pd+po)
    z = (-po)/(pd)
    tabla = NULL
    a = NULL
    i = NULL  
    for (i in 0:t) {
        Pt = Pe+(Po-Pe)*((z)^(i))
        Ptm1 = Pe+(Po-Pe)*((z)^(i-1))
        Qd = abs(do-(pd*Pt))
        Qs = abs((po*Ptm1) - oo)
        a = c(i,Qd,Qs,Pt)
        tabla = rbind.data.frame(tabla,a)  
        names(tabla) = c(
            "Periodo",
            "CantidadDemandada",
            "CantidadOfrecida",
            "Precio en t")
    }
    return(tabla)
}

#Para crear la animacion vamos a crear otra funcion que tiene los mismos argumentos que las anteriores.
#Su resultado es una tabla , pero su estructura esta pensada para la animacion en gganimate. Vamos a agregar por filas
#Los datos obtenidos, primero el de la oferta y luego el de la demanda. 
Animacion=function(t,Po,do,pd,oo,po) 
    #Vamos a definir la solucion de la ecuacion en diferencia:
    {Pe=(do+oo)/(pd+po) ; z=(-po)/(pd) 
    #Vamos a -declarar- unos objetos que la funcion tiene que usar para almacenar datos:
    tabla=NULL ; a=NULL ; i=NULL ; b=NULL 
    #En el periodo inicial, existe una demanda del bien , pero los productores no tienen dicha produccion para venderla en el mercado
    #por lo que Qs=0 y Qd sera segun la funcion establecida.
    tabla=rbind.data.frame(tabla,c(0,0,Po),c(0,abs(do-(pd*Po)),Po))
    #Para tener que ejecturar la funcion una unica vez , vamos a usar el bucle for , que lo que hace es generar numeros que nos serviran para representar el tiempo en cada funcion. Se debe declarar un identificador (i)
    #para poder llamar el numero cuando lo nesecitemos y establecer su recorrido.
    for(i in 1:t) 
    {Pt=Pe+(Po-Pe)*((z)^(i));Ptm1=Pe+(Po-Pe)*((z)^(i-1));Qd=abs(do-(pd*Pt));Qs=abs((po*Ptm1)-oo);a=c(i,Qs,Ptm1) ; b =c(i,Qd,Pt);tabla=rbind.data.frame(tabla,a,b) ; 
    names(tabla)=c("Periodo","Cantidad","Pt")}
    return(tabla)}
#Las siguientes presentaciones graficas, estan en el plano (Q,P) lo cual se debe tomar algunas consideraciones respecto a las concluciones sobre la osiclacion del precio. Es decir, en el estas concluciones son para el plano (P,Q) el inverso al que estara
#Representado en la animacion , a efectos de los datos estos seran iguales, pero notaran que lo que decimos sobre las inclinaciones de las curvas tendra un sentido inverso en la representacion grafica.
##################################################################################################################################################################################################################################                                                                                                                                                                                                                          #
#                                  Modelo de la telarnia amortiguado b<1 (En la expresion de Pt, la pendiente de la curva de demanda es mayor, en P,Q)                                                                           #      
##################################################################################################################################################################################################################################
#Como habiamos dicho antes, el mercado nunca se equilibraria en el periodo, es mas la oferta sera retardada.
TablasinPrecioDinamicoE1=TablasinPrecioDinamico(35,40,12,0.3,5,0.25)
#En cambio , ya que esta funcion si tiene implicita las identidades del modelo , podemos ver que las cantidades son las mismas.
TablaPrecioDinamicoE1=TablaPrecioDinamico(35,40,12,0.3,5,0.25)
TablaPrecioDinamicoE1
write.csv(round(TablaPrecioDinamicoE1,1),file = "TablaE1.csv",row.names = FALSE)
p1=seq(21,40,0.1);C1=cbind.data.frame(p1,qdd=12-((0.3)*p1),qss=0.25*p1-5)
AnimacionE1=Animacion(35,40,12,0.3,5,0.25)
write.csv(round(AnimacionE1,1),file = "E1.csv",row.names = FALSE)
#Vamos a crear la grafica con ggplot, este paquete a diferencia del que viene de base en R , se usan capas para personalizarlo. Primero definimos el objeto donde estan los datos
#Que variable va en cada eje y su forma de represtacion (geom_ponit,geom_line,etc). Para superponer graficos tenemos que sumar el estilo de los graficos pero declarado que la informacion
#Se encuentre en lugares diferentes.
GE1=ggplot(AnimacionE1,aes(Cantidad,Pt))+geom_line(size=1.5)
GME1=GE1+geom_line(data=C1,aes(x=qdd,y=p1,colour="#0009"),size=2.7,)+geom_line(data=C1,aes(x=qss,y=p1,colour="#0020"),size=2.7)
GME1=GME1+xlab("Cantidad")+ylab("Precio")
GME1=GME1+theme_classic()+theme(legend.position="none")
GME1A=GME1+transition_reveal(AnimacionE1$Periodo)+shadow_mark()
anim_save(GME1A,filename = "GME1A.gif")
GME1A
MovPE1=ggplot(AnimacionE1,aes(x=Periodo,y=Pt))+geom_line(size=2.7)+scale_x_discrete()+xlab("")+ylab("Precio")
MovPE1=MovPE1+theme_classic()+theme(legend.position="none")
MovPE1=MovPE1+transition_reveal(AnimacionE1$Periodo)
anim_save(MovPE1,filename = "MovPE1.gif")
MovPE1
##################################################################################################################################################################################################################################                                                                                                                                                                                                                          #
#                                          Modelo de la telarnia Uniforme (b=1) (En la expresion de Pt, la pendientes son iguales, en P,Q y Q,P)                                                                                #       
##################################################################################################################################################################################################################################
TablasinPrecioDinamicoE2=TablasinPrecioDinamico(12,15,9,0.3,3,0.3)
TablaPrecioDinamicoE2=TablaPrecioDinamico(12,15,9,0.3,3,0.3)
TablaPrecioDinamicoE2
write.csv(round(TablaPrecioDinamicoE2,1),file = "TablaE2.csv",row.names = FALSE)
p2=seq(10,30,0.1);C2=cbind.data.frame(p2,qdd=9-((0.3)*p2),qss=0.3*p2-3)
AnimacionE2=Animacion(12,15,9,0.3,3,0.3)
write.csv(round(AnimacionE2,1),file = "E2.csv",row.names = FALSE)
GE2=ggplot(AnimacionE2,aes(Cantidad,Pt))+geom_line(size=1.5)
GME2=GE2+geom_line(data=C2,aes(x=qdd,y=p2,colour="#0009"),size=2.7,)+geom_line(data=C2,aes(x=qss,y=p2,colour="#0020"),size=2.7)
GME2=GME2+xlab("Cantidad")+ylab("Precio")
GME2=GME2+theme_classic()+theme(legend.position="none")
GME2A=GME2+transition_reveal(AnimacionE2$Periodo)+shadow_mark()
anim_save(GME2A,filename = "GME2A.gif")
GME2A
MovPE2=ggplot(AnimacionE2,aes(x=Periodo,y=Pt))+geom_line(size=2.7)+scale_x_discrete()+xlab("")+ylab("Precio")
MovPE2=MovPE2+theme_classic()+theme(legend.position="none")
MovPE2=MovPE2+transition_reveal(AnimacionE2$Periodo)
anim_save(MovPE2,filename = "MovPE2.gif")
MovPE2
##################################################################################################################################################################################################################################                                                                                                                                                                                                                          #
#                                          Modelo de la telarania explosiva (b>1) (En la expresion de Pt, la pendiente de la curva de oferta es mayor, en P,Q en Q,P es inversa)                                                 #       
##################################################################################################################################################################################################################################
TablasinPrecioDinamicoE3=TablasinPrecioDinamico(10,6,16,3/2,4,2)
TablaPrecioDinamicoE3=TablaPrecioDinamico(10,6,16,3/2,4,2)
TablaPrecioDinamicoE3
write.csv(round(TablaPrecioDinamicoE3,1),file = "TablaE3.csv",row.names = FALSE)
p3=seq(1,11,0.001);C3=cbind.data.frame(p3,qdd=16-((3/2)*p3),qss=2*p3-4)
AnimacionE3=Animacion(10,6,16,3/2,4,2)
write.csv(round(AnimacionE3,1),file = "E3.csv",row.names = FALSE)
GE3=ggplot(AnimacionE3,aes(Cantidad,Pt))+geom_line(size=1.5)
GME3=GE3+geom_line(data=C3,aes(x=qdd,y=p3,colour="#0009"),size=2.7,)+geom_line(data=C3,aes(x=qss,y=p3,colour="#0020"),size=2.7)
GME3=GME3+xlab("Cantidad")+ylab("Precio")
GME3=GME3+theme_classic()+theme(legend.position="none")
GME3A=GME3+transition_reveal(AnimacionE3$Periodo)+shadow_mark()
anim_save(GME3A,filename = "GME3A.gif")
GME3A
MovPE3=ggplot(AnimacionE3,aes(x=Periodo,y=Pt))+geom_line(size=2.7)+scale_x_discrete()+xlab("")+ylab("Precio")
MovPE3=MovPE3+theme_classic()+theme(legend.position="none")
MovPE3=MovPE3+transition_reveal(AnimacionE3$Periodo)
anim_save(MovPE3,filename = "MovPE3.gif")
MovPE3

