#PROBLEMA 1: Sea la tabla de decisión con 5 estados de la naturaleza y 4
#alternativas de la siguiente forma:

X = matrix(c(5,4,6,2,3,1,7,8,7,5,2,0,8,9,5,4,3,-1,9,10),nrow=4,ncol=5,byrow=TRUE)
colnames(X)=c('e1','e2','e3','e4','e5')
rownames(X)=c('d1','d2','d3','d4')
X

#Resolverla tanto para situación favorable como desfavorable, con cada uno de los 
#criterios por separado.



tablaDec=crea.tablaX(c(5,4,6,2,3,1,7,8,7,5,2,0,8,9,5,4,3,-1,9,10),numalternativas=4,numestados=5)
 

#Situación favorable
criterio.Wald(tablaDec) #ALternativa 1
criterio.Optimista(tablaDec) #ALternativa 4
criterio.Hurwicz(tablaDec) #Alternativa 1
criterio.Hurwicz.General(tablaDec)#Alternativa 1
dibuja.criterio.Hurwicz(tablaDec)
dibuja.criterio.Hurwicz_Intervalos(tablaDec)
#Intervalo (0,0.333) -> Alternativa 1
#Intervalo (0.333,0.5) -> Alternativa 2
#Intervalo (0.5,1) -> Alternativa 3
criterio.Savage(tablaDec) #Alternativa 2
criterio.Laplace(tablaDec)#Alternativa 2
criterio.PuntoIdeal(tablaDec)#ALternativa 2

#Situación desfavorable
criterio.Wald(tablaDec,favorable = FALSE) #ALternativa 1
criterio.Optimista(tablaDec,favorable = FALSE) #ALternativa 4
criterio.Hurwicz(tablaDec,favorable = FALSE) #Alternativa 1
criterio.Hurwicz.General(tablaDec,favorable = FALSE)#Alternativa 1
dibuja.criterio.Hurwicz(tablaDec,favorable = FALSE)
dibuja.criterio.Hurwicz_Intervalos(tablaDec,favorable = FALSE)
#Intervalo (0,0.571) -> Alternativa 1
#Intervalo (0.571,1) -> Alternativa 4
criterio.Savage(tablaDec,favorable = FALSE) #Alternativa 1 y 4
criterio.Laplace(tablaDec,favorable = FALSE)#Alternativa 1
criterio.PuntoIdeal(tablaDec,favorable = FALSE) #Alternativa 1




#PROBLEMA 2: Un agricultor quiere estudiar el crecimiento de una determinada 
#cosecha de verdura. Posee cuatro posibles terrenos donde sembrar: Terreno 1 (d1), 
#Terreno 2 (d2),Terreno 3 (d3), Terreno 4 (d4) ; habrá 4 posibles estados del 
#clima que influirá en el crecimiento de la cosecha: soleado(e1),nublado(e2),
#lluvia(e3),lluvia abundante(e4). Y en consecuencia se recolectarán más o menos kilos de
#verdura que se recogen en la siguiente tabla: 

X = matrix(c(300,540,615,210,345,105,709,298,365,640,100,540,860,192,570,540),nrow=4,ncol=4,byrow=TRUE)
colnames(X)=c('e1','e2','e3','e4')
rownames(X)=c('d1','d2','d3','d4')
X

tablaAgri=crea.tablaX(c(300,540,615,210,345,105,709,298,365,640,100,540,860,192,570,540),
                      numalternativas=4,numestados=4)
criterio.Todos(tablaAgri)
#e1  e2  e3  e4 Wald Optimista Hurwicz Savage Laplace Punto Ideal
#d1              300 540 615 210  210       615   331.5    560   416.2       664.3
#d2              345 105 709 298  105       709   286.2    535   364.2       781.0
#d3              365 640 100 540  100       640   262.0    609   411.2       784.8
#d4              860 192 570 540  192       860   392.4    448   540.5       469.1
#iAlt.Opt (fav.)  --  --  --  --   d1        d4      d4     d4      d4          d4




#PROBLEMA 3: Aplicar todos los criterios de toma de decisiones bajo incertidumbre, en tablas separadas, donde los valores vienen recogidos por la siguiente matriz. Son 6 estados de la naturaleza y 3 situaciones.
#     e1  e2  e3  e4  e5  e6
# d1  8   3   5   2   0   0
# d2  5   4   10  9   8   1
# d3  12  11  3   7   5   1

source("teoriadecision_funciones_incertidumbre.R") #cargamos todas las funciones necesarias para resolver el problema

tabla1 = crea.tablaX(c(8,3,5,2,0,0,
                       5,4,10,9,8,1,
                       12,11,3,7,5,1), numalternativas = 3, numestados = 6)
tabla1

##Si la tabla fuera favorable:
#Criterio de Wald
cWaldF = criterio.Wald(tabla1, favorable = T)
cWaldF
cat("Para el criterio de Wald, las aleternativas óptimas serian", names(cWaldF$AlternativaOptima))

#Criterio Optimista
cOptF = criterio.Optimista(tabla1, favorable = T)
cOptF
cat("Para el criterio optimista, la alernativa óptima sería", names(cOptF$AlternativaOptima))

#Criterio de Hurwicz con alpha = 0.6
cHurF = criterio.Hurwicz(tabla1, alfa = 0.6, favorable = T)
dibuja.criterio.Hurwicz(tabla1) #Se ve graficamente cual es la decisión tomada
cHurF
cat("Para el criterio de Hurwica, ", names(cHurF$AlternativaOptima), " sería la alternativa óptima")

#Criterio de Savage
cSavF = criterio.Savage(tabla1, favorable = T)
cSavF
cat("Para el criterio de Savage, las aleternativas óptimas serian", names(cSavF$AlternativaOptima))

#Criterio de Laplace
cLapF = criterio.Laplace(tabla1, favorable = T)
cLapF
cat(names(cLapF$AlternativaOptima), "es la alternativa óptima según el criterio de Laplace")

#Criterio del punto ideal
cPIF = criterio.PuntoIdeal(tabla1, favorable = T)
cPIF
cat("La alternativa óptima por el criterio del punto ideal es ", names(cPIF$AlternativaOptima))

cat("Para esta matriz de datos y siendo favorable la alternativa escogida por todos los criterios ha sido la tercera.")


##Si la tabla fuera desfavorable:
#Criterio de Wald
cWaldnF = criterio.Wald(tabla1, favorable = F)
cWaldnF
cat("Para el criterio de Wald, las aleternativas óptimas serian", names(cWaldnF$AlternativaOptima))

#Criterio Optimista
cOptnF = criterio.Optimista(tabla1, favorable = F)
cOptnF
cat("Para el criterio optimista, la alernativa óptima sería", names(cOptnF$AlternativaOptima))

#Criterio de Hurwicz con alpha = 0.6
cHurnF = criterio.Hurwicz(tabla1, alfa = 0.6, favorable = F)
dibuja.criterio.Hurwicz(tabla1, favorable = F) #Se ve graficamente cual es la decisión tomada
cHurnF
cat("Para el criterio de Hurwica, ", names(cHurnF$AlternativaOptima), " sería la alternativa óptima")

#Criterio de Savage
cSavnF = criterio.Savage(tabla1, favorable = F)
cSavnF
cat("Para el criterio de Savage, las aleternativas óptimas serian", names(cSavnF$AlternativaOptima))

#Criterio de Laplace
cLapnF = criterio.Laplace(tabla1, favorable = F)
cLapnF
cat(names(cLapnF$AlternativaOptima), "es la alternativa óptima según el criterio de Laplace")

#Criterio del punto ideal
cPInF = criterio.PuntoIdeal(tabla1, favorable = F)
cPInF
cat("La alternativa óptima por el criterio del punto ideal es ", names(cPInF$AlternativaOptima))

cat("Para esta matriz de datos y siendo desfavorable, la alternativa escogida por todos los criterios ha sido la primera.")




#PROBLEMA 4: Una almazara, cooperativa de aceite, tiene que decidir si abre para la cosecha de ese año o por el contrario la alquila. Esto de'penderá de la posible cosewcha y de los beneficios qie se obtendrán de la venta de aceite. Se sabe que la producción de todos los años ronda las 200 toneladas de aceitunas, por lo tanto los gastos de producción ronda los 15 céntimos por litro. A parte, se tienen unos gastos fijos de 15000€. Si se alquilase la almazara, los gastos fijos habría que pagarlos, sin embargo, los gastos de producción quedarían a cargo del arrendatario. 
#Si durante el año lleuve y no hace viento, la producción de aceite es de 1 litro por cada kilo de aceitunas y el precio del litro al venderlo es de 1€. Si durante el año lluve bastante y hace viento, la producción de aceite es la misma que en el caso anterior pero el precio de venta aumneta a 1,50€ el litro. Por el contrario, si durante el año no llueve, la producción de aceite disminuye a por cada 3 kilos de aceitunas se hace un litro de aceite y el precio de venta de unm litro de aceite aumenta 2,50€.
#Por otra parte si se alquila se hace por 150000€ y la empresa produce y gana el 10% de la producción anual.
#Si se quieren maximizar los beneficios, ¿qué opción deberá elegir la empresa de la almazara?

#Introducir los datos:
#e1: llueve y no hace viento
#e2: llueve y hace viento
#e3: no llueve

#d1: producir el aceite
#d2: alquilar la almazara

m11 = (200000/1)*1 - 15000 - (200000*0.15)
m21 = 150000 - 15000 + ((200000/1)*1 - (200000*0.15))*0.1

m12 = (200000/1)*1.5 - 15000 - (200000*0.15)
m22 = 150000 - 15000 + ((200000/1)*1.5 - (200000*0.15))*0.1

m13 = (200000/3)*2.5 - 15000 - (200000*0.15)
m23 = 150000 - 15000 + ((200000/3)*2.5 - (200000*0.15))*0.1

#Resolución:
source("teoriadecision_funciones_incertidumbre.R") #cargamos todas las funciones necesarias para resolver el problema
tabla2 = crea.tablaX(c(m11,m12,m13,
                       m21,m22,m23), numalternativas = 2, numestados = 3)
tabla2

res2 = criterio.Todos(tabla2, 0.4, favorable = T)
res2
cat("Todos los criterios han elegido que la almazara produzca el aceite.")

## PROBLEMA 5
Aplicar los criterios de decisión bajo incertidumbre a los problemas cuya matriz de valores viene dada e la tabla siguiente.  
Considerar beneficios (favorable) y costos (desfavorable)  

```{r echo=FALSE}
source("teoriadecision_funciones_incertidumbre.R")
m1A <- crea.tablaX(c(7,5,1,
                     -3,1,3,
                     5,-1,3,
                     5,4,-10),
                   numalternativas = 4, numestados = 3)
m1A
```

Vamos a resolver el apartado usando las funciones creadas de cada criterio.  

### TABLA DE BENEFICIOS  


#### Método de Wald

```{r}
sol1a_W <- criterio.Wald(m1A, favorable = T) #ponemos T prorque estamos trabajando con una tabla de beneficios
sol1a_W
```
La mejor alternativa según el criterio de Wald es la "d1" siendo el valor óptimo el 1.  

#### Criterio Optimista

```{r}
sol1a_O <- criterio.Optimista(m1A,T)
sol1a_O
```
La mejor alternativa según el criterio Optimista es la "d1" siendo el valor optimo el 7.  

#### Criterio Hurwicz
```{r}
sol1a_H <- criterio.Hurwicz(m1A, favorable = T)
sol1a_H 

```

La mejor alternativa según el criterio Hurwicz es la "d1" siendo el valor optimo el 2.8.

```{r}
dibuja.criterio.Hurwicz(m1A)
dibuja.criterio.Hurwicz_Intervalos(m1A,T,T)
```
#### Criterio Savage
```{r}

sol1a_S <- criterio.Savage(m1A,T)
sol1a_S
```

La mejor alternativa según el criterio Savage es la "d1" siendod el valor optimo el 2.

#### Criterio LaPlace
```{r}

sol1a_LP <- criterio.Laplace(m1A,T)
sol1a_LP
``` 


La mejor alternativa según el criterio LaPlace es la "d1" siendo el valor optimo el 4.33333.

#### Criterio Punto ideal

```{r}

sol1a_PI <- criterio.PuntoIdeal(m1A,T)
sol1a_PI
```

La mejor alternativa según el criterio Punto ideal es la "d1" siendo el valor optimo el 2

#### En resumen:
```{r}
sol1a_T <- criterio.Todos(m1A,alfa = 0.5,T)
sol1a_T
```
En general, cuando tratamos esta matriz como una tabla de beneficios, la mejor alternativa es la "d1"

## TABLA DE COSTOS

#### Método de Wald
```{r}
sol1b_W <- criterio.Wald(m1A, favorable = F) 
sol1b_W
``` 

La mejor alternativa según el criterio de Wald es la "d2" siendo el valor óptimo el 3.

#### Criterio Optimista
```{r}

sol1b_O <- criterio.Optimista(m1A,F)
sol1b_O
```


La mejor alternativa según el criterio Optimista es la "d4" siendo el valor optimo el -10.

#### Criterio Hurwicz
```{r}
sol1b_H <- criterio.Hurwicz(m1A, favorable = T)
sol1b_H 
```

La mejor alternativa según el criterio Hurwicz es la "d1" siendo el valor optimo el 2.8.
```{r}

dibuja.criterio.Hurwicz(m1A)
```

#### Criterio Savage
```{r}

sol1b_S <- criterio.Savage(m1A,F)
sol1b_S
```

La mejor alternativa según el criterio Savage es la "d4" siendod el valor optimo el 8.

#### Criterio LaPlace
```{r}
sol1b_LP <- criterio.Laplace(m1A,F)
sol1b_LP
```

La mejor alternativa según el criterio LaPlace es la "d4" siendo el valor optimo el -0.33.

#### Criterio Punto ideal
```{r}

sol1b_PI <- criterio.PuntoIdeal(m1A,F)
sol1b_PI
```
La mejor alternativa según el criterio Punto ideal es la "d4" siendo el valor optimo el 9.433981

#### En resumen:
```{r}
sol1b_T <- criterio.Todos(m1A,alfa = 0.5,F)
sol1b_T

```

En general, cuando tratamos esta matriz como una tabla de costos, la mejor alternativa es la "d4".

## PROBLEMA 6

**Elección de un Transporte para el Trabajo**

Nico está considerando sus opciones de transporte para ir al trabajo. Tiene tres alternativas:

1. **Conducir su propio automóvil:** Nico puede conducir su automóvil al trabajo. El costo mensual de gasolina y estacionamiento es de 200 euros. Nico tiene un colega que trabaja cerca de su trabajo, con el que puede compartir el gasto de la gasolina a medias si coinciden en horario

2. **Tomar el transporte público:** Nico puede optar por usar el transporte público para ir al trabajo. El costo mensual del pase de transporte público es de 80. Sin embargo, si su colega opta por ir con Nico en transporte público pueden sacar un bono con un 15% de descuento para ambos.

3. **Blablacar:** Nico puede por irse al trabajo en blablacar costandole el transporte 160 euros mensuales,si su colega se apunta con Nico al anunciante de blablacar el transporte le costaria un 30% menos solo para Nico.

Nico tiene dos estados de la naturaleza para considerar:

**Estado de la Naturaleza 1: Disponibilidad del Colega:** En este estado, su colega está disponible para compartir el viaje.

**Estado de la Naturaleza 2: Indisponibilidad del Colega:** En este estado, su colega no puede compartir el viaje.

¿Cuál es la opción más conveniente para Nico en términos de costo total durante el primer año? Representa las ecuaciones de alternativas para cada opción de transporte y estado de la naturaleza

```{r}


m11 <- 200/2*12
m12 <- 200*12
m21 <- (80-80*0.15)*12
m22 <- 80*12
m31 <- (160-160*0.3)*12
m32 <- 160*12
tb <- crea.tablaX(c(m11,m12,
                      m21,m22,
                      m31,m32),  numalternativas=3,numestados=2)


rownames(tb) <- c("Coche Propio", "Bus", "Blablacar")
colnames(tb) <- c("Disponibilidad del Colega", "Indisponibilidad del Colega")

criterio.Todos(tb, 0.5, T)

```
 Con estos resultados se recomienda a Nico utilizar su coche Propio.
