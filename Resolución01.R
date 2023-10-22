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
