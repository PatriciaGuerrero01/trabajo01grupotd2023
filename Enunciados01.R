#Enunciados de los problemas

#PROBLEMA 1: Sea la tabla de decisión con 5 estados de la naturaleza y 4
#alternativas de la siguiente forma:

X = matrix(c(5,4,6,2,3,1,7,8,7,5,2,0,8,9,5,4,3,-1,9,10),nrow=4,ncol=5,byrow=TRUE)
colnames(X)=c('e1','e2','e3','e4','e5')
rownames(X)=c('d1','d2','d3','d4')
X

#Resolverla tanto para situación favorable como desfavorable, con cada uno de los 
#criterios por separado.


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

#PROBLEMA 3: Aplicar todos los criterios de toma de decisiones bajo incertidumbre, en tablas separadas,
#donde los valores vienen recogidos por la siguiente matriz. Son 6 estados de la naturalez y 3 situaciones.
X = matrix(c(8,3,5,2,,0,0,5,4,10,9,8,1,12,11,3,7,5,1),nrow=3,ncol=6,byrow=TRUE)
colnames(X)=c('e1','e2','e3','e4','e5','e6')
rownames(X)=c('d1','d2','d3')
X

#PROBLEMA 4:Una almazara, cooperativa de aceite, tiene que decidir si abre para la cosecha de ese año o 
#por el contrario la alquila. Esto dependerá de la posible cosecha y de los beneficios que se obtendrán 
#de la venta de aceite. Se sabe que la producción de todos los años ronda las 200 toneladas de aceitunas, 
#por lo tanto, los gastos de producción rondan los 15 céntimos por litro. A parte, se tienen unos gastos 
#fijos de 15000€. Si se alquilase la almazara, los gastos fijos habría que pagarlos, sin embargo, los gastos 
#de producción quedarían a cargo del arrendatario. Si durante el año llueve y no hace viento, la producción 
#de aceite es de 1 litro por cada kilo de aceitunas y el precio del litro al venderlo es de 1€. Si durante eL
#año llueve bastante y hace viento, la producción de aceite es la misma que en el caso anterior pero el precio 
#de venta aumenta a 1,50€ el litro. Por el contrario, si durante el año no llueve, la producción de aceite disminuye 
#a por cada 3 kilos de aceitunas se hace un litro de aceite y el precio de venta de un litro de aceite aumenta hasta 2,50€.
Por otra parte si se alquila se hace por 150000€ y la empresa produce y gana el 10% de la producción anual.
