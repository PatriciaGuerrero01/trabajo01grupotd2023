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