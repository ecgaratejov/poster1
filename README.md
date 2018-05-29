# poster1
analisis de regresion con modelos gamlss y spline
datos<-read.csv(file = "/home/garatejo/puntajes.csv",header = TRUE,sep = ",",dec = ",")
tabla<-table(datos$NATURALEZA)
tabla<-prop.table(tabla)
tabla1<-table(datos$CALENDARIO)
tabla1<-prop.table(tabla1)
tabla2<-table(datos$JORNADA)
tabla2<-prop.table(tabla2)
tabla3<-table(datos$NATURALEZA,datos$JORNADA)
datos2<-subset(datos,NATURALEZA=="OFICIAL")
datos3<-subset(datos,NATURALEZA=="NO OFICIAL")
#graficos descriptivos
par(mfrow=c(2,2))
barplot(tabla3,beside = TRUE,xlab = "Jornada",ylab = "Frecuencia",col = c("orange","goldenrod4"))
legend('topright',legend = rownames(tabla3),bty = "n",fill = c("orange","goldenrod4"))
barplot(tabla,xlab = "Naturaleza",ylab = "frecuencia relativa",col = c("red2","blue2"))
den.ofic<-density(datos2$PERIODO,bw=1,adjust = 1,kernel = "gaussian",na.rm = TRUE)
den.nofic<-density(datos3$PERIODO,bm=1,adjust = 1,kernel = "gaussian",na.rm = TRUE)
d<-density(datos$PERIODO,bw=1,adjust = 1,kernel = "gaussian",na.rm = TRUE)

plot(d,lwd=4)
plot(den.ofic,lwd=4,col="blue")
lines(den.nofic,lwd=4,col="red")
library(gamlss)
d.ofic<-fitDist(PERIODO,data = datos2,type = "realplus")
d.nofic<-fitDist(PERIODO,data = datos3,type = "realplus")
d.ofic$fits
d.nofic$fits


m1<-fitDist(PERIODO,data = datos,type = "realplus")
m1$fits
m2<-histDist(PERIODO,data = datos,family = BCPEo)
m3<-histDist(PERIODO,data = datos,family = IGAMMA)
m4<-histDist(PERIODO,data = datos,family = GG)
m5<-histDist(PERIODO,data = datos,family = BCCGo)
m6<-histDist(PERIODO,data = datos,family = GIG)
naturaleza<-factor(datos$NATURALEZA,levels = c("OFICIAL","NO OFICIAL"))
jornada<-factor(datos$JORNADA,levels = c("MAÑANA","COMPLETA","TARDE","NOCTURNA","ÚNICA","FIN DE SEMANA"))
calendario<-factor(datos$CALENDARIO,levels = c("A","B","O"))
modelo1<-gamlss(PERIODO~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,sigma.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,nu.formula =~1,tau.formula =~1,data =datos,family = BCPEo)
modelo2<-gamlss(PERIODO~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,sigma.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,data =datos,family = IGAMMA)
modelo3<-gamlss(PERIODO~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,sigma.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,nu.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,data =datos,family = GG)
modelo4<-gamlss(PERIODO~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,sigma.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,nu.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,data =datos,family = BCCGo)
modelo5<-gamlss(PERIODO~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,sigma.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,nu.formula =~calendario+naturaleza+jornada+EVALUADOS+
                  PROMSOCIALESYCIUDADANAS+PROMLECTURACRITICA+
                  PROMMATEMATICA+PROMCIENCIASNATURALES+PROMINGLES,data =datos,family = GIG)
modelo11<-stepGAIC(modelo1)
modelo111<-stepGAIC(modelo11,parameter = "sigma")
modelo22<-stepGAIC(modelo1)
modelo222<-stepGAIC(modelo11,parameter = "sigma")
modelo33<-stepGAIC(modelo1)
modelo333<-stepGAIC(modelo11,parameter = "sigma")
modelo44<-stepGAIC(modelo1)
modelo444<-stepGAIC(modelo11,parameter = "sigma")
modelo55<-stepGAIC(modelo1)
modelo555<-stepGAIC(modelo11,parameter = "sigma")
modelo11<-stepGAIC(modelo1,parameter = "tau")
modelo111<-stepGAIC(modelo11,parameter = "nu")
modelo333<-stepGAIC(modelo33,parameter = "nu")
modelo444<-stepGAIC(modelo444,parameter = "nu")
modelo555<-stepGAIC(modelo11,parameter = "nu")
wp(modelo111)
wp(modelo222)
wp(modelo333)
wp(modelo444)
wp(modelo555)