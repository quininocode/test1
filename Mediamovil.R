
rm(list=ls())

# con estos comandos se instalan los paquete - ya los tengo instalados
#install.packages("TTR")
#install.packages("quantmod")

library(TTR)
library(quantmod)

#con este comando del paquete quantmod descargamos los datos del indice S&P500
getSymbols("^GSPC", from=as.Date("1900-01-01"), to=Sys.Date(), src ="yahoo")

GSPC = to.monthly(GSPC)

#lo paso a vector y me quedo solo con el cierre adjusted
ts = as.numeric(GSPC$GSPC.Adjusted)

#graficamos los datos
plot(ts, type='l')

#tomo una media móvil de 10 períodos
varmediamovil= 10

ma = SMA(ts, varmediamovil)
lines(ma, col = "red")

#calculemos retornos geometricos simples (rentabilidades)
r = diff(ts)/ts[1:(length(ts)-1)]


#definimos una posicion inicial
posicion = 0

# retorno de estrategia donde almacenaremos los retornos
re = 0

for(i in varmediamovil:(length(ts)-1)){
  # si el precio de cierre del mes es mayor o igual a la media movil
    if(ts[i] >= ma[i]){
    posicion[i] = 1
    re[i]=r[i]
  }else{
    posicion[i] = 0
    re[i]= 0
  }
}

# calculamos el retorno d ela estrategia original
ce = cumprod(1+re[varmediamovil:length(re)])
# calculamos retorno de buy and hold
bh = cumprod(1+r[varmediamovil:length(r)])

# grafiquemos ambas:
plot(ce, type="l")
lines(bh, col=2)

# 
title("Estrategia vs Buy'n'Hold ")

# cosmetica para mejor visualizacion
legend(varmediamovil,2000, legend=c("Estrategia", "BH"), col=c("black", "red"), lty = 1, cex=0.8)


#calculamos maximo acumulado

rmbh = runMax(bh, n=1, cumulative = TRUE)

lines(rmbh, col="green")


ddbh = bh/rmbh -1

plot(ddbh, type="l")

maxdd_bh = min(ddbh)

maxdd_bh

rmce = runMax(ce, n=1, cumulative = TRUE)
ddce = ce/rmce -1

plot(ddce, type="l")

maxdd_ce = min(ddce)

maxdd_ce

# calcular ratio de sharpe de cada estrategia...


sh_estrategia = sqrt(12) *mean(re[varmediamovil:length(re)])/sd(re[varmediamovil:length(re)])

sh_bh = sqrt(12) *mean(r[varmediamovil:length(re)])/sd(r[varmediamovil:length(re)])


