#### Script para replicar graficas y analisis de tesis ###
# Tesis: Hipoxia en aguas superficiales...
# Autor: Carlos Alberto Herrera Becerril
##  --------------------  ##
#### // Definiciones \\ ####
##  -------------------   ##
{
rm(list = ls())
  graphics.off()
#install.packages("lubridate")
#install.packages("RColorBrewer")
library(lubridate)
library(RColorBrewer)
colpal = brewer.pal(n = 12, name = "Paired") #Paleta de colores
# Definir ruta de carpeta donde se encuentran los datos: ()
wd_data = "C:/Users/Carlos/Desktop/Analisis Tesis" #Ruta de carpeta de datos
setwd(wd_data) #Definir escritorio de trabajo
setwd("C:/Users/Carlos/Desktop/Analisis Tesis")

datos = readRDS("datos_tesis_CAHB.rds") #Leer matriz de datos

# Objetos utiles: 
do_col = colpal[2]
do_lab = c(expression(DO*" "*("mg L"^-1)))
sst_col = colpal[6]
sst_lab = "SST (°C)"
cui_col = colpal[8]
cui_lab = c(expression(CUI*" "*("m"^3*" "*"s"^-1*"100 m"^-1)))
sstgrad_col = colpal[10]
sstgrad_lab = c(expression(SSTGrad*" "*("°C km"^-1)))

# Tamaño ejes y etiquetas 
lab.size=1.1 
axes.size=1.1

#dir.create("graficas",showWarnings = F)
#setwd("graficas")
#wd_graficas <- getwd()
}

##  ------------------------------------  ##
#### /// Descripcion series completas \\\ ####
##  ------------------------------------  ##
##  --------  ##
#### -- DO -- ####
##  --------  ##
setwd(wd_graficas)
  #### - Serie DO completa - ####
### cortes huecos ##
install.packages("plotrix")
library(plotrix)
{
  lim1a =as.POSIXct('2014-01-21 00:00:00 UTC'); lim1b = as.POSIXct('2014-08-25 00:00:00 UTC')
  lim2a = as.POSIXct('2014-11-25 00:00:00 UTC'); lim2b = as.POSIXct('2015-08-01 00:00:00 UTC')
  lim3a = as.POSIXct('2015-10-01 00:00:00 UTC'); lim3b = as.POSIXct('2016-04-20 00:00:00 UTC')
  lim4a = as.POSIXct('2016-06-01 00:00:00 UTC'); lim4b = as.POSIXct('2016-10-02 00:00:00 UTC')
}

ejex <- seq(from = as.POSIXct('2014-01-01 00:00:00 UTC'),
            to = as.POSIXct('2016-11-01 00:00:00 UTC'),by="1 month" )
xxx  <- seq(from = as.POSIXct('2014-02-01 00:00:00 UTC'),
            to = as.POSIXct('2016-11-01 00:00:00 UTC'),by="2 months")
lab <- format(xxx,format="%Y-%b")

{
  png(filename = "DO_serie_completa.png",
      width = 30,height = 15,res=400,units = "cm")
  par(mar=c(5.2,3.5,1,2))
  plot(datos$dateUTC, datos$DO, col=do_col, type="n",
       xlab = "",ylab = "",xaxt="n",yaxs="i", xaxs="i",
       ylim=c(0,9),las=2, cex.axis=axes.size,cex.lab=lab.size,
       xlim=c(lim1a,lim4b))
  axis(1, at = xxx, labels = substring(xxx,1,7),cex.axis=0.9,las=2)
  
  year<- c(as.POSIXct('2014-01-01'),as.POSIXct('2015-01-01'),as.POSIXct('2016-01-01'))
  axis(1, at =year, labels = substr(year,1,7), cex.axis=0.9,las=2)
  abline(v=year, lty=2)
  
  mtext(text = "Fecha", side = 1, cex = lab.size, line = 4.2)
  mtext(text = do_lab, side = 2, cex = lab.size, line = 2.1)
  abline(h=2, lty=2, col="red",lwd=1)
  # Cortes huecos
  axis.break(axis =1 ,breakpos =as.POSIXct("2014-09-03"))
  axis.break(axis =3 ,breakpos =as.POSIXct("2014-09-03"))
  axis.break(axis =1 ,breakpos =as.POSIXct("2014-11-10"))
  axis.break(axis =3 ,breakpos =as.POSIXct("2014-11-10"))
  axis.break(axis =1 ,breakpos =as.POSIXct("2015-08-01"))
  axis.break(axis =3 ,breakpos =as.POSIXct("2015-08-01"))
  axis.break(axis =1 ,breakpos =as.POSIXct("2015-10-01"))
  axis.break(axis =3 ,breakpos =as.POSIXct("2015-10-01"))
  axis.break(axis =1 ,breakpos =as.POSIXct("2016-04-20"))
  axis.break(axis =3 ,breakpos =as.POSIXct("2016-04-20"))
  axis.break(axis =1 ,breakpos =as.POSIXct("2016-06-01"))
  axis.break(axis =3 ,breakpos =as.POSIXct("2016-06-01"))
  
  ## SOMBREADO Conclusion ###
    #### Sombreado  azul ## # Si surgencia
  # ev2 <- c(as.POSIXct('2014-03-02'),as.POSIXct('2014-03-09'))
  # polygon(x= c(ev2,rev(ev2)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  # 
  # ev4 <- c(as.POSIXct('2014-03-21'),as.POSIXct('2014-03-24'))
  # polygon(x= c(ev4,rev(ev4)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  # 
  # ev5 <- c(as.POSIXct('2014-04-05'),as.POSIXct('2014-04-27'))
  # polygon(x= c(ev5,rev(ev5)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  # 
  # ev8 <- c(as.POSIXct('2015-03-04'),as.POSIXct('2015-03-15'))
  # polygon(x= c(ev8,rev(ev8)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  # 
  # ev10_1 <- c(as.POSIXct('2015-10-16'),as.POSIXct('2015-10-31'))
  # polygon(x= c(ev10_1,rev(ev10_1)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  # 
  # ev10_3 <- c(as.POSIXct('2015-11-12'),as.POSIXct('2015-11-23'))
  # polygon(x= c(ev10_3,rev(ev10_3)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  # 
  # ev10_4 <- c(as.POSIXct('2015-11-29'),as.POSIXct('2015-12-08'))
  # polygon(x= c(ev10_4,rev(ev10_4)),y=c(0,0,10,10),col=rgb(0,0,1,0.2),border=NA) #Azul
  
    ### Sombreado rojo NO surgencia ##
  # ev1 <- c(as.POSIXct('2014-02-24'),as.POSIXct('2014-02-28'))
  # polygon(x= c(ev1,rev(ev1)),y=c(0,0,10,10),col=rgb(1,0,0,0.2),border=NA) #Red
  # 
  # ev6 <- c(as.POSIXct('2014-07-10'),as.POSIXct('2014-07-30'))
  # polygon(x= c(ev6,rev(ev6)),y=c(0,0,10,10),col=rgb(1,0,0,.3),border=NA) #Red
  # 
  # # ev7 <- c(as.POSIXct('2015-02-21'),as.POSIXct('2015-03-24'))
  # # polygon(x= c(ev7,rev(ev7)),y=c(0,0,10,10),col=rgb(1,0,0,.3),border=NA) #Red
  # 
  # ev9 <- c(as.POSIXct('2015-04-01'),as.POSIXct('2015-05-12'))
  # polygon(x= c(ev9,rev(ev9)),y=c(0,0,10,10),col=rgb(1,0,0,.3),border=NA) #Red
  # 
  # ev9bis <- c(as.POSIXct('2015-05-14'),as.POSIXct('2015-06-07'))
  # polygon(x= c(ev9bis,rev(ev9bis)),y=c(0,0,10,10),col=rgb(1,0,0,.3),border=NA) #Red
  # 
  # ev10_2 <- c(as.POSIXct('2015-11-05'),as.POSIXct('2015-11-11'))
  # polygon(x= c(ev10_2,rev(ev10_2)),y=c(0,0,10,10),col=rgb(1,0,0,.3),border=NA) #Red
  # 
  # ev11 <- c(as.POSIXct('2016-09-09'),as.POSIXct('2016-09-21'))
  # polygon(x= c(ev11,rev(ev11)),y=c(0,0,10,10),col=rgb(1,0,0,.3),border=NA) #Red
  
  lines(datos$dateUTC, datos$DO,col=do_col)
  dev.off()
}

##  ----------------------  ##
  #### - Boxplot DO total - ####
##  ----------------------  ##
lab.size=1
axes.size=1
{
  png(filename = "Boxplot_DO_total.png",width = 5,height = 8,res=400,units = "cm")
  par(mar=c(4, 3.5,.7,.7))
  boxplot(datos$DO,col=do_col, ylim=c(0,10), yaxs="i",
          cex.axis=axes.size,las=2,cex=0.5, pch=16)
  grid(ny = 10,nx=0,lty = 1)
  par(new=T)
  boxplot(datos$DO,col=do_col, ylim=c(0,10), yaxs="i",
          cex.axis=axes.size,las=2,cex=0.5, pch=16)
  mtext(text = do_lab, side = 2, cex = lab.size, line = 2.1)
  abline(h=2, lty=2, col="red")
  dev.off()
}

##  ------------------------  ##
  #### - Boxplot trimestral - ####
##  ------------------------  ##
{
datos$trim <- NA
datos$trim [month(datos$dateUTC)>=1  & month(datos$dateUTC)<=3] <- 1
datos$trim [month(datos$dateUTC)>=4  & month(datos$dateUTC)<=6] <- 2
datos$trim [month(datos$dateUTC)>=7  & month(datos$dateUTC)<=9] <- 3
datos$trim [month(datos$dateUTC)>=10  & month(datos$dateUTC)<=12] <- 4
names <- c("ene-mar","abr-jun", "jul-sep", "oct-dic")
}
{
  png(filename = "Boxplot_DO_trimestre.png",width = 13,height = 8,res=300,units = "cm")
  par(mar=c(4, 3.5,.7,.7))
  boxplot(DO ~ trim, data = datos,col=do_col, ylim=c(0,10), yaxs="i",
          cex.axis=axes.size,las=1,cex=0.5, pch=16,names=F, ylab="",xlab="" )
  grid(ny = 10,nx=0,lty = 1)
  par(new=T)
  aa<-boxplot(DO ~ trim, data = datos,col=do_col, ylim=c(0,10), yaxs="i",
              cex.axis=axes.size,las=1,cex=0.5, pch=16,names=F, ylab="",xlab="" )
  abline(h=2, lty=2, col="red")
  axis(1, at=c(1,2,3,4),labels = names,
       tick = F,line =-0.3,xpd=T,cex.axis=axes.size)
  axis(1, at=c(1,2,3,4),labels = paste("n =",aa$n),
       tick = F,line = 0.5,xpd=T,cex.axis=0.85)
  
  mtext(text = do_lab, side = 2, cex = lab.size, line = 2.1)
  mtext(text = "Trimestre", side = 1, cex = lab.size, line = 2.7)
  dev.off()
}

##  ---------------------------  ##
  #### - Porcentaje de hipoxia - ####
##  ---------------------------  ##
hist(datos$DO)
do <- datos$DO
do <- na.omit(do)
summary(do)
( length(which(do<=2)) /length(do) )*100
( length(which(do<=4 & do>2)) /length(do) )*100
( length(which(do<4)) /length(do) )*100
rm(do)

datos[which(datos$DO == 0),]

##  -----------  ##
#### // CUI \\ ####
##  -----------  ##
  #### - Serie CUI completa ####
cui <- datos[,c("dateUTC","CUI","CUI_daily_rollmean")]
cui <- na.omit(cui)

lim1a =as.POSIXct('2014-01-22 00:00:00 UTC')
lim4b = as.POSIXct('2016-10-01 00:00:00 UTC')

eje2mes  <- seq(from = as.POSIXct('2014-02-01 00:00:00 UTC'),
                to = as.POSIXct('2016-12-01 00:00:00 UTC'),by="2 months")
lab2mes <- format(eje2mes,format="%Y-%b")
lab.size=1
axes.size=0.8
year<- c(as.POSIXct('2014-01-01'),as.POSIXct('2015-01-01'),as.POSIXct('2016-01-01'))

{
  png("CUI_completa.png",width = 13,height = 8,units = "cm", res = 400)
  par(mar=c(5.2,3.5,1,1))
  plot(cui$dateUTC, cui$CUI, col=cui_col, type="l",
       ylim=c(-250,250),xlab = "",ylab = "",xaxt="n",yaxs="i", xaxs="i",
       xlim=c(lim1a, lim4b),las=2, 
       cex.axis=axes.size,cex.lab=lab.size)
  axis(1, at = eje2mes, labels = substring(eje2mes,1,7),
       cex.axis=axes.size, las=2)
  
  mtext(text = "Fecha", side = 1, cex = lab.size, line = 4.1)
  mtext(text = cui_lab, side = 2, cex = lab.size, line = 2.2)
  abline(h=0, lty=2, col=1,lwd=1)
  abline(v=year, lty=3, col="gray40")
  dev.off()
}

  #### BOXPLOT CUI total ####
lab.size=1
axes.size=1
{
  png(filename = "Boxplot_CUI_total.png",width = 5,height = 8,res=300,units = "cm")
  par(mar=c(4, 3.5,.7,.7))
  boxplot(datos$CUI,col=cui_col,ylim=c(-200,200), yaxs="i",
          cex.axis=axes.size,las=2,cex=0.5, pch=16)
  grid(ny = 8,nx=0,lty = 1)
  par(new=T)
  boxplot(datos$CUI,col=cui_col, ylim=c(-200,200), yaxs="i",
          cex.axis=axes.size,las=2,cex=0.5, pch=16)
  mtext(text = cui_lab, side = 2, cex = lab.size, line = 2.1)
  abline(h=0, lty=2, col=1)
  dev.off()
}

  #### Boxplot CUI trimestral ####
{
lab.size=1
axes.size=1

datos$trim <- NA
datos$trim [month(datos$dateUTC)>=1  & month(datos$dateUTC)<=3] <- 1
datos$trim [month(datos$dateUTC)>=4  & month(datos$dateUTC)<=6] <- 2
datos$trim [month(datos$dateUTC)>=7  & month(datos$dateUTC)<=9] <- 3
datos$trim [month(datos$dateUTC)>=10  & month(datos$dateUTC)<=12] <- 4
names <- c("ene-mar","abr-jun", "jul-sep", "oct-dic")
}

{
  png(filename = "Boxplot_CUI_trimestre.png",width = 13,height = 8,res=300,units = "cm")
  par(mar=c(4, 3.5,.7,.7))
  boxplot(CUI ~ trim, data = datos,col=cui_col, ylim=c(-200,200), yaxs="i",
          cex.axis=axes.size,las=1,cex=0.5, pch=16,names=F, ylab="",xlab="" )
  grid(ny = 8,nx=0,lty = 1)
  par(new=T)
  aa<-boxplot(CUI ~ trim, data = datos,col=cui_col, ylim=c(-200,200), yaxs="i",
              cex.axis=axes.size,las=1,cex=0.5, pch=16,names=F, ylab="",xlab="" )
  abline(h=0, lty=2, col=1)
  axis(1, at=c(1,2,3,4),labels = names,
       tick = F,line =-0.3,xpd=T,cex.axis=axes.size)
  axis(1, at=c(1,2,3,4),labels = paste("n =",aa$n),
       tick = F,line = 0.5,xpd=T,cex.axis=0.85)
  
  mtext(text = cui_lab, side = 2, cex = lab.size, line = 2.1)
  mtext(text = "Trimestre", side = 1, cex = lab.size, line = 2.7)
  dev.off()
}


##  -----------  ##
#### // SSTGrad \\ ####
##  -----------  ##
  #### Serie SSTGrad completa ####
sstgrad <- datos[,c("dateUTC","SSTGrad")]
sstgrad <- na.omit(sstgrad)

lim1a =as.POSIXct('2014-01-22 00:00:00 UTC')
lim4b = as.POSIXct('2016-10-01 00:00:00 UTC')

eje2mes  <- seq(from = as.POSIXct('2014-02-01 00:00:00 UTC'),
                to = as.POSIXct('2016-12-01 00:00:00 UTC'),by="2 months")
lab2mes <- format(eje2mes,format="%Y-%b")
year<- c(as.POSIXct('2014-01-01'),as.POSIXct('2015-01-01'),as.POSIXct('2016-01-01'))

lab.size=1
axes.size=0.8

{
  png("sstgrad_completa.png",width = 13,height = 8,units = "cm", res = 400)
  par(mar=c(5.2,3.6,1,1))
  plot(sstgrad$dateUTC, sstgrad$SSTGrad, col=sstgrad_col, type="l",
       xlab = "",ylab = "",xaxt="n",yaxs="i", xaxs="i",
       xlim=c(lim1a, lim4b),ylim=c(-0.04,0.08), las=2, 
       cex.axis=axes.size,cex.lab=lab.size)
  axis(1, at = eje2mes, labels = substring(eje2mes,1,7),
       cex.axis=axes.size, las=2)
  
  mtext(text = "Fecha", side = 1, cex = lab.size, line = 4.1)
  mtext(text = sstgrad_lab, side = 2, cex = lab.size, line = 2.3)
  abline(h=0, lty=2, col=1,lwd=1)
  abline(v=year, lty=3, col="gray40")
  dev.off()
}

  #### BOXPLOT SSTgrad total ####
lab.size=1
axes.size=0.9

{
  png(filename = "Boxplot_sstgrad_total.png",width = 5,height = 8,res=400,units = "cm")
  par(mar=c(4, 3.6,.7,.7))
  boxplot(datos$SSTGrad,col=sstgrad_col, ylim=c(-0.04,0.08), yaxs="i",
          cex.axis=axes.size,las=2,cex=0.5, pch=16)
  grid(ny = 6,nx=0,lty = 1)
  par(new=T)
  boxplot(datos$SSTGrad,col=sstgrad_col, ylim=c(-0.04,0.08), yaxs="i",
          cex.axis=axes.size,las=2,cex=0.5, pch=16)
  mtext(text = sstgrad_lab, side = 2, cex = lab.size, line = 2.3)
  abline(h=0, lty=2, col=1)
  dev.off()
}

  #### Boxplot SSTGrad trimestral ####
{
lab.size=1
axes.size=1
datos$trim <- NA
datos$trim [month(datos$dateUTC)>=1  & month(datos$dateUTC)<=3] <- 1
datos$trim [month(datos$dateUTC)>=4  & month(datos$dateUTC)<=6] <- 2
datos$trim [month(datos$dateUTC)>=7  & month(datos$dateUTC)<=9] <- 3
datos$trim [month(datos$dateUTC)>=10  & month(datos$dateUTC)<=12] <- 4
names <- c("ene-mar","abr-jun", "jul-sep", "oct-dic")
}

{
  png(filename = "Boxplot_SSTgrad_trimestre.png",width = 13,height = 8,res=300,units = "cm")
  par(mar=c(4, 4.7,.7,.7))
  aa <-boxplot(SSTGrad ~ trim, data = datos,col=sstgrad_col, ylim=c(-0.04,0.08), yaxs="i",
               cex.axis=axes.size,las=1,cex=0.5, pch=16,names=F, ylab="",xlab="" )
  grid(ny = 6,nx=0,lty = 1)
  par(new=T)
  boxplot(SSTGrad ~ trim, data = datos,col=sstgrad_col, ylim=c(-0.04,0.08), yaxs="i",
          cex.axis=axes.size,las=1,cex=0.5, pch=16,names=F, ylab="",xlab="" )
  abline(h=0, lty=2, col=1)
  axis(1, at=c(1,2,3,4),labels = names,
       tick = F,line =-0.3,xpd=T,cex.axis=axes.size)
  axis(1, at=c(1,2,3,4),labels = paste("n =",aa$n),
       tick = F,line = 0.5,xpd=T,cex.axis=0.85)
  
  mtext(text = sstgrad_lab, side = 2, cex = lab.size, line = 2.9)
  mtext(text = "Trimestre", side = 1, cex = lab.size, line = 2.7)
  dev.off()
}



##  ----------------------------  ##
#### // Analisis por eventos \\ ####
##  ----------------------------  ##
setwd(wd_graficas)
wd_graficas <- getwd()

ev_vector<- (unique(datos$EventoHypox))
ev_vector<- ev_vector[-c(1,6)] # Borrar NA's 
ev_vector

for (ii in ev_vector){
  setwd(wd_graficas)
  #print(ii)
  evento <- subset(datos, subset = datos$EventoHypox == ii)
  EvName <- as.character(ii)
  dateVector <- evento$dateUTC
  (TimeRange <-range(dateVector))
  duracion <- diff.Date(TimeRange)
  DO_min <- min(evento$DO)
  DO_max <- max(evento$DO)
  print(ii)
  print(TimeRange)
  print(duracion)
  print(DO_min)
  print("______________") 
  
  # Crear carpeta de resultados por eventos
  dir_name = paste0(EvName,"_",
                    substr(TimeRange[1],1,10), "__",
                    substr(TimeRange[2],1,10))
  dir.create(path = dir_name,showWarnings = F)
  setwd(dir_name)
  
  ##  ------------------------------------  ##
  ####  - Grafica DO eventos de hipoxia - ####
  ##  ------------------------------------  ##
  {graph_name <- paste0(ii,"DO",".png")
  png(graph_name,width = 13,height = 8,units = "cm", res = 400)
  par(mar=c(4,3.5,1.6,0.8))
  plot(evento$dateUTC, evento$DO, col=do_col, type="o",pch=16, 
       xlab="", ylab="",ylim=c(0,DO_max+0.8),
       cex.axis=axes.size, cex.lab=lab.size, 
       las=1, yaxs="i",xaxs="i",cex=.5)
  lines(evento$dateUTC, evento$DO_daily_rollmean, col=colpal[10], lwd=2)
  abline(h=2, col="red",lwd=1, lty=2)
  mtext(text = "Fecha", side = 1, cex = lab.size, line = 2)
  mtext(text = paste("Min = ",DO_min), side = 1, cex = lab.size, line = 3)
  mtext(text = do_lab, side = 2, cex = lab.size, line = 2)
  mtext(side = 3, line = 0.4,cex = lab.size,
        text = paste(substr(TimeRange[1],1,10), "-",
                     substr(TimeRange[2],1,10),sep = "  "))
  legend("bottomleft",lty=c(1,1),pch=c(16,NA),
         lwd=2,cex=0.9, inset = c(.01,.01),bty="n",
         col=c(do_col,colpal[10]),legend = c("DO","Media móvil diaria"))
  dev.off()}
  
  ##  ---------------------------------------------  ##
  #### - Grafico Paralelo DO, SST, CUI y SSTGrad - ####
  ##  ---------------------------------------------  ##
  {graph_name <- paste0(EvName,"_paralelo.png")
  png(graph_name,width = 17, height = 15,units = "cm", res = 400)
  par(mfrow=c(6,1))
  par(mar=c(0, 4, 0, 5))
  plot(0,0,axes = F, type="n",ylab="")
  #SST
  par(mar=c(0.1, 4, 0, 5))
  plot(evento$dateUTC, evento$SST, type="o",
       col=sst_col,pch=20, xaxt="n", ylab="", xlab="",las=2)
  mtext(side = 2, line = 2.7, text = sst_lab, cex=0.8)
  mtext(side=3, line=0.8, text = paste(substr(TimeRange[1],1,10), " - ",
                                       substr(TimeRange[2],1,10)))
  #DO
  par(mar=c(0.1, 4, 0.1, 5))
  plot(evento$dateUTC, evento$DO, type="o",yaxt="n",
       col=do_col,pch=20, xaxt="n", ylab="", xlab="",las=2)
  abline(h=2, col=2, lty=2)
  axis(side = 4,las=2)
  mtext(side = 4, line = 2.5, text = do_lab, cex=0.8)
  #CUI
  par(mar=c(0.1, 4, 0.1, 5))
  plot(evento$dateUTC, evento$CUI_daily_rollmean, type="o",
       col=cui_col,pch=20, xaxt="n", ylab="", xlab="",las=2)
  lines(evento$dateUTC, evento$CUI_daily_interpolated, col=cui_col)
  abline(h=0, col=1, lty=2)
  mtext(side = 2, line = 2.2, text = cui_lab, cex=0.8)
  #SSTGrad
  par(mar=c(0.1, 4, 0.1, 5))
  plot(evento$dateUTC, evento$SSTGrad, type="o",yaxt="n",
       col=sstgrad_col,pch=20, ylab="", xlab="",las=1, cex.axis=1.2)
  lines(evento$dateUTC, evento$SSTGrad_interpolated, col=sstgrad_col)
  abline(h=0, col=1, lty=2)
  axis(side=4, las=2)
  mtext(side = 4, line = 4, text = sstgrad_lab, cex=0.8)
  mtext(side = 1, line = 2.8, text = "Fecha", cex=1.1)
  dev.off()}
  
  ##  ------------------------------  ##
  #### - Distribucion estadistica - ####
  ##  ------------------------------  ##
  
  #Seleccionar por horas:
  evento_H <- evento[minute(evento$dateUTC)==0,]
  rownames(evento_H)<- 1:length(evento_H$dateUTC)
  # Normalidad de los datos
  DO_shp <- shapiro.test(x = evento_H$DO)
  SST_shp <- shapiro.test(x = evento_H$SST)
  CUI_shp <- shapiro.test(x = evento_H$CUI_daily_interpolated)
  SSTGrad_shp <- shapiro.test(x = evento_H$SSTGrad_interpolated)
  
  # Histogramas
  hist_name <- paste0(EvName,"_histogramas.png")
  {png(hist_name,width = 10,height = 20,units = "cm", res = 400)
  par(mfrow =c(4,1))
  par(mar=c(4,4, 1,1))
  #DO
  hist(evento_H$DO, main="",xlab=do_lab,border = do_col,
       freq=F,ylab="Densidad")
  lines(density(na.omit(evento_H$DO)))
  legend("topleft", bty = "n",
         legend = paste("Shapiro p-val =", round(DO_shp$p.value,3)))
  #SST
  hist(evento_H$SST, main="",xlab=sst_lab,border = sst_col,
       freq=F,ylab="Densidad")
  lines(density(na.omit(evento_H$SST)),lwd=1)
  legend("topleft", bty = "n",
         legend = paste("Shapiro p-val =", round(SST_shp$p.value,3)))
  #CUI
  hist(evento_H$CUI_daily_interpolated, main="",xlab=sst_lab,
       border = cui_col, freq=F,ylab="Densidad")
  lines(density(na.omit(evento_H$CUI_daily_interpolated)),lwd=1)
  legend("topleft", bty = "n",
         legend = paste("Shapiro p-val =", round(CUI_shp$p.value,3)))
  # SSTGrad
  hist(evento_H$SSTGrad_interpolated, main="",xlab=sstgrad_lab,border = sstgrad_col,
       freq=F,ylab="Densidad")
  lines(density(na.omit(evento_H$SSTGrad_interpolated)),lwd=1)
  legend("topleft", bty = "n",
         legend = paste("Shapiro p-val =", round(SSTGrad_shp$p.value,3)))
  dev.off()}
  ## ------------------------------- ##
  #### -- Estadistica bivariada -- ####
  ## ------------------------------- ##
  ## ------------- ##
  #### DO vs SST ####
  ## ------------  ##
  cor <- cor.test(evento_H$SST, evento_H$DO, method = "spearman")
  rho <- round(cor$estimate,2)
  pval <- cor$p.value
  if (pval >0.05){
    signif = paste("pval =",round(pval,2))
  } else if (pval <= 0.05 & pval > 0.01){
    signif = "*"
  } else if (pval <= 0.01 & pval > 0.001){
    signif = "**"
  } else if (pval <= 0.001){
    signif ="***"
  }
  
  # Grafico correlacion DO vs SST#
  cor_name <- paste(EvName,"DO_vs_SST_corr.png")
  {
    png(cor_name,width = 10,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3.5,3.5,2,1))
    #Caida:
    plot(evento_H$SST, evento_H$DO,las=1,
         pch=20,col=1,
         xlab="", 
         ylab="")
    #Etiquetas: 
    mtext(side = 1,line = 2.2,text = sst_lab)  #etiqueta ejeX
    mtext(text = do_lab,side = 2,line = 2) #etiqueta ejeY (DO)
    mtext(text = paste(TimeRange[1],"-",TimeRange[2]),side = 3,line = 0.8)
    
    #Linea regresion:
    abline(lm(evento_H$DO ~ evento_H$SST),
           lty=2,col=sst_col, lwd=2)
    #Puede ajustar coordenadas manualmente:
    legend("topleft", legend = paste0("rho = ",rho," ", signif)
           ,cex=0.8,box.col = "white", #bty = "n",
           text.font = 2, inset = c(0.01,0.01))
    dev.off()
  }
  
  # Correlacion cruzada DO vs SST #
  Xcor_name <- paste(EvName, "CrossCorr_DO_SST.png")
  {
    png(Xcor_name,width = 9,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3,3,3,0.5))
    croscor<-ccf(x = evento_H$SST,y=evento_H$DO, lag.max = 50,
                 xlab="", ylab="", main="")
    mtext(text = "Desfase (horas)",side = 1,line = 2)
    mtext(text = "r",side = 2,line = 2)
    mtext(text = paste(TimeRange[1]," ",TimeRange[2]),side = 3,line = 1.5,cex = lab.size)
    mtext(text = paste("DO  SST"),side = 3,line = 0.5,cex = lab.size)
    abline(v=0, lty=2,col="blue")
    dev.off()
  }
  
  ## ------------  ##
  #### DO vs CUI ####
  ## ------------  ##
  cor <- cor.test(evento_H$CUI_daily_interpolated, evento_H$DO, method = "spearman")
  rho <- round(cor$estimate,2)
  pval <- cor$p.value
  if (pval >0.05){
    signif = paste("pval =",round(pval,2))
  } else if (pval <= 0.05 & pval > 0.01){
    signif = "*"
  } else if (pval <= 0.01 & pval > 0.001){
    signif = "**"
  } else if (pval <= 0.001){
    signif ="***"
  }
  
  # Grafico correlacion #
  cor_name <- paste(EvName,"DO_vs_CUI_corr.png")
  {
    png(cor_name,width = 10,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3.5,3.5,2,1))
    #Caida:
    plot(evento_H$CUI_daily_interpolated, evento_H$DO,las=1,
         pch=20,col=1,
         xlab="", 
         ylab="")
    #Etiquetas: 
    mtext(side = 1,line = 2.3,text = cui_lab)  #etiqueta ejeX
    mtext(text = do_lab,side = 2,line = 2) #etiqueta ejeY (DO)
    mtext(text = paste(TimeRange[1],"-",TimeRange[2]),side = 3,line = 0.8)
    
    #Linea regresion:
    abline(lm(evento_H$DO ~ evento_H$CUI_daily_interpolated),
           lty=2,col=cui_col, lwd=2)
    #Puede ajustar coordenadas manualmente:
    legend("topleft", legend = paste0("rho = ",rho," ", signif)
           ,cex=0.8,box.col = "white", #bty = "n",
           text.font = 2, inset = c(0.01,0.01))
    dev.off()
  }
  
  # Correlacion cruzada #
  Xcor_name <- paste(EvName, "CrossCorr_DO_CUI.png")
  {
    png(Xcor_name,width = 9,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3,3,3,0.5))
    croscor<-ccf(x = evento_H$CUI_daily_interpolated,y=evento_H$DO, lag.max = 50,
                 xlab="", ylab="", main="")
    mtext(text = "Desfase (horas)",side = 1,line = 2)
    mtext(text = "r",side = 2,line = 2)
    mtext(text = paste(TimeRange[1]," ",TimeRange[2]),side = 3,line = 1.5,cex = lab.size)
    mtext(text = paste("DO  CUI"),side = 3,line = 0.5,cex = lab.size)
    abline(v=0, lty=2,col="blue")
    dev.off()
  }

  ## -----------  ##
  #### DO vs SSTGrad ####
  ##------------  ##
  cor <- cor.test(evento_H$SSTGrad_interpolated, evento_H$DO, method = "spearman")
  rho <- round(cor$estimate,2)
  pval <- cor$p.value
  if (pval >0.05){
    signif = paste("pval =",round(pval,2))
  } else if (pval <= 0.05 & pval > 0.01){
    signif = "*"
  } else if (pval <= 0.01 & pval > 0.001){
    signif = "**"
  } else if (pval <= 0.001){
    signif ="***"
  }
  
  # Grafico correlacion DO vs SSTGrad#
  cor_name <- paste(EvName,"DO_vs_SSTGrad_corr.png")
  {
    png(cor_name,width = 10,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3.5,3.5,2,1))
    #Caida:
    plot(evento_H$SSTGrad_interpolated, evento_H$DO,las=1,
         pch=20,col=1,
         xlab="", 
         ylab="")
    #Etiquetas: 
    mtext(side = 1,line = 2.3,text = sstgrad_lab)  #etiqueta ejeX
    mtext(text = do_lab,side = 2,line = 2) #etiqueta ejeY (DO)
    mtext(text = paste(TimeRange[1],"-",TimeRange[2]),side = 3,line = 0.8)
    
    #Linea regresion:
    abline(lm(evento_H$DO ~ evento_H$SSTGrad_interpolated),
           lty=2,col=sstgrad_col, lwd=2)
    #Puede ajustar coordenadas manualmente:
    legend("topleft", legend = paste0("rho = ",rho," ", signif)
           ,cex=0.8,box.col = "white", #bty = "n",
           text.font = 2, inset = c(0.01,0.01))
    dev.off()
  }
  
  # Correlacion cruzada DO vs SSTGrad#
  Xcor_name <- paste(EvName, "CrossCorr_DO_SSTGrad.png")
  {
    png(Xcor_name,width = 9,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3,3,3,0.5))
    croscor<-ccf(x = evento_H$SSTGrad_interpolated,y=evento_H$DO, lag.max = 50,
                 xlab="", ylab="", main="")
    mtext(text = "Desfase (horas)",side = 1,line = 2)
    mtext(text = "r",side = 2,line = 2)
    mtext(text = paste(TimeRange[1]," ",TimeRange[2]),side = 3,line = 1.5,cex = lab.size)
    mtext(text = paste("DO  SSTGrad"),side = 3,line = 0.5,cex = lab.size)
    abline(v=0, lty=2,col="blue")
    dev.off()
  }
  
  ## ------------- ##
  #### CUI vs SSTGrad ####
  ## ------------  ##
  cor <- cor.test(evento_H$CUI_daily_interpolated, evento_H$SSTGrad_interpolated, method = "spearman")
  rho <- round(cor$estimate,2)
  pval <- cor$p.value
  if (pval >0.05){
    signif = paste("pval =",round(pval,2))
  } else if (pval <= 0.05 & pval > 0.01){
    signif = "*"
  } else if (pval <= 0.01 & pval > 0.001){
    signif = "**"
  } else if (pval <= 0.001){
    signif ="***"
  }
  
  # Grafico correlacion CUI vs SSTGrad#
  cor_name <- paste(EvName,"CUI_vs_SSTGrad_corr.png")
  {
    png(cor_name,width = 10,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3.5,4,2,1))
    #Caida:
    plot(evento_H$CUI_daily_rollmean, evento_H$SSTGrad_interpolated,las=1,
         pch=20,col=1,
         xlab="", 
         ylab="")
    #Etiquetas: 
    mtext(side = 1,line = 2.3,text = cui_lab)  #etiqueta ejeX
    mtext(text = sstgrad_lab,side = 2,line = 2.5) #etiqueta ejeY 
    mtext(text = paste(TimeRange[1],"-",TimeRange[2]),side = 3,line = 0.7)
    
    #Linea regresion:
    abline(lm(evento_H$SSTGrad_interpolated ~ evento_H$CUI_daily_interpolated),
           lty=2,col=sst_col, lwd=2)
    #Puede ajustar coordenadas manualmente:
    legend("topleft", legend = paste0("rho = ",rho," ", signif)
           ,cex=0.8,box.col = "white", #bty = "n",
           text.font = 2, inset = c(0.01,0.01))
    dev.off()
  }
  
  # Correlacion cruzada CUI vs SSTGrad#
  Xcor_name <- paste(EvName, "CrossCorr_CUI_SSTGrad.png")
  {
    png(Xcor_name,width = 9,height = 8,units = "cm", res = 400)
    par(mfrow=c(1,1))
    par(mar=c(3,3,3,0.5))
    croscor<-ccf(x = evento_H$CUI_daily_interpolated,y=evento_H$SSTGrad_interpolated, lag.max = 50,
                 xlab="", ylab="", main="")
    mtext(text = "Desfase (horas)",side = 1,line = 2)
    mtext(text = "r",side = 2,line = 2)
    mtext(text = paste(TimeRange[1]," ",TimeRange[2]),side = 3,line = 1.5,cex = lab.size)
    mtext(text = paste("CUI  SSTGrad"),side = 3,line = 0.5,cex = lab.size)
    abline(v=0, lty=2,col="blue")
    dev.off()
  }
  
  ##  --------------------------  ##
  #### - Analisis de factores - ####
  ##  --------------------------  ##
  rad_list <- c("neutro 01","semineutro 02", "Evento 11")
  if (ii %in% rad_list){
    matriz_FA <- evento_H[,c("DO","SST",
                             "CUI_daily_interpolated",
                             "SSTGrad_interpolated","Rad")]
    matriz_FA <- na.omit(matriz_FA)
    colnames(matriz_FA) <- c("DO", "SST", "CUI", "SSTGrad","Rad")
  }else{
    matriz_FA <- evento_H[,c("DO","SST",
                             "CUI_daily_interpolated",
                             "SSTGrad_interpolated","SLH")]
    matriz_FA <- na.omit(matriz_FA)
    colnames(matriz_FA) <- c("DO", "SST", "CUI", "SSTGrad","SLH")
  }
  FA <- factanal(matriz_FA, factors = 2, rotation = "varimax")
  print(EvName)
  print(FA)
  
  fact_name <- paste(EvName, "Factor.png")
  {png(fact_name, width = 10,height = 8,units = "cm", res = 400)
  par(mar=c(4,4,2,1))
  plot(FA$loadings, pch=16, cex=1.2, col ="red",
       xlab="",ylab="",xlim=c(-1.1, 1.1),ylim=c(-1.1, 1.1),
       cex.axis=1)
  mtext(text = "Factor 1",side = 1,line = 2, cex=1.1,font = 2) # XLAB
  mtext(text = "Factor 2",side = 2,line = 2, cex=1.1,font = 2) # YLAB
  abline(h=0, v=0, col="blue", lty=2,lwd=2)
  arrows(0, 0, FA$loadings[,1], FA$loadings[,2], length = 0, lty = 1, lwd=2)
  text(FA$loadings+0.06, labels = names(matriz_FA), cex=1.1)
  #title(main = titulo, sub = paste("p-val = ",round(matriz_fa$PVAL,2)) ) 
  mtext(text = paste(TimeRange[1]," ",TimeRange[2]),side = 3,line = 0.7, cex=1.1)
  mtext(text = paste("p-val = ",round(FA$PVAL,2)),
        side = 1,line = 3, cex=1)
  dev.off()
  }
  print("______________") 
  setwd(wd_graficas)
}


## ------------------------------------------------------  ##
#### // Dispersion  con desfase de maxima correlacion \\ ####
## ------------------------------------------------------  ##
# Seleccione evento:
setwd(wd_graficas)
table(datos$EventoHypox)
EvName <- "Evento 08"
{
evento <- subset(datos, subset = datos$EventoHypox == EvName)
evento_H <- evento[minute(evento$dateUTC)==0,]
dateVector <- evento_H$dateUTC
(TimeRange <-range(dateVector))
dir_name = paste0(EvName,"_",
                  substr(TimeRange[1],1,10), "__",
                  substr(TimeRange[2],1,10))
dir.create(path = dir_name,showWarnings = F)
setwd(dir_name)
}

# Seleccione variables X y Y e indique sus nombres
XX <- evento_H$CUI_daily_interpolated
X_name <- "CUI"
X_lab <- cui_lab
X_col <- cui_col
YY <- evento_H$DO
Y_name <- "DO"
Y_lab <- do_lab
ccf(XX,YY,lag.max = 60)

# Indique desfase deseado:
lag = -60
len <- length(XX)

if (lag < 0){
  print("Desfase negativo")
  XXlag <- XX[-c( (len-abs(lag)+1):len ) ]
  YYlag <- YY[- c(1:abs(lag))]
} else {
  print("Desfase negativo")
  XXlag<- XX[-c(1:abs(lag))]
  YYlag <- YY[-c( (len-abs(lag)+1):len )]
}

{
  summary(lm(YYlag~XXlag))
  (cor_lag <- cor.test(XXlag,YYlag, method = "spearman"))
  r_lag <- round(cor_lag$estimate,2)
  pval_lag <- cor_lag$p.value
  
  if (pval_lag >0.05){
    signif_lag = paste("pval =",round(pval,2))
  } else if (pval_lag <= 0.05 & pval_lag > 0.01){
    signif_lag = "*"
  } else if (pval_lag <= 0.01 & pval_lag > 0.001){
    signif_lag = "**"
  } else if (pval_lag <= 0.001){
    signif_lag ="***"
  }
  print(r_lag)
  print(signif_lag)
}

cor_lag_name <- paste(EvName,Y_name,X_name,"corr_lag.png")

{
  png(cor_lag_name,width = 10,height = 8,units = "cm", res = 400)
  par(mfrow=c(1,1))
  par(mar=c(3.3,3.3,3,0.5))
  #Caida:
  plot(XXlag,YYlag,las=1,
       pch=20,col=1,
       xlab="", 
       ylab="",
       cex.axis=axes.size, cex.lab=lab.size)
  #Etiquetas: 
  mtext(side = 1,line = 2.3,text = X_lab)
  mtext(side = 2,line = 1.9,text = Y_lab)
  mtext(text = paste(TimeRange[1],"-",TimeRange[2]),side = 3,line = 1.3,cex = lab.size)
  mtext(text = paste("desfase =",lag,"(horas)"),side = 3,line = 0.3,cex = lab.size)
  
  #Linea regresion:
  abline(lm(YYlag~XXlag),
         lty=2, lwd=2,col=X_col ) 
  #Puede ajustar coordenadas manualmente:
  legend("bottomleft", legend = paste0("rho = ",r_lag, signif_lag),
         bty = "n",text.font = 2, inset = c(0.01,0.01))
  dev.off()
}


## ---------------------------------------------------  ##
#### // Analisis de factores con SSTGrad desfasado \\ ####
## ---------------------------------------------------  ##
setwd(wd_graficas)
table(datos$EventoHypox)
EvName <- "Evento 08"
{
  evento <- subset(datos, subset = datos$EventoHypox == EvName)
  evento_H <- evento[minute(evento$dateUTC)==0,]
  dateVector <- evento_H$dateUTC
  (TimeRange <-range(dateVector))
  dir_name = paste0(EvName,"_",
                    substr(TimeRange[1],1,10), "__",
                    substr(TimeRange[2],1,10))
  dir.create(path = dir_name,showWarnings = F)
  setwd(dir_name)
}

# Seleccione matriz con un solo SSTGrad (elija desfase )
matriz_FA <- evento_H[,c("DO","SST",
                         "CUI_daily_interpolated",
                         #"SSTGrad_interpolated", #Sin desfase
                         #"SSTGrad_lag_24H", #Desfase 1 dia,
                         #"SSTGrad_lag_48H", #Desfase 2 dias
                         "SSTGrad_lag_72H" , #Desfase 3 dias
                         "SLH")]
matriz_FA <- na.omit(matriz_FA)
colnames(matriz_FA) <- c("DO", "SST", "CUI", "SSTGrad","SLH")

FA <- factanal(matriz_FA, factors = 2, rotation = "varimax")
print(EvName)
print(FA)

## Indicar la proporcion de varianza de cada factor
Var1 = "(32 %)"
Var2 = "(24 %)"
# Indique desfase seleccionado para SSTGrad
lag = "SSTGrad lag -3 dias"

fact_name <- paste(EvName, lag,"Factor.png")
{png(fact_name,width = 10,height = 8,units = "cm", res = 400)
  par(mar=c(4,4,2.5,1))
  plot(FA$loadings, pch=16, cex=1.2, col ="red",
       xlab="",ylab="",xlim=c(-1.1, 1.1),ylim=c(-1.1, 1.1),
       cex.axis=1)
  mtext(text = paste("Factor 1", Var1),side = 1,line = 2, cex=1.1,font = 2) # XLAB
  mtext(text = paste("Factor 2",Var2),side = 2,line = 2, cex=1.1,font = 2) # YLAB
  abline(h=0, v=0, col="blue", lty=2,lwd=2)
  arrows(0, 0, FA$loadings[,1], FA$loadings[,2], length = 0, lty = 1, lwd=2)
  text(FA$loadings+0.06, labels = names(matriz_FA), cex=1.1)
  mtext(text = paste(TimeRange[1]," ",TimeRange[2]),side = 3,line = 1.3, cex=1.1)
  mtext(text = lag, side = 3,line = 0.3)
  mtext(text = paste("p-val = ",round(FA$PVAL,2)),
        side = 1,line = 3, cex=1)
  dev.off()
  }

setwd(wd_graficas)


##  -------------------  ##
#### // Cross-wavelets \\ ####
##  -------------------  ##
{
#install.packages("biwavelet")
library(biwavelet)
setwd(wd_graficas)
dir.create("Cross-Wavelets",showWarnings = F)
setwd("Cross-Wavelets/")
datos_H <- datos[minute(datos$dateUTC)==0,]

datos_H$dia <-  yday(datos_H$dateUTC) + (hour(datos_H$dateUTC)/24)+
  (minute(datos_H$dateUTC)/1440)

}

  #### 1 Separar por tramos ####
# Seleccionar "Tramo 1", "Tramo 2", "Tramo 3" o "Tramo 4"
EvName <- "Tramo 4" 
{
  evento <- subset(datos_H, subset = datos_H$tramo ==EvName)  ### Por tramo
  evento <- evento[,c("dateUTC","dia","DO","SST",
                      "SSTGrad_interpolated","CUI_daily_interpolated")]
  evento <- na.omit(evento)
  rownames(evento) <- 1:length(evento$dateUTC)
  (TimeRange <-range(evento$dateUTC))
  (diff.Date(TimeRange))
  titulo <- paste(substr(TimeRange[1],1,10)," - ", substr(TimeRange[2],1,10))
  evento$diaX1  <- evento$dateUTC - as.POSIXct("2014-01-01") ### Dias desde 2014-01-01
  evento$diaX2 <-  yday(evento$dateUTC) + (hour(evento$dateUTC)/24)
  XX <- 1:length(evento$dateUTC)
}

  #### 2. Definir series Y1 y Y2 ####
# Seleccione variables e indique sus atributos (color, etiqueta y nombre)
{
  Y1 <- evento$DO   #Variable 1
  Y1col <- do_col   #Color de la variable (do_col, cui_col, sst_col, sstgrad_col)
  Y1lab <- do_lab
  Y1name <- "DO"
  
  Y2 <- evento$CUI_daily_interpolated #Variable2
  Y2col <- cui_col #Color variable2
  Y2lab <- cui_lab
  Y2name <- "CUI"
  fig.name <- paste0("Paralelo ",EvName," Xwave ",Y1name,"-",Y2name,".png")
}

  #### 3. Calcular Cross-wavelet ####
xwave <- xwt(cbind(evento$diaX1,Y1), cbind(evento$diaX1,Y2))

  #### 4 Grafico Paralelo Crosswavelet y 2 series  ####
lab.size=1.2
axes.size=1.2
{
  png(filename = fig.name,width = 15,height = 20,units = "cm",res=400)
  par(mfrow=c(3,1)) #3 filas 1 columna
  ## Xwavelet ##
  par(mar = c(0.2,5.2, 4,7.5))
  plot(xwave,  plot.cb = T, plot.phase = T,
       cex.axis=axes.size,las=2,
       lwd.sig = 1, xaxt = "n",
       xlab="", ylab="",
       yaxs="i",las=2)
  # Eje X (por fechas):
  axs.pos <- seq(xwave$t[1],to = tail(xwave$t,1), length.out = 5) #Ubicacion en ejeX en unidades de "t"
  axs.date <- seq(1, length(XX),length.out = 5) # indices correspondientes a axs.pos
  axs.lab <- substr(evento$dateUTC[axs.date],start = 1,stop = 10) #Etiquetas
  axis(1, at=axs.pos, labels = NA,cex.axis=axes.size) # Pinta lineas sin etiquetas
  mtext("Periodo (días)",side = 2,line = 3.3,cex = lab.size)
  mtext(paste(EvName,"Cross-wavelet",Y1name,Y2name),side = 3.3,line = 1.5,cex = 1.5)
  
  ## Serie 1 #
  par(mar = c(1.5, 5.2, 1.5, 7.5))
  plot(evento$diaX1, Y1, col=Y1col,
       type="l",xaxs="i",yaxs="i",xaxt="n",
       xlab = "", ylab="",cex.axis=axes.size,las=2)
  axis(1, at=axs.pos, labels = NA,cex.axis=axes.size) # Pinta lineas sin etiquetas
  mtext(text = Y1lab,side = 2,line = 2.9,cex = lab.size)
  if (Y1name == "DO"){
    abline(h=2, col="red",lty=2) ## Linea de hipoxia
  }
  abline(h=0, lty=2) # Linea 0 para indices
  
  ## Serie 2 #
  par(mar = c(5,5.2,0.2,7.5))
  plot(evento$diaX1, Y2,col=Y2col,
       type="l",xaxs="i",yaxs="i",xaxt="n",
       xlab = "", ylab="",cex.axis=axes.size,las=2)
  axis(1, at=axs.pos, labels = axs.lab,cex.axis=axes.size)
  #mtext(text =paste(Y2lab,Y2unit), side = 2,line = 2.9,cex = lab.size)
  mtext(text =Y2lab,side = 2,line = 2.9,cex = lab.size)
  mtext("Fecha", side=1, line=2.8, cex=lab.size)
  abline(h=0, lty=2) # Linea 0 para indices
  
  dev.off()
}
