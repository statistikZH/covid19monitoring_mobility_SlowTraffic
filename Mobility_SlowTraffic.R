# Mobility_SlowTraffic_OEV.R
# Import libraries
require(tidyquant)
require(xts)
require(anytime)
library (readr)
library (lattice)
library(chron)
library(reshape)








#only median values distances and without restschweiz for simplicity 
#!!! Mistakes in coding vars intervista where means are concerned!!!
mobility_intervista<-subset(intervista, grepl("median", intervista$variable_short)==T & grepl("distanz", intervista$variable_short)==T & location!="CH ohne ZH")

#write the final file for publication
write.table(mobility_intervista, "Mobility_intervista.csv", sep=",", fileEncoding="UTF-8", row.names = F)









as#Tagesfrequenzen Velo beide Richtungen
velodaily<-with(zhlv, tapply(VELO_OUT+VELO_IN, list(datum, FK_STANDORT), sum, na.rm=T))
#Velozählstellen an denen 2020 überhaupt etwas gemessen wird
zst<-apply(velodaily,2, sum, na.rm=T)>0
names(zst[zst==T])
zst<-c("52", "53", "60", "732", "1037", "1692", "2976", "2978", "2979", 
       "2980", "2986", "2989", "2991", "2993", "2996", "2997", "3277", 
       "3598", "3923", "3924", "3926", "3927")
velodaily<-velodaily[,zst]
velotot<-data.frame(velofreq=apply(velodaily,1, sum, na.rm=T))

velofreq<-data.frame(
  date=as.POSIXct(paste(rownames(velotot), "00:00:00", sep=" ")), 
  value=velotot$velofreq, 
  topic="mobilität", 
  variable="velo_tot_261",
  location="Stadt Zürich",
  unit="Anzahl Velos", 
  origin="Stadt Zürich",
  update="daily",
  public="ja",
  description="kumulierte tägliche Anzahl Velos an allen Velo-zählstellen der Stadt Zürich")

#Tagesfrequenzen FUSS beide Richtungen
fussdaily<-with(zhlv, tapply(FUSS_OUT+FUSS_IN, list(datum, FK_STANDORT), sum, na.rm=T))
#Fussverkehrszählstellen an denen 2020 überhaupt etwas gemessen wird
#zst<-apply(fussdaily,2, sum, na.rm=T)>0
#edit(names(zst)[zst==T]) 
zst<-c("2", "20", "39", "394", "1357", "1358", #? warum sind folgende Werte nicht dabei?: 32, 52, 53, 65, 2994
       "1681", "2640", "2641", "2642", "2961", "2984", "2985", 
       "3279")
fussdaily<-fussdaily[,zst]
fusstot<-data.frame(fussfreq=apply(fussdaily,1, sum, na.rm=T))

fussfreq<-data.frame(
  date=as.POSIXct(paste(rownames(fusstot), "00:00:00", sep=" ")), 
  value=fusstot$fussfreq, 
  topic="mobilität", 
  variable="fuss_tot_261",
  location="Stadt Zürich",
  unit="Anzahl Fussgänger", 
  origin="Stadt Zürich",
  update="daily",
  public="ja",
  description="kumulierte tägliche Anzahl Passanten (beide Richtungen) an allen aktiven Fussverkehrzählstellen der Stadt Zürich")


allfreq<-rbind(velofreq, fussfreq)







################################
# Preliminary Code subject to review, Results to be viewed with caution!! 
# Download data
urlfile="https://data.stadt-zuerich.ch/dataset/83ca481f-275c-417b-9598-3902c481e400/resource/b9308f85-9066-4f5b-8eab-344c790a6982/download/2020_verkehrszaehlungen_werte_fussgaenger_velo.csv"
zhlv<-data.frame(read_csv(url(urlfile)))

################################

# Format data according to data structure specification
#Zählstellen, Zahl der Messungen
zhlv$datum<-as.POSIXct(zhlv$DATUM)
zhlv$datum<-lubridate::date(zhlv$datum)

#Tagesfrequenzen Velo beide Richtungen
velodaily<-with(zhlv, tapply(VELO_OUT+VELO_IN, list(datum, FK_STANDORT), sum, na.rm=T))
#Velozählstellen an denen 2020 überhaupt etwas gemessen wird
zst<-apply(velodaily,2, sum, na.rm=T)>0
names(zst[zst==T])
zst<-c("52", "53", "60", "732", "1037", "1692", "2976", "2978", "2979", 
       "2980", "2986", "2989", "2991", "2993", "2996", "2997", "3277", 
       "3598", "3923", "3924", "3926", "3927")
velodaily<-velodaily[,zst]
velotot<-data.frame(velofreq=apply(velodaily,1, sum, na.rm=T))

velofreq<-data.frame(
  date=as.POSIXct(paste(rownames(velotot), "00:00:00", sep=" ")), 
  value=velotot$velofreq, 
  topic="mobilität", 
  variable="velo_tot_261",
  location="Stadt Zürich",
  unit="Anzahl Velos", 
  origin="Stadt Zürich",
  update="daily",
  public="ja",
  description="kumulierte tägliche Anzahl Velos an allen Velo-zählstellen der Stadt Zürich")

#Tagesfrequenzen FUSS beide Richtungen
fussdaily<-with(zhlv, tapply(FUSS_OUT+FUSS_IN, list(datum, FK_STANDORT), sum, na.rm=T))
#Fussverkehrszählstellen an denen 2020 überhaupt etwas gemessen wird
#zst<-apply(fussdaily,2, sum, na.rm=T)>0
#edit(names(zst)[zst==T]) 
zst<-c("2", "20", "39", "394", "1357", "1358", #? warum sind folgende Werte nicht dabei?: 32, 52, 53, 65, 2994
       "1681", "2640", "2641", "2642", "2961", "2984", "2985", 
       "3279")
fussdaily<-fussdaily[,zst]
fusstot<-data.frame(fussfreq=apply(fussdaily,1, sum, na.rm=T))

fussfreq<-data.frame(
  date=as.POSIXct(paste(rownames(fusstot), "00:00:00", sep=" ")), 
  value=fusstot$fussfreq, 
  topic="mobilität", 
  variable="fuss_tot_261",
  location="Stadt Zürich",
  unit="Anzahl Fussgänger", 
  origin="Stadt Zürich",
  update="daily",
  public="ja",
  description="kumulierte tägliche Anzahl Passanten (beide Richtungen) an allen aktiven Fussverkehrzählstellen der Stadt Zürich")


allfreq<-rbind(velofreq, fussfreq)

################################



#write the final file for publication
write.table(hardoev, "Mobility_SlowTraffic.csv", sep=",", fileEncoding="UTF-8", row.names = F)





# export result
write.table(allfreq, "./velo_fuss_freq_261.csv", sep=",", fileEncoding="UTF-8", row.names = F)







