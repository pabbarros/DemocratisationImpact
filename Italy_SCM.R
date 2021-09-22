rm(list = ls())
#### DATA - ALL DATABASES ######
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(SciViews)
library(reshape2)
library(Synth)
library(data.table)
library(plm)

ERT <- read.csv("ERT.csv")
dem <- read_excel("dem.xlsx")
ep_study <- read_excel("ep_study.xlsx")
colnames(dem)[2]<-"country_name"
db <- read_excel("db.xlsx")
db$year <- as.integer(db$year)
db1 <- read_excel("db1.xlsx")
dblm <- db[,c(1,3,5,7,10,12,15,16,17,19)]
dblm <- merge(dblm, dem, by.x = c("year", "countrycode"), by.y = c("year", "country_text_id"))
dblm <- dblm[,c(1:12)]
countrycode <- (unique(db1[,"countrycode"]))
countrycode <- data.frame(countrycode)
countrycode[,"codenum"] <- 1:141
db1 <- merge(db1, countrycode, by="countrycode")
#db1 <- db1[which(db1$codenum %in% c(3:11, 13:17,19:22,24:25,27,29:31,33:34,36,39,40,43:47,49,51:56,58:64,67:71)),]
db1$year <- as.integer(db1$year)
db1$primary_barro <- as.integer(db1$primary_barro)
db1$secondary_barro <- as.integer(db1$secondary_barro)

##### CONTROLS #####
#Italy in Europe
source("controls_w.R")
italy <- controls(dem, ep_study, c="ITA")
italy_ <- data.frame(Reduce(rbind, italy))
italy_ <- italy_[!(is.na(italy_$country_text_id)),]
italy_c <- unique(italy_$country_name)
italy_in <- italy_[italy_$Continent_Name=="Europe",]
italy_in <- italy_in[!(is.na(italy_in$country_text_id)),]
italy_in_c <- unique(italy_in$country_name)
italy_in_c <- c(italy_in_c, "Italy")
italy_c <- c(italy_c, "Italy")


#####IN EUROPE ####
db <- db1[which(db1$country %in% italy_in_c),]
db <- db[db$year>1933 & db$year<1955,]
table(db$codenum)

db <- db[which((db$codenum %in% c(49,63,104,107))),]

dataprep.out <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 63,
  controls.identifier = c(49,104,107),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.out$X1
dataprep.out$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.out <- synth(dataprep.out)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
synth.tables$tab.w
synth.tables$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y1  <- dataprep.out$Y1
Y1synth <- dataprep.out$Y0 %*% synth.out$solution.w
plot(1934:1954, Y1, ylim=c(-10,10), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines(1934:1954, Y1synth, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-25, 40), Legend = c("Italy",
                                        "Synthetic Italy"), Legend.position = "bottomright")
abline(v=c(1944), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tables$tab.w)
xtable(synth.tables$tab.pred)
###PLACEBOS

dataprep.out.plac <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 49,
  controls.identifier = c(104,107),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.out.plac$X1
dataprep.out.plac$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac <- synth(dataprep.out.plac)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tables.plac <- synth.tab(dataprep.res = dataprep.out.plac, synth.res = synth.out.plac)
synth.tables.plac$tab.w
synth.tables.plac$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y1plac  <- dataprep.out.plac$Y1
Y1synthplac <- dataprep.out.plac$Y0 %*% synth.out.plac$solution.w
plot(1934:1954, Y1plac, ylim=c(-10,10), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines(1934:1954, Y1synthplac, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_PLC1.png")
path.plot(synth.res = synth.out.plac, dataprep.res = dataprep.out.plac,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-20, 60), Legend = c("Greece",
                                        "Synthetic Greece"), Legend.position = "bottomright")
dev.off()



dataprep.out.plac2 <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 104,
  controls.identifier = c(49,107),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.out.plac2$X1
dataprep.out.plac2$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac2 <- synth(dataprep.out.plac2)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tables.plac2 <- synth.tab(dataprep.res = dataprep.out.plac2, synth.res = synth.out.plac2)
synth.tables.plac2$tab.w
synth.tables.plac2$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y12  <- dataprep.out.plac2$Y1
Y1synth2 <- dataprep.out.plac2$Y0 %*% synth.out.plac2$solution.w
plot(1934:1954, Y12, ylim=c(-10,10), type="l", xlab = "Year", ylab = "ESP GDP per capita growth rate")
lines(1934:1954, Y1synth2, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_PLC2.png")
path.plot(synth.res = synth.out.plac2, dataprep.res = dataprep.out.plac2,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-30, 135), Legend = c("Portugal",
                                        "Synthetic Portugal"), Legend.position = "topleft")
dev.off()


dataprep.out.plac3 <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 107,
  controls.identifier = c(49,104),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.out.plac3$X1
dataprep.out.plac3$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac3 <- synth(dataprep.out.plac3)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tables.plac3 <- synth.tab(dataprep.res = dataprep.out.plac3, synth.res = synth.out.plac3)
synth.tables.plac3$tab.w
synth.tables.plac3$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y13 <- dataprep.out.plac3$Y1
Y1synth3 <- dataprep.out.plac3$Y0 %*% synth.out.plac3$solution.w
plot(1934:1954, Y12, ylim=c(-10,10), type="l", xlab = "Year", ylab = "ESP GDP per capita growth rate")
lines(1934:1954, Y1synth2, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_PLC3.png")
path.plot(synth.res = synth.out.plac3, dataprep.res = dataprep.out.plac3,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-40, 180), Legend = c("Romania",
                                        "Synthetic Romania"), Legend.position = "topleft")
dev.off()

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_ALL.png")
plot(1934:1954, Y1, ylim=c(-32,180), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1934:1954, Y1plac, type="l", lty=2, col="blue")
lines(1934:1954, Y12, type="l", lty=2, col="blue4")
lines(1934:1954, Y13, type="l", lty=2, col="blue3")
legend("topleft", legend = c("Italy", "Greece", "Portugal","Romania"),
       col = c("red", "blue", "blue4", "blue3"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(1944), col=c("black"), lwd=c(2))
dev.off()

### DENSITY ####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1plac - Y1synthplac)
effects['placebo2'] <- (Y12 - Y1synth2)
effects['placebo3'] <- (Y13 - Y1synth3)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10
mean_p3 <- sum(effects[effects$years>11,'placebo3'])/10

for (i in 1:10){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
  std[i,'placebo3'] <- (effects[effects$year==std[i,'year'],'placebo3']-mean_p3)^2
  
}

std[11,'year']<- 'std_10' 
std[11,'years']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))
std[11,'placebo3'] <- sqrt(sum(na.omit(std$placebo3))/(nrow(std)-2))


std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))
std[12,'placebo3'] <- sqrt(sum(std[std$years<=13,'placebo3'])/(13-11-1))


std[13,'year']<- 'std_3' 
std[13,'years']<- 'std_3'
std[13,'country'] <- sqrt(sum(std[std$years<=14,'country'])/(14-11-1))
std[13,'placebo1'] <- sqrt(sum(std[std$years<=14,'placebo1'])/(14-11-1))
std[13,'placebo2'] <- sqrt(sum(std[std$years<=14,'placebo2'])/(14-11-1))
std[13,'placebo3'] <- sqrt(sum(std[std$years<=14,'placebo3'])/(14-11-1))


std[14,'year']<- 'std_4' 
std[14,'years']<- 'std_4'
std[14,'country'] <- sqrt(sum(std[std$years<=15,'country'])/(15-11-1))
std[14,'placebo1'] <- sqrt(sum(std[std$years<=15,'placebo1'])/(15-11-1))
std[14,'placebo2'] <- sqrt(sum(std[std$years<=15,'placebo2'])/(15-11-1))
std[14,'placebo3'] <- sqrt(sum(std[std$years<=15,'placebo3'])/(15-11-1))


std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))
std[15,'placebo3'] <- sqrt(sum(std[std$years<=16,'placebo3'])/(16-11-1))


density <- as.data.frame(c("1year", "2year", "3year", "4year", "5year", "10year"))
colnames(density) <- "effect"
density[density$effect=="1year", "ATE_country"]<- effects[effects$years==12,"country"]
density[density$effect=="2year", "ATE_country"]<- sum(effects[effects$years==12 | effects$yearss==13,"country"])/2
density[density$effect=="3year", "ATE_country"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"country"])/3
density[density$effect=="4year", "ATE_country"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"country"])/4
density[density$effect=="5year", "ATE_country"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"country"])/5
density[density$effect=="10year", "ATE_country"]<- sum(effects[effects$years>11,"country"])/(21-11)

density[density$effect=="1year", "ATE_placebo1"]<- effects[effects$years==12,"placebo1"]
density[density$effect=="2year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13,"placebo1"])/2
density[density$effect=="3year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo1"])/3
density[density$effect=="4year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo1"])/4
density[density$effect=="5year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo1"])/5
density[density$effect=="10year", "ATE_placebo1"]<- sum(effects[effects$years>11,"placebo1"])/(21-11)

density[density$effect=="1year", "ATE_placebo2"]<- effects[effects$years==12,"placebo2"]
density[density$effect=="2year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13,"placebo2"])/2
density[density$effect=="3year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo2"])/3
density[density$effect=="4year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo2"])/4
density[density$effect=="5year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo2"])/5
density[density$effect=="10year", "ATE_placebo2"]<- sum(effects[effects$years>11,"placebo2"])/(21-11)

density[density$effect=="1year", "ATE_placebo3"]<- effects[effects$years==12,"placebo3"]
density[density$effect=="2year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13,"placebo3"])/2
density[density$effect=="3year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo3"])/3
density[density$effect=="4year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo3"])/4
density[density$effect=="5year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo3"])/5
density[density$effect=="10year", "ATE_placebo3"]<- sum(effects[effects$years>11,"placebo3"])/(21-11)


norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_country'],
    sd = std[std$years=='std_10', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo1'],
    sd = std[std$year=='std_10', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo2'],
    sd = std[std$year=='std_10', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo3'],
    sd = std[std$year=='std_10', 'placebo3']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-50,50), ylim=c(0, 0.06), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Portugal","Romania"),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2, 2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_country'],
    sd = std[std$years=='std_5', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_placebo1'],
    sd = std[std$year=='std_5', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_placebo2'],
    sd = std[std$year=='std_5', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_placebo3'],
    sd = std[std$year=='std_5', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-50,50), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Portugal","Romania"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_country'],
    sd = std[std$year=='std_4', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo1'],
    sd = std[std$year=='std_4', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo2'],
    sd = std[std$year=='std_4', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo3'],
    sd = std[std$year=='std_4', 'placebo3']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-50,50), ylim=c(0, 0.045), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Portugal","Romania"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='3year','ATE_country'],
    sd = std[std$year=='std_3', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='3year','ATE_placebo1'],
    sd = std[std$year=='std_3', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='3year','ATE_placebo2'],
    sd = std[std$year=='std_3', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='3year','ATE_placebo3'],
    sd = std[std$year=='std_3', 'placebo3']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.09), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Portugal","Romania"),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='2year','ATE_country'],
    sd = std[std$year=='std_2', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='2year','ATE_placebo1'],
    sd = std[std$year=='std_2', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='2year','ATE_placebo2'],
    sd = std[std$year=='std_2', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='2year','ATE_placebo3'],
    sd = std[std$year=='std_2', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/ITA_IN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Portugal","Romania"),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()




##### IN WORLD ####

dbw <- db1[which(db1$country %in% italy_c),]
dbw <- dbw[dbw$year>1933 & dbw$year<1955,]
table <- as.data.frame(table(dbw$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)
dbw <- dbw[which((dbw$codenum %in% c(5, 13, 16, 26, 28, 32, 38, 40, 49, 50, 54, 63, 81, 90, 99, 
                                     101, 104, 107, 119, 137))),]

dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 63,
  controls.identifier = c(5, 13, 16, 26, 28, 32, 38, 40, 49, 50, 54, 81, 90, 99, 
                          101, 104, 107, 119, 137),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.outw$X1
dataprep.outw$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.outw <- synth(dataprep.outw)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y1w  <- dataprep.outw$Y1
Y1synthw <- dataprep.outw$Y0 %*% synth.outw$solution.w
plot(1934:1954, Y1w, ylim=c(-20,50), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines(1934:1954, Y1synthw, type="l", lty=2)


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA.png")
path.plot(synth.res = synth.outw, dataprep.res = dataprep.outw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-20, 35), Legend = c("Italy",
                                        "Synthetic Italy"), Legend.position = "bottomright")
abline(v=c(1944), col=c("black"), lwd=c(2))
dev.off()


xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)

###PLACEBOS

dataprep.out.placw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 49,
  controls.identifier = c(13, 16, 26, 28, 32, 38, 40, 5, 50, 54, 81, 90, 99, 
                          101, 104, 107, 119, 137),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.out.placw$X1
dataprep.out.placw$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.out.placw <- synth(dataprep.out.placw)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tables.placw <- synth.tab(dataprep.res = dataprep.out.placw, synth.res = synth.out.placw)
synth.tables.placw$tab.w
synth.tables.placw$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y1placw  <- dataprep.out.placw$Y1
Y1synthplacw <- dataprep.out.placw$Y0 %*% synth.out.placw$solution.w
plot( 1934:1954, Y1placw, ylim=c(-10,10), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines( 1934:1954, Y1synthplacw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_PLC1.png")
path.plot(synth.res = synth.out.placw, dataprep.res = dataprep.out.placw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-20, 50), Legend = c("Greece",
                                        "Synthetic Greece"), Legend.position = "bottomright")
dev.off()



dataprep.out.plac2w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 5,
  controls.identifier = c(28, 13, 16, 26, 32, 38, 40, 49, 50, 54, 81, 90, 99, 
                          101, 104, 107, 119, 137),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.out.plac2w$X1
dataprep.out.plac2w$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac2w <- synth(dataprep.out.plac2w)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tables.plac2w <- synth.tab(dataprep.res = dataprep.out.plac2w, synth.res = synth.out.plac2w)
synth.tables.plac2w$tab.w
synth.tables.plac2w$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y12w  <- dataprep.out.plac2w$Y1
Y1synth2w <- dataprep.out.plac2w$Y0 %*% synth.out.plac2w$solution.w
plot(1934:1954, Y12w, ylim=c(-10,10), type="l", xlab = "Year", ylab = "PTR GDP per capita growth rate")
lines(1934:1954, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_PLC2.png")
path.plot(synth.res = synth.out.plac2w, dataprep.res = dataprep.out.plac2w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-10, 15), Legend = c("Argentina",
                                        "Synthetic Argentina"), Legend.position = "bottomleft")
dev.off()


dataprep.out.plac3w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1934:1943 ,
  special.predictors = list(
    list("averagegdppc", seq(1935, 1940,5), "mean"),
    #list("primary_barro", seq(1935, 1940,5), "mean"),
    list("ggdp", 1938:1943, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 13,
  controls.identifier = c(5,28, 49, 16, 26, 32, 38, 40, 50, 54, 81, 90, 99, 
                          101, 104, 107, 119, 137),
  time.optimize.ssr = 1934:1943,
  time.plot = 1934:1954)

# InspeÃÂÃÂ§ÃÂÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂÃÂ?sitcas da observaÃÂÃÂ§ÃÂÃÂ£o tratada e da 
# matriz das observaÃÂÃÂ§ÃÂÃÂµes da variÃÂÃÂ¡vel de resultado da tratada no perÃÂÃÂ?odo prÃÂÃÂ©-tratamento, repectivamente.
dataprep.out.plac3w$X1
dataprep.out.plac3w$Z1

# Correr a funÃÂÃÂ§ÃÂÃÂ£o synth() (que optimiza a funÃÂÃÂ§ÃÂÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac3w <- synth(dataprep.out.plac3w)

# ExtraÃÂÃÂ§ÃÂÃÂ£o dos resultados da optimizaÃÂÃÂ§ÃÂÃÂ£o feita pela funÃÂÃÂ§ÃÂÃÂ£o synth(). ReplicaÃÂÃÂ§ÃÂÃÂ£o parcial da Tabela 3.
synth.tables.plac3w <- synth.tab(dataprep.res = dataprep.out.plac3w, synth.res = synth.out.plac3w)
synth.tables.plac3w$tab.w
synth.tables.plac3w$tab.pred

# ReplicaÃÂÃÂ§ÃÂÃÂ£o da Figura 1
Y13w  <- dataprep.out.plac3w$Y1
Y1synth3w <- dataprep.out.plac3w$Y0 %*% synth.out.plac3w$solution.w
plot(1934:1954, Y12w, ylim=c(-10,10), type="l", xlab = "Year", ylab = "PTR GDP per capita growth rate")
lines(1934:1954, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_PLC3.png")
path.plot(synth.res = synth.out.plac3w, dataprep.res = dataprep.out.plac3w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-13, 20), Legend = c("Bolivia",
                                        "Synthetic Bolivia"), Legend.position = "bottomleft")
dev.off()

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_ALL.png")
plot(1934:1954, Y1w, ylim=c(-20,50), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1934:1954, Y1placw, type="l", lty=2, col="blue")
lines(1934:1954, Y12w, type="l", lty=2, col="blue4")
lines(1934:1954, Y13w, type="l", lty=2, col="blue3")
legend("topleft", legend = c("Italy", "Greece", "Argentina", "Bolivia"),
       col = c("red", "blue", "blue4", "blue3"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(1944), col=c("black"), lwd=c(2))
dev.off()

###DENSITY#####
effects<- as.data.frame(Y1w - Y1synthw)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1placw - Y1synthplacw)
effects['placebo2'] <- (Y12w - Y1synth2w)
effects['placebo3'] <- (Y13w - Y1synth3w)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10
mean_p3 <- sum(effects[effects$years>11,'placebo3'])/10

for (i in 1:10){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
  std[i,'placebo3'] <- (effects[effects$year==std[i,'year'],'placebo3']-mean_p3)^2
}

std[11,'year']<- 'std_10' 
std[11,'years']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))
std[11,'placebo3'] <- sqrt(sum(na.omit(std$placebo3))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))
std[12,'placebo3'] <- sqrt(sum(std[std$years<=13,'placebo3'])/(13-11-1))

std[13,'year']<- 'std_3' 
std[13,'years']<- 'std_3'
std[13,'country'] <- sqrt(sum(std[std$years<=14,'country'])/(14-11-1))
std[13,'placebo1'] <- sqrt(sum(std[std$years<=14,'placebo1'])/(14-11-1))
std[13,'placebo2'] <- sqrt(sum(std[std$years<=14,'placebo2'])/(14-11-1))
std[13,'placebo3'] <- sqrt(sum(std[std$years<=14,'placebo3'])/(14-11-1))


std[14,'year']<- 'std_4' 
std[14,'years']<- 'std_4'
std[14,'country'] <- sqrt(sum(std[std$years<=15,'country'])/(15-11-1))
std[14,'placebo1'] <- sqrt(sum(std[std$years<=15,'placebo1'])/(15-11-1))
std[14,'placebo2'] <- sqrt(sum(std[std$years<=15,'placebo2'])/(15-11-1))
std[14,'placebo3'] <- sqrt(sum(std[std$years<=15,'placebo3'])/(15-11-1))

std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))
std[15,'placebo3'] <- sqrt(sum(std[std$years<=16,'placebo3'])/(16-11-1))

density <- as.data.frame(c("1year", "2year", "3year", "4year", "5year", "10year"))
colnames(density) <- "effect"
density[density$effect=="1year", "ATE_country"]<- effects[effects$years==12,"country"]
density[density$effect=="2year", "ATE_country"]<- sum(effects[effects$years==12 | effects$yearss==13,"country"])/2
density[density$effect=="3year", "ATE_country"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"country"])/3
density[density$effect=="4year", "ATE_country"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"country"])/4
density[density$effect=="5year", "ATE_country"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"country"])/5
density[density$effect=="10year", "ATE_country"]<- sum(effects[effects$years>11,"country"])/(21-11)

density[density$effect=="1year", "ATE_placebo1"]<- effects[effects$years==12,"placebo1"]
density[density$effect=="2year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13,"placebo1"])/2
density[density$effect=="3year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo1"])/3
density[density$effect=="4year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo1"])/4
density[density$effect=="5year", "ATE_placebo1"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo1"])/5
density[density$effect=="10year", "ATE_placebo1"]<- sum(effects[effects$years>11,"placebo1"])/(21-11)

density[density$effect=="1year", "ATE_placebo2"]<- effects[effects$years==12,"placebo2"]
density[density$effect=="2year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13,"placebo2"])/2
density[density$effect=="3year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo2"])/3
density[density$effect=="4year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo2"])/4
density[density$effect=="5year", "ATE_placebo2"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo2"])/5
density[density$effect=="10year", "ATE_placebo2"]<- sum(effects[effects$years>11,"placebo2"])/(21-11)

density[density$effect=="1year", "ATE_placebo3"]<- effects[effects$years==12,"placebo3"]
density[density$effect=="2year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13,"placebo3"])/2
density[density$effect=="3year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo3"])/3
density[density$effect=="4year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo3"])/4
density[density$effect=="5year", "ATE_placebo3"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo3"])/5
density[density$effect=="10year", "ATE_placebo3"]<- sum(effects[effects$years>11,"placebo3"])/(21-11)


norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_country'],
    sd = std[std$years=='std_10', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo1'],
    sd = std[std$year=='std_10', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo2'],
    sd = std[std$year=='std_10', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo3'],
    sd = std[std$year=='std_10', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.12), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Argentina", "Bolivia"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_country'],
    sd = std[std$year=='std_5', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_placebo1'],
    sd = std[std$year=='std_5', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_placebo2'],
    sd = std[std$year=='std_5', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='5year','ATE_placebo3'],
    sd = std[std$year=='std_5', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.12), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Argentina", "Bolivia"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_country'],
    sd = std[std$year=='std_4', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo1'],
    sd = std[std$year=='std_4', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo2'],
    sd = std[std$year=='std_4', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo3'],
    sd = std[std$year=='std_4', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.09), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Argentina", "Bolivia"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_country'],
    sd = std[std$year=='std_3', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_placebo1'],
    sd = std[std$year=='std_3', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_placebo2'],
    sd = std[std$year=='std_3', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_placebo3'],
    sd = std[std$year=='std_3', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.09), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Argentina", "Bolivia"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_country'],
    sd = std[std$year=='std_2', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_placebo1'],
    sd = std[std$year=='std_2', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_placebo2'],
    sd = std[std$year=='std_2', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_placebo3'],
    sd = std[std$year=='std_2', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/ITA_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.06), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
legend("topleft", legend = c("Italy", "Greece", "Argentina", "Bolivia"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()




