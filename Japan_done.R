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
countrycode <- (unique(db1[,"countrycode"]))
countrycode <- data.frame(countrycode)
countrycode[,"codenum"] <- 1:141
db1 <- merge(db1, countrycode, by="countrycode")
db1$year <- as.integer(db1$year)
db1$primary_barro <- as.integer(db1$primary_barro)
db1$secondary_barro <- as.integer(db1$secondary_barro)

source("controls_w.R")
#Japan
Japan <- controls(dem, ep_study, c="JPN")
Japan_ <- data.frame(Reduce(rbind, Japan))
Japan_ <- Japan_[!(is.na(Japan_$country_text_id)),]
Japan_c <- unique(Japan_$country_name)
Japan_in <- Japan_[Japan_$Continent_Name=="Asia" |Japan_$Continent_Name=="Oceania" ,]
Japan_in <- Japan_in[!(is.na(Japan_in$country_text_id)),]
Japan_in_c <- unique(Japan_in$country_name)

Japan_in_c <- c(Japan_in_c, "Japan")
Japan_c <- c(Japan_c, "Japan")


#####IN ASIA ####
db <- db1[which(db1$country %in% Japan_in_c),]
db <- db[db$year>1946 & db$year<1963,]
db <- db[which(!(rownames(db) %in% c('5048', '5052', '5056', '5058', '5060', '5062', '5064', '5066', '5068'))),]
table(db$codenum)

#db <- db[which((db$codenum %in% c(65,76,102,120,138))),]

dataprep.out <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1947:1952 ,
  special.predictors = list(
    #list("averagegdppc", seq(1947, 1952,5), "mean"),
    #list("primary_barro", seq(1947, 1952,5), "mean"),
    list('gdp5average', 1948:1952, 'mean'),
    list("ggdp", 1949:1952, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 65,
  controls.identifier = c(102,120),
  time.optimize.ssr = 1947:1962,
  time.plot = 1947:1962)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.out$X1
dataprep.out$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.out <- synth(dataprep.out)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
synth.tables$tab.w
synth.tables$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y1  <- dataprep.out$Y1
Y1synth <- dataprep.out$Y0 %*% synth.out$solution.w

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-10, 20), Legend = c("Japan",
                                        "Synthetic Japan"), Legend.position = "topright")
abline(v=c(1952), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tables$tab.w)
xtable(synth.tables$tab.pred)

###PLACEBOS

dataprep.out.plac <-  dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1947:1952 ,
  special.predictors = list(
    #list("averagegdppc", seq(1947, 1952,5), "mean"),
    #list("primary_barro", seq(1947, 1952,5), "mean"),
    list('gdp5average', 1948:1952, 'mean'),
    list("ggdp", 1949:1952, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 120,
  controls.identifier = c(102,65),
  time.optimize.ssr = 1947:1952,
  time.plot = 1947:1962)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.out.plac$X1
dataprep.out.plac$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac <- synth(dataprep.out.plac)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tables.plac <- synth.tab(dataprep.res = dataprep.out.plac, synth.res = synth.out.plac)
synth.tables.plac$tab.w
synth.tables.plac$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y1plac  <- dataprep.out.plac$Y1
Y1synthplac <- dataprep.out.plac$Y0 %*% synth.out.plac$solution.w
plot(1947:1962, Y1plac, ylim=c(-10,30), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines(1947:1962, Y1synthplac, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_PLC1.png")
path.plot(synth.res = synth.out.plac, dataprep.res = dataprep.out.plac,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-20, 30), Legend = c("South Korea",
                                        "Synthetic South Korea"), Legend.position = "topright")
dev.off()

dataprep.out.plac2 <-  dataprep(
  foo = db,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1947:1952 ,
  special.predictors = list(
    #list("averagegdppc", seq(1947, 1952,5), "mean"),
    #list("primary_barro", seq(1947, 1952,5), "mean"),
    list('gdp5average', 1948:1952, 'mean'),
    list("ggdp", 1949:1952, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 102,
  controls.identifier = c(120,65),
  time.optimize.ssr = 1947:1952,
  time.plot = 1947:1962)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.out.plac$X1
dataprep.out.plac$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac2 <- synth(dataprep.out.plac)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tables.plac2 <- synth.tab(dataprep.res = dataprep.out.plac, synth.res = synth.out.plac)
synth.tables.plac2$tab.w
synth.tables.plac2$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y1plac2  <- dataprep.out.plac2$Y1
Y1synthplac2 <- dataprep.out.plac2$Y0 %*% synth.out.plac2$solution.w
plot(1947:1962, Y1plac2, ylim=c(-10,30), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines(1947:1962, Y1synthplac2, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_PLC2.png")
path.plot(synth.res = synth.out.plac2, dataprep.res = dataprep.out.plac2,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-10, 35), Legend = c("Philippines",
                                        "Synthetic Philippines"), Legend.position = "topright")
dev.off()


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_ALL.png")
plot(1947:1962, Y1, ylim=c(-20,35), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1947:1962, Y1plac, type="l", lty=2, col="blue")
lines(1947:1962, Y1plac2, type="l", lty=2, col="blue4")
legend("topright", legend = c("Japan", "South Korea","Philippines" ),
       col = c("red", "blue", 'blue4'), lty = c(1,2,2), cex = 0.8)
abline(v=c(1952), col=c("black"), lwd=c(2))
dev.off()

#####DENSITY####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-6:21
effects['placebo1'] <- (Y1plac - Y1synthplac)
effects['placebo2'] <- (Y1plac2- Y1synthplac2)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10

for (i in 1:10){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
}

std[11,'year']<- 'std_10' 
std[11,'years']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))

std[13,'year']<- 'std_3' 
std[13,'years']<- 'std_3'
std[13,'country'] <- sqrt(sum(std[std$years<=14,'country'])/(14-11-1))
std[13,'placebo1'] <- sqrt(sum(std[std$years<=14,'placebo1'])/(14-11-1))
std[13,'placebo2'] <- sqrt(sum(std[std$years<=14,'placebo2'])/(14-11-1))

std[14,'year']<- 'std_4' 
std[14,'years']<- 'std_4'
std[14,'country'] <- sqrt(sum(std[std$years<=15,'country'])/(15-11-1))
std[14,'placebo1'] <- sqrt(sum(std[std$years<=15,'placebo1'])/(15-11-1))
std[14,'placebo2'] <- sqrt(sum(std[std$years<=15,'placebo2'])/(15-11-1))

std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))

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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.1), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap3), col='orange', lty=2)
legend("topleft", legend = c("Japan", "South Korea", 'Philippines'),
       col = c("red", "blue", 'green'), lty = c(1,2,2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.07), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='pink', lty=2)
legend("topleft", legend = c("Japan", "South Korea", 'Philippines'),
       col = c("red", "blue", 'green'), lty = c(1,2,2), cex = 0.8)
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
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.06), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "South Korea", 'Philippines'),
       col = c("red", "blue", 'green'), lty = c(1,2,2), cex = 0.8)
dev.off()

norm_data <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='3year','ATE_country'],
    sd = std[std$year=='std_3', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='3year','ATE_placebo1'],
    sd = std[std$year=='std_3', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='3year','ATE_placebo2'],
    sd = std[std$year=='std_3', 'placebo2']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "South Korea", 'Philippines'),
       col = c("red", "blue", 'green'), lty = c(1,2,2), cex = 0.8)
dev.off()

norm_data <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='2year','ATE_country'],
    sd = std[std$year=='std_2', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='2year','ATE_placebo1'],
    sd = std[std$year=='std_2', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='2year','ATE_placebo2'],
    sd = std[std$year=='std_2', 'placebo2']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_IN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "South Korea", 'Philippines'),
       col = c("red", "blue", 'green'), lty = c(1,2,2), cex = 0.8)
dev.off()


##### IN WORLD ####

dbw <- db1[which(db1$country %in% Japan_c),]
dbw <- dbw[dbw$year>1941 & dbw$year<1963,,]
table <- as.data.frame(table(dbw$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)
table(dbw$codenum)

dbw <- dbw[which((dbw$codenum %in% c)),]

dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1942:1952 ,
  special.predictors = list(
    #list("averagegdppc", seq(1942, 1952,5), "mean"),
    list("primary_barro", seq(1945, 1950,5), "mean"),
    list('gdp5average', 1942:1952, 'mean'),
    list("ggdp", 1942:1952, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 65,
  controls.identifier = c(5, 13, 16, 28, 32, 38, 40, 49, 50, 54, 81, 90, 99, 100, 
                          101, 104, 119, 122, 137),
  time.optimize.ssr = 1942:1952,
  time.plot = 1942:1962)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.outw$X1
dataprep.outw$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.outw <- synth(dataprep.outw)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y1w  <- dataprep.outw$Y1
Y1synthw <- dataprep.outw$Y0 %*% synth.outw$solution.w
plot(1942:1962, Y1w, ylim=c(-25,15), type="l", xlab = "Year", ylab = "Finland GDP per capita growth rate")
lines(1942:1962, Y1synthw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN.png")
path.plot(synth.res = synth.outw, dataprep.res = dataprep.outw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-25,15), Legend = c("Japan",
                                       "Synthetic Japan"), Legend.position = "bottomright")

abline(v=c(1952), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)
###PLACEBOS

dataprep.out.placw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1942:1952 ,
  special.predictors = list(
    #list("averagegdppc", seq(1942, 1952,5), "mean"),
    list("primary_barro", seq(1945, 1950,5), "mean"),
    list('gdp5average', 1942:1952, 'mean'),
    list("ggdp", 1947:1952, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 99,
  controls.identifier = c(13, 16, 28, 32, 38, 40, 49, 50, 54, 81, 90, 5, 100,122, 
                          101, 104, 119, 137),
  time.optimize.ssr = 1942:1952,
  time.plot = 1942:1962)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.out.placw$X1
dataprep.out.placw$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.out.placw <- synth(dataprep.out.placw)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tables.placw <- synth.tab(dataprep.res = dataprep.out.placw, synth.res = synth.out.placw)
synth.tables.placw$tab.w
synth.tables.placw$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y1placw  <- dataprep.out.placw$Y1
Y1synthplacw <- dataprep.out.placw$Y0 %*% synth.out.placw$solution.w
plot(1907:1927, Y1placw, ylim=c(-10,10), type="l", xlab = "Year", ylab = "Italy GDP per capita growth rate")
lines(1907:1927, Y1synthplacw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_PLC1.png")
path.plot(synth.res = synth.out.placw, dataprep.res = dataprep.out.placw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-15, 10), Legend = c("Panama",
                                        "Synthetic Panama"), Legend.position = "bottomright")
dev.off()

dataprep.out.plac2w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1942:1952 ,
  special.predictors = list(
    #list("averagegdppc", seq(1942, 1952,5), "mean"),
    list('gdp5average', 1942:1952, 'mean'),
    list("primary_barro", seq(1945, 1950,5), "mean"),
    list("ggdp", 1947:1952, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 122,
  controls.identifier = c(5, 13, 16, 28, 32, 38, 40, 49, 50, 54, 81, 90, 99,100, 
                          101, 104, 119, 137),
  time.optimize.ssr = 1942:1952,
  time.plot = 1942:1962)

# InspeÃÂ§ÃÂ£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃÂ?sitcas da observaÃÂ§ÃÂ£o tratada e da 
# matriz das observaÃÂ§ÃÂµes da variÃÂ¡vel de resultado da tratada no perÃÂ?odo prÃÂ©-tratamento, repectivamente.
dataprep.out.plac2w$X1
dataprep.out.plac2w$Z1

# Correr a funÃÂ§ÃÂ£o synth() (que optimiza a funÃÂ§ÃÂ£o de perda encadeada para os pesos W e V)
synth.out.plac2w <- synth(dataprep.out.plac2w)

# ExtraÃÂ§ÃÂ£o dos resultados da optimizaÃÂ§ÃÂ£o feita pela funÃÂ§ÃÂ£o synth(). ReplicaÃÂ§ÃÂ£o parcial da Tabela 3.
synth.tables.plac2w <- synth.tab(dataprep.res = dataprep.out.plac2w, synth.res = synth.out.plac2w)
synth.tables.plac2w$tab.w
synth.tables.plac2w$tab.pred

# ReplicaÃÂ§ÃÂ£o da Figura 1
Y12w  <- dataprep.out.plac2w$Y1
Y1synth2w <- dataprep.out.plac2w$Y0 %*% synth.out.plac2w$solution.w
plot(1907:1927, Y12w, ylim=c(-10,10), type="l", xlab = "Year", ylab = "PTR GDP per capita growth rate")
lines(1907:1927, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_PLC2.png")
path.plot(synth.res = synth.out.plac2w, dataprep.res = dataprep.out.plac2w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-10, 12), Legend = c("Spain",
                                        "Synthetic Spain"), Legend.position = "bottomright")
dev.off()


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_ALL.png")
plot(1942:1962, Y1w, ylim=c(-25,15), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1942:1962, Y1placw, type="l", lty=2, col="blue")
lines(1942:1962, Y12w, type="l", lty=2, col="blue4")
#lines(1910:1930, Y12w, type="l", lty=2, col="green")
legend("bottomright", legend = c("Japan", "Panama","Spain"),
       col = c("red", "blue", "blue4"), lty = c(1,2,2), cex = 0.8)
abline(v=c(1952), col=c("black"), lwd=c(2))
dev.off()

#####DENSITY####
effects<- as.data.frame(Y1w - Y1synthw)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1placw - Y1synthplacw)
effects['placebo2'] <- (Y12w - Y1synth2w)
#effects['placebo3'] <- (Y13w - Y1synth3w)
#effects['placebo4'] <- (Y14w - Y1synth4w)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10
#mean_p3 <- sum(effects[effects$years>11,'placebo3'])/10
#mean_p4 <- sum(effects[effects$years>11,'placebo4'])/10


for (i in 1:10){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
  #std[i,'placebo3'] <- (effects[effects$year==std[i,'year'],'placebo3']-mean_p3)^2
}

std[11,'year']<- 'std_10' 
std[11,'years']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))
#std[11,'placebo3'] <- sqrt(sum(na.omit(std$placebo3))/(nrow(std)-2))
#std[11,'placebo4'] <- sqrt(sum(na.omit(std$placebo4))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))
#std[12,'placebo3'] <- sqrt(sum(std[std$years<=13,'placebo3'])/(13-11-1))
#std[12,'placebo4'] <- sqrt(sum(std[std$years<=13,'placebo4'])/(13-11-1))


std[13,'year']<- 'std_3' 
std[13,'years']<- 'std_3'
std[13,'country'] <- sqrt(sum(std[std$years<=14,'country'])/(14-11-1))
std[13,'placebo1'] <- sqrt(sum(std[std$years<=14,'placebo1'])/(14-11-1))
std[13,'placebo2'] <- sqrt(sum(std[std$years<=14,'placebo2'])/(14-11-1))
#std[13,'placebo3'] <- sqrt(sum(std[std$years<=14,'placebo3'])/(14-11-1))


std[14,'year']<- 'std_4' 
std[14,'years']<- 'std_4'
std[14,'country'] <- sqrt(sum(std[std$years<=15,'country'])/(15-11-1))
std[14,'placebo1'] <- sqrt(sum(std[std$years<=15,'placebo1'])/(15-11-1))
std[14,'placebo2'] <- sqrt(sum(std[std$years<=15,'placebo2'])/(15-11-1))
#std[14,'placebo3'] <- sqrt(sum(std[std$years<=15,'placebo3'])/(15-11-1))
#std[14,'placebo4'] <- sqrt(sum(std[std$years<=15,'placebo4'])/(15-11-1))


std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))
#std[15,'placebo3'] <- sqrt(sum(std[std$years<=16,'placebo3'])/(16-11-1))
#std[15,'placebo4'] <- sqrt(sum(std[std$years<=16,'placebo4'])/(16-11-1))


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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.1), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "Panama", "Spain"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.15), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "Panama", "Spain"),
       col = c("red", "blue", "green"), lty = c(1,2,2,2), cex = 0.8)
dev.off()

norm_data <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='4year','ATE_country'],
    sd = std[std$year=='std_4', 'country']
  )

norm_datap1<-
  rnorm(
    n = 100000,
    mean = density[density$effect=='4year','ATE_placebo1'],
    sd = std[std$year=='std_4', 'placebo1']
  )

norm_datap2<-
  rnorm(
    n = 100000,
    mean = density[density$effect=='4year','ATE_placebo2'],
    sd = std[std$year=='std_4', 'placebo2']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.13), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "Panama", "Spain"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.13), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "Panama", "Spain"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/JPN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.1), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Japan", "Panama", "Spain"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
dev.off()

