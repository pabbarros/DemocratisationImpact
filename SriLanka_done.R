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
countries <- unique(dem[, c("country_text_id", "country_name")])

source("controls_w.R")
#Sri Lanka
SriLanka <- controls(dem, ep_study, c="LKA")
SriLanka_ <- data.frame(Reduce(rbind, SriLanka))
SriLanka_ <- SriLanka_[!(is.na(SriLanka_$country_text_id)),]
SriLanka_c <- unique(SriLanka_$country_name)
SriLanka_in <- SriLanka_[SriLanka_$Continent_Name=="Asia" |SriLanka_$Continent_Name=="Oceania" ,]
SriLanka_in <- SriLanka_in[!(is.na(SriLanka_in$country_text_id)),]
SriLanka_in_c <- unique(SriLanka_in$country_name)

SriLanka_in_c <- c(SriLanka_in_c, "Sri Lanka")
SriLanka_c <- c(SriLanka_c, "Sri Lanka")


#####IN ASIA - not possible ####
db <- db1[which(db1$country %in% SriLanka_in_c),]
db <- db[db$year>1936 & db$year<1958,]
table(db$codenum)

db <- db[which((db$codenum %in% c(123,120))),]

##### IN WORLD ####

dbw <- db1[which(db1$country %in% SriLanka_c),]
dbw <- dbw[dbw$year>1936 & dbw$year<1958,,]
table(dbw$codenum)

dbw <- dbw[which((dbw$codenum %in% c(5,13,16,26,28,32,38,40,49,50,54,81,90,99,101,104,107,119,122,123,137))),]

dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1937:1947 ,
  special.predictors = list(
    list("averagegdppc", seq(1940, 1945,5), "mean"),
    list("primary_barro", seq(1940, 1945,5), "mean"),
    list("ggdp", 1937:1946, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 123,
  controls.identifier = c(5,13,16,26,28,32,38,40,49,50,54,81,90,99,101,104,119,122,137),
  time.optimize.ssr = 1937:1946,
  time.plot = 1937:1957)

# InspeÃ§Ã£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃ?sitcas da observaÃ§Ã£o tratada e da 
# matriz das observaÃ§Ãµes da variÃ¡vel de resultado da tratada no perÃ?odo prÃ©-tratamento, repectivamente.
dataprep.outw$X1
dataprep.outw$Z1

# Correr a funÃ§Ã£o synth() (que optimiza a funÃ§Ã£o de perda encadeada para os pesos W e V)
synth.outw <- synth(dataprep.outw)

# ExtraÃ§Ã£o dos resultados da optimizaÃ§Ã£o feita pela funÃ§Ã£o synth(). ReplicaÃ§Ã£o parcial da Tabela 3.
synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

# ReplicaÃ§Ã£o da Figura 1
Y1w  <- dataprep.outw$Y1
Y1synthw <- dataprep.outw$Y0 %*% synth.outw$solution.w
plot(1937:1957, Y1w, ylim=c(-12,20), type="l", xlab = "Year", ylab = "Finland Real GDP per capita Growth Rate")
lines(1937:1957, Y1synthw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA.png")
path.plot(synth.res = synth.outw, dataprep.res = dataprep.outw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-12,20), Legend = c("Sri Lanka",
                                        "Synthetic Sri Lanka"), Legend.position = "topleft")
abline(v=c(1947), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)

###PLACEBOS

dataprep.out.placw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1937:1947 ,
  special.predictors = list(
    list("averagegdppc", seq(1940, 1945,5), "mean"),
    list("primary_barro", seq(1940, 1945,5), "mean"),
    list("ggdp", 1937:1946, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 40,
  controls.identifier = c(5,13,16,26,28,32,38,49,50,54,81,90,99,101,104,119,122,137),
  time.optimize.ssr = 1937:1946,
  time.plot = 1937:1957)

# InspeÃ§Ã£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃ?sitcas da observaÃ§Ã£o tratada e da 
# matriz das observaÃ§Ãµes da variÃ¡vel de resultado da tratada no perÃ?odo prÃ©-tratamento, repectivamente.
dataprep.out.placw$X1
dataprep.out.placw$Z1

# Correr a funÃ§Ã£o synth() (que optimiza a funÃ§Ã£o de perda encadeada para os pesos W e V)
synth.out.placw <- synth(dataprep.out.placw)

# ExtraÃ§Ã£o dos resultados da optimizaÃ§Ã£o feita pela funÃ§Ã£o synth(). ReplicaÃ§Ã£o parcial da Tabela 3.
synth.tables.placw <- synth.tab(dataprep.res = dataprep.out.placw, synth.res = synth.out.placw)
synth.tables.placw$tab.w
synth.tables.placw$tab.pred

# ReplicaÃ§Ã£o da Figura 1
Y1placw  <- dataprep.out.placw$Y1
Y1synthplacw <- dataprep.out.placw$Y0 %*% synth.out.placw$solution.w
plot(1907:1927, Y1placw, ylim=c(-10,10), type="l", xlab = "Year", ylab = "Italy Real GDP per capita Growth Rate")
lines(1907:1927, Y1synthplacw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_PLC1.png")
path.plot(synth.res = synth.out.placw, dataprep.res = dataprep.out.placw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-12, 27), Legend = c("El Salvador",
                                        "Synthetic El Salvador"), Legend.position = "topleft")
dev.off()

dataprep.out.plac2w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1937:1947 ,
  special.predictors = list(
    list("averagegdppc", seq(1940, 1945,5), "mean"),
    list("primary_barro", seq(1940, 1945,5), "mean"),
    list("ggdp", 1937:1946, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 54,
  controls.identifier = c(5,13,16,26,28,32,38,40,49,50,81,90,99,101,104,119,122,137),
  time.optimize.ssr = 1937:1946,
  time.plot = 1937:1957)

# InspeÃ§Ã£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃ?sitcas da observaÃ§Ã£o tratada e da 
# matriz das observaÃ§Ãµes da variÃ¡vel de resultado da tratada no perÃ?odo prÃ©-tratamento, repectivamente.
dataprep.out.plac2w$X1
dataprep.out.plac2w$Z1

# Correr a funÃ§Ã£o synth() (que optimiza a funÃ§Ã£o de perda encadeada para os pesos W e V)
synth.out.plac2w <- synth(dataprep.out.plac2w)

# ExtraÃ§Ã£o dos resultados da optimizaÃ§Ã£o feita pela funÃ§Ã£o synth(). ReplicaÃ§Ã£o parcial da Tabela 3.
synth.tables.plac2w <- synth.tab(dataprep.res = dataprep.out.plac2w, synth.res = synth.out.plac2w)
synth.tables.plac2w$tab.w
synth.tables.plac2w$tab.pred

# ReplicaÃ§Ã£o da Figura 1
Y12w  <- dataprep.out.plac2w$Y1
Y1synth2w <- dataprep.out.plac2w$Y0 %*% synth.out.plac2w$solution.w
plot(1907:1927, Y12w, ylim=c(-10,10), type="l", xlab = "Year", ylab = "PTR Real GDP per capita Growth Rate")
lines(1907:1927, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_PLC2.png")
path.plot(synth.res = synth.out.plac2w, dataprep.res = dataprep.out.plac2w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-12, 22), Legend = c("Honduras",
                                        "Synthetic Honduras"), Legend.position = "topright")
dev.off()

dataprep.out.plac3w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", 'popg') ,
  predictors.op = c("mean"),
  time.predictors.prior = 1937:1947 ,
  special.predictors = list(
    list("averagegdppc", seq(1940, 1945,5), "mean"),
    list("primary_barro", seq(1940, 1945,5), "mean"),
    list("ggdp", 1937:1946, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 13,
  controls.identifier = c(5,16,26,28,32,38,40,49,50,54,81,90,99,101,104,119,122,137),
  time.optimize.ssr = 1937:1946,
  time.plot = 1937:1957)

# InspeÃ§Ã£o das matrizes X1 e Z1, ou seja, da matriz das caracterÃ?sitcas da observaÃ§Ã£o tratada e da 
# matriz das observaÃ§Ãµes da variÃ¡vel de resultado da tratada no perÃ?odo prÃ©-tratamento, repectivamente.
dataprep.out.plac3w$X1
dataprep.out.plac3w$Z1

# Correr a funÃ§Ã£o synth() (que optimiza a funÃ§Ã£o de perda encadeada para os pesos W e V)
synth.out.plac3w <- synth(dataprep.out.plac3w)

# ExtraÃ§Ã£o dos resultados da optimizaÃ§Ã£o feita pela funÃ§Ã£o synth(). ReplicaÃ§Ã£o parcial da Tabela 3.
synth.tables.plac3w <- synth.tab(dataprep.res = dataprep.out.plac3w, synth.res = synth.out.plac3w)
synth.tables.plac3w$tab.w
synth.tables.plac3w$tab.pred

# ReplicaÃ§Ã£o da Figura 1
Y13w  <- dataprep.out.plac3w$Y1
Y1synth3w <- dataprep.out.plac3w$Y0 %*% synth.out.plac3w$solution.w
plot(1907:1927, Y13w, ylim=c(-10,10), type="l", xlab = "Year", ylab = "PTR Real GDP per capita Growth Rate")
lines(1907:1927, Y1synth3w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_PLC3.png")
path.plot(synth.res = synth.out.plac3w, dataprep.res = dataprep.out.plac3w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "year",
          Ylim = c(-12, 12), Legend = c("Bolivia",
                                        "Synthetic Bolivia"), Legend.position = "bottomleft")
dev.off()

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_ALL.png")
plot(1937:1957, Y1w, ylim=c(-12,25), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1937:1957, Y1placw, type="l", lty=2, col="blue")
lines(1937:1957, Y12w, type="l", lty=2, col="blue4")
lines(1937:1957, Y13w, type="l", lty=2, col="blue3")
legend("topleft", legend = c("Sri Lanka", "El Salvador", "Honduras","Bolivia"),
       col = c("red", "blue", "blue4", "blue3"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(1947), col=c("black"), lwd=c(2))
dev.off()


###DEENSITY ####
effects<- as.data.frame(Y1w - Y1synthw)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1placw - Y1synthplacw)
effects['placebo2'] <- (Y12w - Y1synth2w)
effects['placebo3'] <- (Y13w - Y1synth3w)
#effects['placebo4'] <- (Y14w - Y1synth4w)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10
mean_p3 <- sum(effects[effects$years>11,'placebo3'])/10
#mean_p4 <- sum(effects[effects$years>11,'placebo4'])/10


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
#std[11,'placebo4'] <- sqrt(sum(na.omit(std$placebo4))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))
std[12,'placebo3'] <- sqrt(sum(std[std$years<=13,'placebo3'])/(13-11-1))
#std[12,'placebo4'] <- sqrt(sum(std[std$years<=13,'placebo4'])/(13-11-1))


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
#std[14,'placebo4'] <- sqrt(sum(std[std$years<=15,'placebo4'])/(15-11-1))


std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))
std[15,'placebo3'] <- sqrt(sum(std[std$years<=16,'placebo3'])/(16-11-1))
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
    n = 100000,
    mean = density[density$effect=='10year','ATE_placebo3'],
    sd = std[std$year=='std_10', 'placebo3']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Sri Lanka", "El Salvador", "Honduras", "Bolivia"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
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
    n = 100000,
    mean = density[density$effect=='5year','ATE_placebo2'],
    sd = std[std$year=='std_5', 'placebo2']
  )

norm_datap3 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='5year','ATE_placebo3'],
    sd = std[std$year=='std_5', 'placebo3']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.05))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Sri Lanka", "El Salvador", "Honduras", "Bolivia"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
dev.off()

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_country'],
    sd = std[std$year=='std_4', 'country']
  )

norm_datap1<-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo1'],
    sd = std[std$year=='std_4', 'placebo1']
  )

norm_datap2<-
  rnorm(
    n = 100000,
    mean = density[density$effect=='4year','ATE_placebo2'],
    sd = std[std$year=='std_4', 'placebo2']
  )

norm_datap3<-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo3'],
    sd = std[std$year=='std_4', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Sri Lanka", "El Salvador", "Honduras", "Bolivia"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.04), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
legend("topleft", legend = c("Sri Lanka", "El Salvador", "Honduras", "Bolivia"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/LKA_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.03), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
abline(v=c(0), col=c("black"), lwd=c(2))
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Sri Lanka", "El Salvador", "Honduras", "Bolivia"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
dev.off()


