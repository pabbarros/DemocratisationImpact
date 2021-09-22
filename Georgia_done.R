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
library(xtable)

ERT <- read.csv("ERT.csv")
dem <- read_excel("dem.xlsx")
ep_study <- read_excel("ep_study.xlsx")
colnames(dem)[2]<-"country_name"
db <- read_excel("db.xlsx")
db$year <- as.integer(db$year)
db1 <- read_excel("db3.xlsx")
db2 <- read_excel("db3wbt.xlsx")
db3 <- read_excel("db3wbtc.xlsx")
db4 <- read_excel("db3imft.xlsx")
db5 <- read_excel("db3imftc.xlsx")

countrycode <- (unique(db1[,"countrycode"]))
countrycode <- data.frame(countrycode)
countrycode[,"codenum"] <- 1:156
db1 <- merge(db1, countrycode, by="countrycode")
db1$year <- as.integer(db1$year)
db1$primary_barro <- as.integer(db1$primary_barro)
db1$secondary <- as.integer(db1$secondary)
db1$secondary_barro <- as.integer(db1$secondary_barro)
db1$gcf <- as.integer(db1$gcf)
db1$gcff <- as.integer(db1$gcff)
db1$trade_wb <- as.integer(db1$trade_wb)


db2 <- merge(db2, countrycode, by="countrycode")
db2$year <- as.integer(db2$year)
db2$secondary <- as.integer(db2$secondary)
db2$primary <- as.integer(db2$primary)
db2$gcf <- as.integer(db2$gcf)
db2$trade_wb <- as.integer(db2$trade_wb)

db3 <- merge(db3, countrycode, by="countrycode")
db3$year <- as.integer(db3$year)
db3$primary <- as.integer(db3$primary)
db3$secondary <- as.integer(db3$secondary)
db3$gcf <- as.integer(db3$gcf)

db4 <- merge(db4, countrycode, by="countrycode")
db4$year <- as.integer(db4$year)
db4$trade_wb <- as.integer(db4$trade_wb)
db4$primary <- as.integer(db4$primary)
db4$secondary <- as.integer(db4$secondary)

db5 <- merge(db5, countrycode, by="countrycode")
db5$year <- as.integer(db5$year)
db5$primary <- as.integer(db5$primary)
db5$secondary <- as.integer(db5$secondary)

##### CONTROLS #####
#Georgia
source("controls_w.R")
georgia <- controls(dem, ep_study, c="GEO")
georgia_ <- data.frame(Reduce(rbind, georgia))
georgia_ <- georgia_[!(is.na(georgia_$country_text_id)),]
georgia_c <- unique(georgia_$country_name)
georgia_in <- georgia_[georgia_$Continent_Name=="Asia" | georgia_$Continent_Name=="Oceania" ,]
georgia_in <- georgia_in[!(is.na(georgia_in$country_text_id)),]
georgia_in_c <- unique(georgia_in$country_name)
georgia_in_c <- c(georgia_in_c, "Georgia")
georgia_c <- c(georgia_c, "Georgia")

#####IN ASIA/OCEANIA ####
db <- db1[which(db1$country %in% georgia_in_c),]
db <- db[db$year>1993 & db$year<2015,]
db <- unique(db)
table <- as.data.frame(table(db$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)

table(db$codenum)
db <- db[which((db$codenum %in% c)),]
db[db$gcf==0,'gfc']<-NA


##SCM
dataprep.out <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 52,
  controls.identifier = c(1, 10, 23, 30, 66, 72, 75, 76, 78, 86, 106, 107, 
                          115, 122, 127, 137, 144, 147, 151, 153),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)


dataprep.out$X1
dataprep.out$Z1

synth.out <- synth(dataprep.out)

synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
synth.tables$tab.w
synth.tables$tab.pred

Y1  <- dataprep.out$Y1
Y1synth <- dataprep.out$Y0 %*% synth.out$solution.w
plot(1994:2014, Y1, ylim=c(-10,15), type="l", xlab = "Year", ylab = "Georgia Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_IN.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-10,13), Legend = c("Georgia",
                                     "Synthetic Georgia"), Legend.position = "bottomright")
abline(v=c(2004), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tables$tab.w)
xtable(synth.tables$tab.pred)

##Placebo 1
dataprep.out.plac <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 76,
  controls.identifier = c(1, 10, 23, 30, 66, 72, 75, 78, 86, 106, 107, 
                          115, 122, 127, 137, 144, 147, 151, 153),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)


dataprep.out.plac$X1
dataprep.out.plac$Z1

synth.out.plac <- synth(dataprep.out.plac)

synth.tables.plac <- synth.tab(dataprep.res = dataprep.out.plac, synth.res = synth.out.plac)
synth.tables.plac$tab.w
synth.tables.plac$tab.pred

Y1.plac  <- dataprep.out.plac$Y1
Y1synth.plac <- dataprep.out.plac$Y0 %*% synth.out.plac$solution.w
plot(1994:2014, Y1.plac, ylim=c(-10,15), type="l", xlab = "Year", ylab = "HUN Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth.plac, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_IN_PLC1.png")
path.plot(synth.res = synth.out.plac, dataprep.res = dataprep.out.plac,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-21,10), Legend = c("Kyrgyz Republic",
                                       "Synthetic Kyrgyz Republic"), Legend.position = "bottomright")
dev.off()


##Placebo 2
dataprep.out.plac2 <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 30,
  controls.identifier = c(1, 10, 23, 66, 72, 75, 76, 78, 86, 106, 107, 
                          115, 122, 127, 137, 144, 147, 151),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)


dataprep.out.plac2$X1
dataprep.out.plac2$Z1

synth.out.plac2 <- synth(dataprep.out.plac2)

synth.tables.plac2 <- synth.tab(dataprep.res = dataprep.out.plac2, synth.res = synth.out.plac2)
synth.tables.plac2$tab.w
synth.tables.plac2$tab.pred

Y1.plac2  <- dataprep.out.plac2$Y1
Y1synth.plac2 <- dataprep.out.plac2$Y0 %*% synth.out.plac2$solution.w
plot(1994:2014, Y1.plac2 , ylim=c(-10,15), type="l", xlab = "Year", ylab = "Georgia Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth.plac2, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_IN_PLC2.png")
path.plot(synth.res = synth.out.plac2, dataprep.res = dataprep.out.plac2,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-3,12), Legend = c("Cambodia",
                                       "Synthetic Cambodia"), Legend.position = "bottomright")
dev.off()

##Placebo 3
dataprep.out.plac3 <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 75,
  controls.identifier = c(1, 10, 23, 66, 72, 30, 76, 78, 86, 106, 107, 
                          115, 122, 127, 137, 144, 147, 151, 153),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)


dataprep.out.plac3$X1
dataprep.out.plac3$Z1

synth.out.plac3 <- synth(dataprep.out.plac3)

synth.tables.plac3 <- synth.tab(dataprep.res = dataprep.out.plac3, synth.res = synth.out.plac3)
synth.tables.plac3$tab.w
synth.tables.plac3$tab.pred

Y1.plac3  <- dataprep.out.plac3$Y1
Y1synth.plac3 <- dataprep.out.plac3$Y0 %*% synth.out.plac3$solution.w
plot(1994:2014, Y1.plac3 , ylim=c(-10,15), type="l", xlab = "Year", ylab = "Georgia Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth.plac3, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_IN_PLC3.png")
path.plot(synth.res = synth.out.plac3, dataprep.res = dataprep.out.plac3,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-8,23), Legend = c("Kuwait",
                                       "Synthetic Kuwait"), Legend.position = "bottomleft")
dev.off()


##ALL PLACEBOS
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_IN_ALL.png")
plot(1994:2014, Y1, ylim=c(-20,23), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1994:2014, Y1.plac, type="l", lty=2, col="blue")
lines(1994:2014, Y1.plac2, type="l", lty=2, col="blue4")
lines(1994:2014, Y1.plac3, type="l", lty=2, col="blue3")
legend("bottomright", legend = c("Georgia",'Kyrgyz', "Cambodia", "Kuwait"),
       col = c("red", "blue", "blue4", "blue3"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(2004), col=c("black"), lwd=c(2))
dev.off()

#####DENSITY####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1.plac - Y1synth.plac)
effects['placebo2'] <- (Y1.plac2 - Y1synth.plac2)
effects['placebo3'] <- (Y1.plac3 - Y1synth.plac3)

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
  std[i,'placebo3'] <- (effects[effects$year==std[i,'year'],'placebo3']-mean_p2)^2
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/GEO_IN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.13), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap3), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", 'Cambodia', 'Kuwait'),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2, 2, 2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 100000,
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/GEO_IN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.1), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='pink', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", 'Cambodia', 'Kuwait'),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2, 2, 2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/GEO_IN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.18), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", 'Cambodia', 'Kuwait'),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2, 2, 2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/GEO_IN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.20), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", 'Cambodia', 'Kuwait'),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2, 2, 2), cex = 0.8)
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

norm_datap2 <-
  rnorm(
    n = 10000000,
    mean = density[density$effect=='2year','ATE_placebo3'],
    sd = std[std$year=='std_2', 'placebo3']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/GEO_IN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.15), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", 'Cambodia', 'Kuwait'),
       col = c("red", "blue", 'green', 'pink'), lty = c(1,2, 2, 2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()


##### IN WORLD ####
dbw <- db1[which(db1$country %in% georgia_c),]
dbw <- dbw[dbw$year>1993 & dbw$year<2015,]
dbw <- unique(dbw)
table <- as.data.frame(table(dbw$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)
dbw <- dbw[which((dbw$codenum %in% c)),]

dbw[dbw$gcf==0,'gfc']<-NA

dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary" ) ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 52,
  controls.identifier = c(1, 3, 4, 10, 22, 23, 24, 27, 28, 30, 35, 40, 47, 
                          50, 51, 59, 66, 72, 74, 75, 76, 78, 81, 86, 89, 
                          94, 95, 106, 107, 115, 120, 122, 125, 127, 137, 144, 145, 
                          147, 151, 153, 156),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)

dataprep.outw$X1
dataprep.outw$Z1

synth.outw <- synth(dataprep.outw)

synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

Y1w  <- dataprep.outw$Y1
Y1synthw <- dataprep.outw$Y0 %*% synth.outw$solution.w
plot(1994:2014, Y1w, ylim=c(-10,15), type="l", xlab = "Year", ylab = "Georgia Real GDP per capita Growth Rate")
lines(1994:2014, Y1synthw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO.png")
path.plot(synth.res = synth.outw, dataprep.res = dataprep.outw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "year",
          Ylim = c(-10, 13), Legend = c("Georgia",
                                        "Synthetic Georgia"), Legend.position = "bottomright")
abline(v=c(2004), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)

##Placebo 1
dataprep.out.placw <-dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary" ) ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 76,
  controls.identifier = c(1, 3, 4, 10, 22, 23, 24, 27, 28, 30, 35, 40, 47, 
                          50, 51, 59, 66, 72, 74, 75, 78, 81, 86, 89, 
                          94, 95, 106, 107, 115, 120, 122, 125, 127, 137, 144, 145, 
                          147, 151, 153, 156),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)

dataprep.out.placw$X1
dataprep.out.placw$Z1

synth.out.placw <- synth(dataprep.out.placw)

synth.tables.placw <- synth.tab(dataprep.res = dataprep.out.placw, synth.res = synth.out.placw)
synth.tables.placw$tab.w
synth.tables.placw$tab.pred

Y1placw  <- dataprep.out.placw$Y1
Y1synthplacw <- dataprep.out.placw$Y0 %*% synth.out.placw$solution.w
plot(1994:2014, Y1placw, ylim=c(-40,15), type="l", xlab = "Year", ylab = "AGO Real GDP per capita Growth Rate")
lines( 1994:2014, Y1synthplacw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_PLC1.png")
path.plot(synth.res = synth.out.placw, dataprep.res = dataprep.out.placw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-21,11), Legend = c("Kyrgyz Republic",
                                       "Synthetic Kyrgyz Republic"), Legend.position = "bottomright")
dev.off()

##Placebo 2
dataprep.out.plac2w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary" ) ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2001:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 153,
  controls.identifier = c(1, 3, 4, 10, 22, 23, 24, 27, 28, 30, 35, 40, 47, 
                          50, 51, 59, 66, 72, 74, 75, 76, 78, 81, 86, 89, 
                          94, 95, 106, 107, 115, 120, 122, 125, 127, 137, 144, 145, 
                          147, 151, 156),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)

dataprep.out.plac2w$X1
dataprep.out.plac2w$Z1

synth.out.plac2w <- synth(dataprep.out.plac2w)

synth.tables.plac2w <- synth.tab(dataprep.res = dataprep.out.plac2w, synth.res = synth.out.plac2w)
synth.tables.plac2w$tab.w
synth.tables.plac2w$tab.pred

Y12w  <- dataprep.out.plac2w$Y1
Y1synth2w <- dataprep.out.plac2w$Y0 %*% synth.out.plac2w$solution.w
plot(1994:2014, Y12w, ylim=c(-10,15), type="l", xlab = "Year", ylab = "PTR Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_PLC2.png")
path.plot(synth.res = synth.out.plac2w, dataprep.res = dataprep.out.plac2w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(0, 9), Legend = c("Vietnam",
                                        "Synthetic Vietnam"), Legend.position = "bottomright")
dev.off()

##Placebo 3
dataprep.out.plac3w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary" ) ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2000:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 35,
  controls.identifier = c(1, 3, 4, 10, 22, 23, 24, 27, 28, 30, 40, 47, 
                          50, 51, 59, 66, 72, 74, 75, 76, 78, 81, 86, 89, 
                          94, 95, 106, 107, 115, 120, 122, 125, 127, 137, 144, 145, 
                          147, 151, 153, 156),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)


dataprep.out.plac3w$X1
dataprep.out.plac3w$Z1

synth.out.plac3w <- synth(dataprep.out.plac3w)

synth.tables.plac3w <- synth.tab(dataprep.res = dataprep.out.plac3w, synth.res = synth.out.plac3w)
synth.tables.plac3w$tab.w
synth.tables.plac3w$tab.pred

Y13w  <- dataprep.out.plac3w$Y1
Y1synth3w <- dataprep.out.plac3w$Y0 %*% synth.out.plac3w$solution.w
plot(1994:2014, Y13w, ylim=c(-20,80), type="l", xlab = "Year", ylab = "PTR Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth3w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_PLC3.png")
path.plot(synth.res = synth.out.plac3w, dataprep.res = dataprep.out.plac3w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-1, 15), Legend = c("Cuba",
                                        "Synthetic Cuba"), Legend.position = "bottomright")
dev.off()

##Placebo 4
dataprep.out.plac4w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade", "gcf", "secondary" ) ,
  predictors.op = c("mean"),
  time.predictors.prior = 1994:2004,
  special.predictors = list(
    list("averagegdppc", seq(1995, 2000,5), "mean"),
    list("ggdp", 2000:2004, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 75,
  controls.identifier = c(1, 3, 4, 10, 22, 23, 24, 27, 28, 30, 40, 47, 
                          50, 72, 74, 35, 76, 78, 81, 86, 89, 
                          94, 95, 106, 107, 115, 120, 122, 125, 127, 137, 144, 145, 
                          147, 151, 153, 156),
  time.optimize.ssr = 1994:2014,
  time.plot = 1994:2014)


dataprep.out.plac4w$X1
dataprep.out.plac4w$Z1

synth.out.plac4w <- synth(dataprep.out.plac4w)

synth.tables.plac4w <- synth.tab(dataprep.res = dataprep.out.plac4w, synth.res = synth.out.plac4w)
synth.tables.plac4w$tab.w
synth.tables.plac4w$tab.pred

Y14w  <- dataprep.out.plac4w$Y1
Y1synth4w <- dataprep.out.plac4w$Y0 %*% synth.out.plac4w$solution.w
plot(1994:2014, Y14w, ylim=c(-20,80), type="l", xlab = "Year", ylab = "PTR Real GDP per capita Growth Rate")
lines(1994:2014, Y1synth4w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_PLC4.png")
path.plot(synth.res = synth.out.plac4w, dataprep.res = dataprep.out.plac4w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-8, 24), Legend = c("Kuwait",
                                        "Synthetic Kuwait"), Legend.position = "bottomleft")
dev.off()

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_ALL.png")
plot(1994:2014, Y1w, ylim=c(-20,24), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red",lwd=c(2))
lines(1994:2014, Y1placw, type="l", lty=2, col="blue")
lines(1994:2014, Y12w, type="l", lty=2, col="blue4")
lines(1994:2014, Y13w, type="l", lty=2, col="blue3")
lines(1994:2014, Y14w, type="l", lty=2, col="blue2")
legend("bottomright", legend = c("Georgia", "Kyrgyz Republic", "Vietnam", "Cuba", "Kuwait"),
       col = c("red", "blue", "blue4", "blue3", "blue2"), lty = c(1,2,2,2,2), cex = 0.8)
abline(v=c(2004), col=c("black"), lwd=c(2))
dev.off()

#####DENSITY####
effects<- as.data.frame(Y1w - Y1synthw)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1placw - Y1synthplacw)
effects['placebo2'] <- (Y12w - Y1synth2w)
effects['placebo3'] <- (Y13w - Y1synth3w)
effects['placebo4'] <- (Y14w - Y1synthplacw)


std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10
mean_p3 <- sum(effects[effects$years>11,'placebo3'])/10
mean_p4 <- sum(effects[effects$years>11,'placebo4'])/10

for (i in 1:10){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
  std[i,'placebo3'] <- (effects[effects$year==std[i,'year'],'placebo3']-mean_p3)^2
  std[i,'placebo4'] <- (effects[effects$year==std[i,'year'],'placebo4']-mean_p3)^2
}

std[11,'year']<- 'std_10' 
std[11,'years']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))
std[11,'placebo3'] <- sqrt(sum(na.omit(std$placebo3))/(nrow(std)-2))
std[11,'placebo4'] <- sqrt(sum(na.omit(std$placebo4))/(nrow(std)-2))


std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))
std[12,'placebo3'] <- sqrt(sum(std[std$years<=13,'placebo3'])/(13-11-1))
std[12,'placebo4'] <- sqrt(sum(std[std$years<=13,'placebo4'])/(13-11-1))


std[13,'year']<- 'std_3' 
std[13,'years']<- 'std_3'
std[13,'country'] <- sqrt(sum(std[std$years<=14,'country'])/(14-11-1))
std[13,'placebo1'] <- sqrt(sum(std[std$years<=14,'placebo1'])/(14-11-1))
std[13,'placebo2'] <- sqrt(sum(std[std$years<=14,'placebo2'])/(14-11-1))
std[13,'placebo3'] <- sqrt(sum(std[std$years<=14,'placebo3'])/(14-11-1))
std[13,'placebo4'] <- sqrt(sum(std[std$years<=14,'placebo4'])/(14-11-1))


std[14,'year']<- 'std_4' 
std[14,'years']<- 'std_4'
std[14,'country'] <- sqrt(sum(std[std$years<=15,'country'])/(15-11-1))
std[14,'placebo1'] <- sqrt(sum(std[std$years<=15,'placebo1'])/(15-11-1))
std[14,'placebo2'] <- sqrt(sum(std[std$years<=15,'placebo2'])/(15-11-1))
std[14,'placebo3'] <- sqrt(sum(std[std$years<=15,'placebo3'])/(15-11-1))
std[14,'placebo4'] <- sqrt(sum(std[std$years<=15,'placebo4'])/(15-11-1))


std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))
std[15,'placebo3'] <- sqrt(sum(std[std$years<=16,'placebo3'])/(16-11-1))
std[15,'placebo4'] <- sqrt(sum(std[std$years<=16,'placebo4'])/(16-11-1))


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

density[density$effect=="1year", "ATE_placebo4"]<- effects[effects$years==12,"placebo4"]
density[density$effect=="2year", "ATE_placebo4"]<- sum(effects[effects$years==12 | effects$years==13,"placebo4"])/2
density[density$effect=="3year", "ATE_placebo4"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14,"placebo4"])/3
density[density$effect=="4year", "ATE_placebo4"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15,"placebo4"])/4
density[density$effect=="5year", "ATE_placebo4"]<- sum(effects[effects$years==12 | effects$years==13| effects$years==14 | effects$years==15 | effects$years==1911,"placebo4"])/5
density[density$effect=="10year", "ATE_placebo4"]<- sum(effects[effects$years>11,"placebo4"])/(21-11)


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

norm_datap4 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo4'],
    sd = std[std$year=='std_10', 'placebo4']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.11), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", "Vietnam", "Cuba", "Kuwait"),
       col = c("red", "blue", "green", 'pink', 'orange'), lty = c(1,2,2,2,2), cex = 0.8)
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
    n = 10000,
    mean = density[density$effect=='5year','ATE_placebo3'],
    sd = std[std$year=='std_5', 'placebo3']
  )

norm_datap4 <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_placebo4'],
    sd = std[std$year=='std_5', 'placebo4']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.09), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", "Vietnam", "Cuba", "Kuwait"),
       col = c("red", "blue", "green", 'pink', 'orange'), lty = c(1,2,2,2,2), cex = 0.8)
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

norm_datap4 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo4'],
    sd = std[std$year=='std_4', 'placebo4']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.09), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", "Vietnam", "Cuba", "Kuwait"),
       col = c("red", "blue", "green", 'pink', 'orange'), lty = c(1,2,2,2,2), cex = 0.8)
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

norm_datap4 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_placebo4'],
    sd = std[std$year=='std_3', 'placebo4']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.10), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", "Vietnam", "Cuba", "Kuwait"),
       col = c("red", "blue", "green", 'pink', 'orange'), lty = c(1,2,2,2,2), cex = 0.8)
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

norm_datap4 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_placebo4'],
    sd = std[std$year=='std_2', 'placebo4']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/GEO_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.11), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
lines(density(norm_datap3), col='pink', lty=2)
lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Georgia", "Kyrgyz Republic", "Vietnam", "Cuba", "Kuwait"),
       col = c("red", "blue", "green", 'pink', 'orange'), lty = c(1,2,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

