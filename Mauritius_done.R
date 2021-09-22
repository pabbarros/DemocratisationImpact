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
db1 <- read_excel("db2.xlsx")
db2 <- read_excel("db2t.xlsx")
db3 <- read_excel("db3wbtc.xlsx")
db4 <- read_excel("db3imft.xlsx")
db5 <- read_excel("db3imftc.xlsx")

countrycode <- (unique(db1[,"countrycode"]))
countrycode <- data.fraºme(countrycode)
countrycode[,"codenum"] <- 1:154
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
#mauritius
source("controls_w.R")
mauritius <- controls(dem, ep_study, c="MUS")
mauritius_ <- data.frame(Reduce(rbind, mauritius))
mauritius_ <- mauritius_[!(is.na(mauritius_$country_text_id)),]
mauritius_c <- unique(mauritius_$country_name)
mauritius_in <- mauritius_[mauritius_$Continent_Name=="Africa" ,]
mauritius_in <- mauritius_in[!(is.na(mauritius_in$country_text_id)),]
mauritius_in_c <- unique(mauritius_in$country_name)
mauritius_in_c <- c(mauritius_in_c, "Mauritius")
mauritius_c <- c(mauritius_c, "Mauritius")

#####IN AFRICA ####
db <- db1[which(db1$country %in% mauritius_in_c),]
db <- db[db$year>1957 & db$year<1979,]
table <- as.data.frame(table(db$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)

table(db$codenum)
db <- db[which((db$codenum %in% c)),]

##SCM
dataprep.out <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg", "trade") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1958:1968,
  special.predictors = list(
    list("averagegdppc", seq(1960, 1965,5), "mean"),
    list("ggdp", 1959:1968, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 90,
  controls.identifier = c(3, 4, 15, 21, 27, 28, 38, 43,  47, 50, 51, 
                          54, 57, 58, 74, 80, 81, 84, 85, 87, 89, 94, 95, 101, 
                          102, 119,  122, 125, 128, 136, 138, 140, 143, 153, 154),
  time.optimize.ssr = 1958:1968,
  time.plot = 1958:1978)


dataprep.out$X1
dataprep.out$Z1

synth.out <- synth(dataprep.out)

synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
synth.tables$tab.w
synth.tables$tab.pred

Y1  <- dataprep.out$Y1
Y1synth <- dataprep.out$Y0 %*% synth.out$solution.w
plot(1958:1978, Y1, ylim=c(-10,30), type="l", xlab = "Year", ylab = "Mauritius GDP per capita growth rate")
lines(1958:1978, Y1synth, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-28,33), Legend = c("Mauritius",
                                       "Synthetic Mauritius"), Legend.position = "topleft")
abline(v=c(1968), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tables$tab.w)
xtable(synth.tables$tab.pred)

##Placebo 1
dataprep.out.plac <- dataprep(
  foo = db,
  predictors = c("child_mort", "popg", "trade") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1958:1968,
  special.predictors = list(
    list("averagegdppc", seq(1960, 1965,5), "mean"),
    list("ggdp", 1959:1968, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 50,
  controls.identifier = c(3, 4, 15, 21, 27, 28, 38, 43,  47, 51, 
                          54, 57, 58, 74, 80, 81, 84, 85, 87, 89, 94, 95, 101, 
                          102, 119,  122, 125, 128, 136, 138, 140, 143, 153, 154),
  time.optimize.ssr = 1958:1968,
  time.plot = 1958:1978)


dataprep.out.plac$X1
dataprep.out.plac$Z1

synth.out.plac <- synth(dataprep.out.plac)

synth.tables.plac <- synth.tab(dataprep.res = dataprep.out.plac, synth.res = synth.out.plac)
synth.tables.plac$tab.w
synth.tables.plac$tab.pred

Y1.plac  <- dataprep.out.plac$Y1
Y1synth.plac <- dataprep.out.plac$Y0 %*% synth.out.plac$solution.w
plot(1958:1978, Y1.plac, ylim=c(-30,30), type="l", xlab = "Year", ylab = "HUN GDP per capita growth rate")
lines(1958:1978, Y1synth.plac, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_PLC1.png")
path.plot(synth.res = synth.out.plac, dataprep.res = dataprep.out.plac,
          Ylab = "Real GDP per capita Growth RateP", Xlab = "Year",
          Ylim = c(-33,33), Legend = c("Gabon",
                                       "Synthetic Gabon"), Legend.position = "bottomleft")
dev.off()



##ALL PLACEBOS

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_ALL.png")
plot(1958:1978, Y1, ylim=c(-30,35), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1958:1978, Y1.plac, type="l", lty=2, col="blue")
#lines(1958:1978, Y1.plac2, type="l", lty=2, col="blue4")
legend("bottomleft", legend = c("Mauritius", "Gabon"),
       col = c("red", "blue", "blue4"), lty = 1:2, cex = 0.8)
abline(v=c(1968), col=c("black"), lwd=c(2))
dev.off()


#####DENSITY####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1.plac - Y1synth.plac)
#effects['placebo2'] <- (Y1.plac2 - Y1synth.plac2)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10
#mean_p2 <- sum(effects[effects$years>11,'placebo2'])/10

for (i in 1:10){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  #std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
}

std[11,'year']<- 'std_10' 
std[11,'years']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
#std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'years']<- 'std_2'
std[12,'country'] <- sqrt(sum(std[std$years<=13,'country'])/(13-11-1))
std[12,'placebo1'] <- sqrt(sum(std[std$years<=13,'placebo1'])/(13-11-1))
#std[12,'placebo2'] <- sqrt(sum(std[std$years<=13,'placebo2'])/(13-11-1))

std[13,'year']<- 'std_3' 
std[13,'years']<- 'std_3'
std[13,'country'] <- sqrt(sum(std[std$years<=14,'country'])/(14-11-1))
std[13,'placebo1'] <- sqrt(sum(std[std$years<=14,'placebo1'])/(14-11-1))
#std[13,'placebo2'] <- sqrt(sum(std[std$years<=14,'placebo2'])/(14-11-1))

std[14,'year']<- 'std_4' 
std[14,'years']<- 'std_4'
std[14,'country'] <- sqrt(sum(std[std$years<=15,'country'])/(15-11-1))
std[14,'placebo1'] <- sqrt(sum(std[std$years<=15,'placebo1'])/(15-11-1))
#std[14,'placebo2'] <- sqrt(sum(std[std$years<=15,'placebo2'])/(15-11-1))

std[15,'year']<- 'std_5' 
std[15,'years']<- 'std_5'
std[15,'country'] <- sqrt(sum(std[std$years<=16,'country'])/(16-11-1))
std[15,'placebo1'] <- sqrt(sum(std[std$years<=16,'placebo1'])/(16-11-1))
#std[15,'placebo2'] <- sqrt(sum(std[std$years<=16,'placebo2'])/(16-11-1))

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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-50,50), ylim=c(0, 0.03), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap3), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon"),
       col = c("red", "blue"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='pink', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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



png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.10), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.07), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_IN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-25,25), ylim=c(0, 0.06), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

##### IN WORLD ####
dbw <- db1[which(db1$country %in% mauritius_c),]
dbw <- dbw[dbw$year>1957 & dbw$year<1979,]
table <- as.data.frame(table(dbw$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)
dbw <- dbw[which((dbw$codenum %in% c)),]


dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1958:1968,
  special.predictors = list(
    list("averagegdppc", seq(1960, 1965,5), "mean"),
    list("ggdp", 1965:1968, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 90,
  controls.identifier = c(1, 2, 3, 4, 15, 16, 19, 20, 21, 23, 27, 28, 30, 
                          31, 35, 38, 41, 42, 43, 44, 47, 50, 51, 54, 56, 57, 
                          58, 59, 60, 62, 65, 66, 72, 74, 75, 78, 80, 81, 84, 85, 
                          86, 87, 89, 91, 92, 94, 95, 100, 101, 102, 106, 
                          108, 109, 110, 111, 112, 116, 119, 121, 122,
                          125, 126, 128, 129, 136, 137, 138, 140, 143, 152, 153, 154),
  time.optimize.ssr = 1958:1968,
  time.plot = 1958:1978)

dataprep.outw$X1
dataprep.outw$Z1

synth.outw <- synth(dataprep.outw)

synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

Y1w  <- dataprep.outw$Y1
Y1synthw <- dataprep.outw$Y0 %*% synth.outw$solution.w
plot(1958:1978, Y1w, ylim=c(-10,15), type="l", xlab = "Year", ylab = "Mauritius GDP per capita growth rate")
lines(1958:1978, Y1synthw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS.png")
path.plot(synth.res = synth.outw, dataprep.res = dataprep.outw,
          Ylab = "Real GDP per capita Growth RateP", Xlab = "year",
          Ylim = c(-17, 27), Legend = c("Mauritius",
                                        "Synthetic Mauritius"), Legend.position = "bottomleft")
abline(v=c(1968), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)

##Placebo 1
dataprep.out.placw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1958:1968,
  special.predictors = list(
    list("averagegdppc", seq(1960, 1965,5), "mean"),
    list("ggdp", 1965:1968, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 50,
  controls.identifier = c(1, 2, 3, 4, 15, 16, 19, 20, 21, 23, 28, 30, 
                          31, 35, 38, 41, 42, 43, 44, 47, 27, 51, 54, 56, 57, 
                          58, 59, 60, 62, 65, 66, 72, 74, 75, 78, 80, 81, 84, 85, 
                          86, 87, 89, 91, 92, 94, 95, 100, 101, 102, 106, 
                          108, 109, 111, 112, 116, 119, 121, 122,
                          125, 126, 128, 129, 136, 137, 138, 140, 143, 152, 153, 154),
  time.optimize.ssr = 1958:1968,
  time.plot = 1958:1978)


dataprep.out.placw$X1
dataprep.out.placw$Z1

synth.out.placw <- synth(dataprep.out.placw)

synth.tables.placw <- synth.tab(dataprep.res = dataprep.out.placw, synth.res = synth.out.placw)
synth.tables.placw$tab.w
synth.tables.placw$tab.pred

Y1placw  <- dataprep.out.placw$Y1
Y1synthplacw <- dataprep.out.placw$Y0 %*% synth.out.placw$solution.w
plot(1958:1978, Y1placw, ylim=c(-5,10), type="l", xlab = "Year", ylab = "AGO GDP per capita growth rate")
lines( 1958:1978, Y1synthplacw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_PLC1.png")
path.plot(synth.res = synth.out.placw, dataprep.res = dataprep.out.placw,
          Ylab = "Real GDP per capita Growth RateP", Xlab = "Year",
          Ylim = c(-30,33), Legend = c("Gabon",
                                       "Synthetic Gabon"), Legend.position = "topleft")
dev.off()

##Placebo 2
dataprep.out.plac2w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "trade") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1958:1968,
  special.predictors = list(
    list("averagegdppc", seq(1960, 1965,5), "mean"),
    list("ggdp", 1965:1968, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 72,
  controls.identifier = c(1, 2, 3, 4, 15, 16, 19, 20, 21, 23, 27, 30, 
                          31, 35, 38, 41, 42, 43, 44, 47, 50, 51, 54, 56, 57, 
                          58, 60, 62, 65, 66, 28, 74, 75, 78, 80, 81, 84, 85, 
                          86, 87, 89, 91, 92, 94, 95, 100, 101, 102, 106, 
                          108, 109, 110, 111, 112, 116, 119, 121, 122,
                          125, 126, 128, 129, 136, 137, 138, 140, 143, 152, 153, 154),
  time.optimize.ssr = 1958:1968,
  time.plot = 1958:1978)

dataprep.out.plac2w$X1
dataprep.out.plac2w$Z1

synth.out.plac2w <- synth(dataprep.out.plac2w)

synth.tables.plac2w <- synth.tab(dataprep.res = dataprep.out.plac2w, synth.res = synth.out.plac2w)
synth.tables.plac2w$tab.w
synth.tables.plac2w$tab.pred

Y12w  <- dataprep.out.plac2w$Y1
Y1synth2w <- dataprep.out.plac2w$Y0 %*% synth.out.plac2w$solution.w
plot(1958:1978, Y12w, ylim=c(-10,15), type="l", xlab = "Year", ylab = "PTR GDP per capita growth rate")
lines(1958:1978, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_PLC2.png")
path.plot(synth.res = synth.out.plac2w, dataprep.res = dataprep.out.plac2w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-15, 25), Legend = c("Jordan",
                                        "Synthetic Jordan"), Legend.position = "topleft")
dev.off()



png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_ALL.png")
plot(1958:1978, Y1w, ylim=c(-27,30), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1958:1978, Y1placw, type="l", lty=2, col="blue")
lines(1958:1978, Y12w, type="l", lty=2, col="blue4")
#lines(1958:1978, Y13w, type="l", lty=2, col="blue3")
abline(v=c(1968), col=c("black"), lwd=c(2))
legend("bottomleft", legend = c("Mauritius", "Gabon", "Jordan"),
       col = c("red", "blue", "blue4", "blue3"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(1968), col=c("black"), lwd=c(2))
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-50,50), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon", "Jordan"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='5year','ATE_country'],
    sd = std[std$year=='std_5', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='5year','ATE_placebo1'],
    sd = std[std$year=='std_5', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 100000,
    mean = density[density$effect=='5year','ATE_placebo2'],
    sd = std[std$year=='std_5', 'placebo2']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.1), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon", "Jordan"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
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
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo2'],
    sd = std[std$year=='std_4', 'placebo2']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.15), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon", "Jordan"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.13), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon", "Jordan"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/MUS_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.1), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Mauritius", "Gabon", "Jordan"),
       col = c("red", "blue", "green", "pink"), lty = c(1,2,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()


