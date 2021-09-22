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
#Czech Republic
ep_study[ep_study['country_name']=='Czech Republic2', 'country_text_id'] <- 'CZE2'
source("controls_w.R")
czechrep <- controls(dem, ep_study, c="CZE2")
czechrep_ <- data.frame(Reduce(rbind, czechrep))
czechrep_ <- czechrep_[!(is.na(czechrep_$country_text_id)),]
czechrep_c <- unique(czechrep_$country_name)
czechrep_in <- czechrep_[czechrep_$Continent_Name=="Europe",]
czechrep_in <- czechrep_in[!(is.na(czechrep_in$country_text_id)),]
czechrep_in_c <- unique(czechrep_in$country_name)
czechrep_in_c <- c(czechrep_in_c, "Czech Republic")
czechrep_c <- c(czechrep_c, "Czech Republic")

#####IN EUROPE ####
dbfinal <- db1[which(db1$country %in% czechrep_in_c),]
dbfinal <- dbfinal[dbfinal$year>1979 & dbfinal$year<2001,]
table <- as.data.frame(table(dbfinal$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)

table(dbfinal$codenum)
dbfinal <- dbfinal[which((dbfinal$codenum %in% c)),]

##SCM
dataprep.out <- dataprep(
  foo = dbfinal,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1980:1990 ,
  special.predictors = list(
    list("averagegdppc", seq(1980, 1990,5), "mean"),
    list("ggdp", 1987:1990, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 37,
  controls.identifier =c(2, 124),
  time.optimize.ssr = 1980:1990,
  time.plot = 1980:2000)


dataprep.out$X1
dataprep.out$Z1

synth.out <- synth(dataprep.out)

synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
synth.tables$tab.w
synth.tables$tab.pred

Y1  <- dataprep.out$Y1
Y1synth <- dataprep.out$Y0 %*% synth.out$solution.w
plot(1980:2000, Y1, ylim=c(-25,15), type="l", xlab = "Year", ylab = "Czech Republic GDP per capita growth rate")
lines(1980:2000, Y1synth, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-30,15), Legend = c("Czech Republic",
                                       "Synthetic Czech Republic"), Legend.position = "bottomright")
abline(v=c(1990), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tables$tab.w)
xtable(synth.tables$tab.pred)


##Placebo 1
dataprep.out.plac <- dataprep(
  foo = dbfinal,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1980:1990 ,
  special.predictors = list(
    list("averagegdppc", seq(1980, 1990,5), "mean"),
    list("ggdp", 1987:1990, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 2,
  controls.identifier =c(37, 124),
  time.optimize.ssr = 1980:1990,
  time.plot = 1980:2000)


dataprep.out.plac$X1
dataprep.out.plac$Z1

synth.out.plac <- synth(dataprep.out.plac)

synth.tables.plac <- synth.tab(dataprep.res = dataprep.out.plac, synth.res = synth.out.plac)
synth.tables.plac$tab.w
synth.tables.plac$tab.pred

Y1.plac  <- dataprep.out.plac$Y1
Y1synth.plac <- dataprep.out.plac$Y0 %*% synth.out.plac$solution.w
plot(1980:2000, Y1.plac, ylim=c(-25,15), type="l", xlab = "Year", ylab = "HUN GDP per capita growth rate")
lines(1980:2000, Y1synth.plac, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_PLC1.png")
path.plot(synth.res = synth.out.plac, dataprep.res = dataprep.out.plac,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-30,15), Legend = c("Albania",
                                       "Synthetic Albania"), Legend.position = "bottomright")
dev.off()

##ALL PLACEBOS

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_ALL.png")
plot(1980:2000, Y1, ylim=c(-28,15), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1980:2000, Y1.plac, type="l", lty=2, col="blue")
#lines(1979:1999, Y1.plac2, type="l", lty=2, col="blue4")
legend("bottomleft", legend = c("Czech Republic", "Albania"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8)
abline(v=c(1990), col=c("black"), lwd=c(2))
dev.off()

#####DENSITY####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1.plac - Y1synth.plac)
#effects['placebo2'] <- (Y12 - Y1synth2)

std <- as.data.frame(effects[effects$years>11, "year"])
colnames(std) <- "year"
std['years']<- 12:21

mean_country <- sum(effects[effects$years>11,'country'])/10
mean_p1 <- sum(effects[effects$years>11,'placebo1'])/10

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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-80,80), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap3), col='orange', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania"),
       col = c("red", "blue"), lty = c(1,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-60,60), ylim=c(0, 0.04), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='pink', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania"),
       col = c("red", "blue"), lty = c(1,2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-60,60), ylim=c(0, 0.04), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania"),
       col = c("red", "blue", "green"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-50,50), ylim=c(0, 0.04), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania"),
       col = c("red", "blue"), lty = c(1,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/CZE_IN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-70,70), ylim=c(0, 0.03), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
#lines(density(norm_datap2), col='green', lty=2)
#lines(density(norm_datap3), col='pink', lty=2)
#lines(density(norm_datap4), col='orange', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania"),
       col = c("red", "blue"), lty = c(1,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()




##### IN WORLD ####
dbw <- db1[which(db1$country %in% czechrep_c),]
dbw <- dbw[dbw$year>1979 & dbw$year<2001,]
table <- as.data.frame(table(dbw$codenum))
table <- table[table$Freq==21,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)
dbw <- dbw[which((dbw$codenum %in% c)),]


dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1980:1990 ,
  special.predictors = list(
    list("averagegdppc", seq(1980, 1990,5), "mean"),
    list("ggdp", 1987:1990, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 37,
  controls.identifier = c(1, 2, 3, 4, 10, 22, 23, 24, 27, 28, 30, 32, 35, 40, 
                          47, 50, 51, 59, 66, 72, 74, 75, 78, 79, 80, 
                          81, 85, 86, 89, 94, 95, 97, 102, 106, 107, 115, 120, 122, 
                          125, 126, 127, 140, 142, 145, 153, 156),
  time.optimize.ssr = 1980:1990,
  time.plot = 1980:2000)

dataprep.outw$X1
dataprep.outw$Z1

synth.outw <- synth(dataprep.outw)

synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

Y1w  <- dataprep.outw$Y1
Y1synthw <- dataprep.outw$Y0 %*% synth.outw$solution.w

plot(1980:2000, Y1w, ylim=c(-20,15), type="l", xlab = "Year", ylab = "Finland GDP per capita growth rate")
lines(1980:2000, Y1synthw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE.png")
path.plot(synth.res = synth.outw, dataprep.res = dataprep.outw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-22, 15), Legend = c("Czech Republic",
                                        "Synthetic Czech Republic"), Legend.position = "bottomleft")
abline(v=c(1990), col=c("black"), lwd=c(3))
dev.off()


xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)

##Placebo 1
dataprep.out.placw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1980:1990 ,
  special.predictors = list(
    list("averagegdppc", seq(1980, 1990,5), "mean"),
    list("ggdp", 1987:1990, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 2,
  controls.identifier = c(1, 3, 4, 10, 22, 23, 24, 27, 28, 30, 32, 35, 40, 
                          47, 51, 59, 66, 72, 74, 75, 78, 79, 80, 
                          81, 85, 86, 89, 94, 95, 97, 102, 106, 107, 115, 120, 122, 50, 
                          125, 126, 127, 140, 142, 145, 153, 156),
  time.optimize.ssr = 1980:1990,
  time.plot = 1980:2000)


dataprep.out.placw$X1
dataprep.out.placw$Z1

synth.out.placw <- synth(dataprep.out.placw)

synth.tables.placw <- synth.tab(dataprep.res = dataprep.out.placw, synth.res = synth.out.placw)
synth.tables.placw$tab.w
synth.tables.placw$tab.pred

Y1placw  <- dataprep.out.placw$Y1
Y1synthplacw <- dataprep.out.placw$Y0 %*% synth.out.placw$solution.w
plot(1980:2000, Y1placw, ylim=c(-25,15), type="l", xlab = "Year", ylab = "AGO GDP per capita growth rate")
lines(1980:2000, Y1synthplacw, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_PLC1.png")
path.plot(synth.res = synth.out.placw, dataprep.res = dataprep.out.placw,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-30,20), Legend = c("Albania",
                                       "Synthetic Albania"), Legend.position = "bottomright")
dev.off()

##Placebo 2
dataprep.out.plac2w <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg", "secondary") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1980:1990 ,
  special.predictors = list(
    list("averagegdppc", seq(1980, 1990,5), "mean"),
    list("ggdp", 1987:1990, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 127,
  controls.identifier = c(1, 2, 3, 4, 10, 22, 23, 24, 27, 28, 30, 32, 35, 40, 
                          47, 50, 51, 59, 66, 72, 74,75, 78, 79, 80, 
                          81, 85, 86, 89, 94, 95, 97, 102, 106, 107, 115, 120, 122, 124, 
                          125, 126, 140, 142, 145, 153, 156),
  time.optimize.ssr = 1980:1990,
  time.plot = 1980:2000)


dataprep.out.plac2w$X1
dataprep.out.plac2w$Z1

synth.out.plac2w <- synth(dataprep.out.plac2w)

synth.tables.plac2w <- synth.tab(dataprep.res = dataprep.out.plac2w, synth.res = synth.out.plac2w)
synth.tables.plac2w$tab.w
synth.tables.plac2w$tab.pred

Y12w  <- dataprep.out.plac2w$Y1
Y1synth2w <- dataprep.out.plac2w$Y0 %*% synth.out.plac2w$solution.w
plot(1980:2000, Y12w, ylim=c(-15,20), type="l", xlab = "Year", ylab = "PTR GDP per capita growth rate")
lines(1980:2000, Y1synth2w, type="l", lty=2)

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_PLC2.png")
path.plot(synth.res = synth.out.plac2w, dataprep.res = dataprep.out.plac2w,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-5, 10), Legend = c("Singapore",
                                        "Synthetic Singapore"), Legend.position = "bottomleft")
dev.off()


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_ALL.png")
plot(1980:2000, Y1w, ylim=c(-30,20), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
lines(1980:2000, Y1placw, type="l", lty=2, col="blue")
lines(1980:2000, Y12w, type="l", lty=2, col="blue4")
legend("topleft", legend = c("Czech Republic", "Albania", "Singapore"),
       col = c("red", "blue", "blue4", "blue3"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(1990), col=c("black"), lwd=c(2))
dev.off()

#####DENSITY####
effects<- as.data.frame(Y1w - Y1synthw)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects[,'years']<-1:21
effects['placebo1'] <- (Y1placw - Y1synthplacw)
effects['placebo2'] <- (Y12w - Y1synth2w)

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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania", "Singapore"),
       col = c("red", "blue", "green"), lty = c(1,2,2,2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania", "Singapore"),
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania", "Singapore"),
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.08), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania", "Singapore"),
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/CZE_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-40,40), ylim=c(0, 0.05), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Czech Republic", "Albania", "Singapore"),
       col = c("red", "blue", "green", 'pink'), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

