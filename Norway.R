rm(list = ls())
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
db1 <- read_excel("db1.xlsx")
countrycode <- (unique(db1[,"countrycode"]))
countrycode <- data.frame(countrycode)
countrycode[,"codenum"] <- 1:141
db1 <- merge(db1, countrycode, by="countrycode")
db1$year <- as.integer(db1$year)
db1$primary_barro <- as.integer(db1$primary_barro)
db1$secondary_barro <- as.integer(db1$secondary_barro)


source("controls_nor.R")
norway <- controls_nor(dem, ep_study, c="Norway")
norway_ <- data.frame(Reduce(rbind, norway))
norway_ <- norway_[!(is.na(norway_$country_text_id)),]
norway_c <- unique(norway_$country_name)
norway_in <- norway_[norway_$Continent_Name=="Europe",]
norway_in <- norway_in[!(is.na(norway_in$country_text_id)),]
norway_in_c <- unique(norway_in$country_name)

norway_in_c <- c(norway_in_c, "Norway")
norway_c <- c(norway_c, "Norway")

nor <- db1[which(db1$country %in% norway_in_c),]
nor <- nor[nor$year>1894 & nor$year<1921,]
table <- as.data.frame(table(nor$codenum))
table <- table[table$Freq==26,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)

nor <- nor[which((nor$codenum %in% c)),]

dataprep.out <- dataprep(
  foo = nor,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1900:1906 ,
  special.predictors = list(
    list("averagegdppc", seq(1895,1905,5), "mean"),
    list("primary_barro", seq(1895,1905,5), "mean"),
    list("ggdp", 1903:1906, "mean")
    #,list("trade", 1905:1906, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 95,
  controls.identifier = c(7, 47, 49, 63, 88, 104, 122, 124, 134),
  time.optimize.ssr = 1900:1906,
  time.plot = 1900:1916)

synth.out <- synth(dataprep.out)

synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
synth.tables$tab.w

xtable(synth.tables$tab.w)

synth.tables$tab.pred
xtable(synth.tables$tab.pred)

#Gráfico do Controlo da Noruega
Y1  <- dataprep.out$Y1
Y1synth <- dataprep.out$Y0 %*% synth.out$solution.w

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_IN.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-3, 10), Legend = c("Norway",
                                        "Synthetic Norway"), Legend.position = "topleft")
abline(v=c(1906), col=c("black"), lwd=c(2))
dev.off()


#Placebos in-Place: 

#Switzerland
dataprep.out2 <- dataprep(
  foo = nor,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1900:1906 ,
  special.predictors = list(
    list("averagegdppc", seq(1895,1905,5), "mean"),
    list("primary_barro", seq(1895,1905,5), "mean"),
    list("ggdp", 1903:1906, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 124,
  controls.identifier = c(7, 47, 49, 63, 88, 104, 122, 134),
  time.optimize.ssr = 1900:1906,
  time.plot = 1900:1916)

# Correr a função synth()
synth.out2 <- synth(data.prep.obj = dataprep.out2, method = "BFGS")

# Extração dos resultados da optimização feita pela função synth(). Replicação parcial da Tabela 3.
synth.tables2 <- synth.tab(dataprep.res = dataprep.out2, synth.res = synth.out2)
synth.tables2$tab
synth.tables2$tab.pred

# Replicação da Figura 4
Y1placebo <- dataprep.out2$Y1
Y1synthplacebo <- dataprep.out2$Y0 %*% synth.out2$solution.w
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_IN_PLC1.png")
path.plot(synth.res = synth.out2, dataprep.res = dataprep.out2,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-5, 10), Legend = c("Sweden",
                                       "Synthetic Sweden"), Legend.position = "bottomleft")
dev.off()


#2- Netherlands
dataprep.out3 <- dataprep(
  foo = nor,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1900:1906 ,
  special.predictors = list(
    list("averagegdppc", seq(1895,1905,5), "mean"),
    list("primary_barro", seq(1895,1905,5), "mean"),
    list("ggdp", 1903:1906, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 88,
  controls.identifier = c(7, 47, 49, 63, 104, 122, 124, 134),
  time.optimize.ssr = 1900:1906,
  time.plot = 1900:1916)

# Correr a função synth()
synth.out3 <- synth(data.prep.obj = dataprep.out3, method = "BFGS")

# Extração dos resultados da optimização feita pela função synth(). Replicação parcial da Tabela 3.
synth.tables3 <- synth.tab(dataprep.res = dataprep.out3, synth.res = synth.out3)
synth.tables3$tab
synth.tables3$tab.pred

# Replicação da Figura 4
Y1placebo2 <- dataprep.out3$Y1
Y1synthplacebo2 <- dataprep.out3$Y0 %*% synth.out3$solution.w
{plot(1900:1916, Y1placebo2, ylim=c(-15,15), type="l", xlab = "Year", ylab = "Netherlands GDP per capita growth rate")
  lines(1900:1916, Y1synthplacebo2, type="l", lty=2)}

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_IN_PLC2.png")
path.plot(synth.res = synth.out3, dataprep.res = dataprep.out3,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-5, 6), Legend = c("The Netherlands",
                                        "Synthetic Netherlands"), Legend.position = "bottomleft")
dev.off()


#PLACEBO - final:
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_IN_ALL.png")
{plot(1900:1916, Y1, ylim=c(-5,10), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
  lines(1900:1916, Y1placebo, type="l", lty=2, col="blue")
  lines(1900:1916, Y1placebo2, type="l", lty=2, col="blue4")
  legend("bottomleft", legend = c("Norway", "Sweden", "The Netherlands"),
         col = c("red", "blue", "blue4"), lty = c(1,2,2), cex = 0.8)
  abline(v=c(1906), col=c("black"), lwd=c(2))}
dev.off()


#### DENSITY #####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects['placebo1'] <- (Y1placebo - Y1synthplacebo)
effects['placebo2'] <- (Y1placebo2 - Y1synthplacebo2)

std <- as.data.frame(effects[effects$year>1906, "year"])
colnames(std) <- "year"
mean_country <- sum(effects[effects$year>1906,'country'])/10
mean_p1 <- sum(effects[effects$year>1906,'placebo1'])/10
mean_p2 <- sum(effects[effects$year>1906,'placebo2'])/10

for (i in seq(1,10)){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
}

std[11,'year']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'country'] <- sqrt(sum(std[std$year<=1908,'country'])/(1908-1906-1))
std[12,'placebo1'] <- sqrt(sum(std[std$year<=1908,'placebo1'])/(1908-1906-1))
std[12,'placebo2'] <- sqrt(sum(std[std$year<=1908,'placebo2'])/(1908-1906-1))

std[13,'year']<- 'std_3' 
std[13,'country'] <- sqrt(sum(std[std$year<=1909,'country'])/(1909-1906-1))
std[13,'placebo1'] <- sqrt(sum(std[std$year<=1909,'placebo1'])/(1909-1906-1))
std[13,'placebo2'] <- sqrt(sum(std[std$year<=1909,'placebo2'])/(1909-1906-1))


std[14,'year']<- 'std_4' 
std[14,'country'] <- sqrt(sum(std[std$year<=1910,'country'])/(1910-1906-1))
std[14,'placebo1'] <- sqrt(sum(std[std$year<=1910,'placebo1'])/(1910-1906-1))
std[14,'placebo2'] <- sqrt(sum(std[std$year<=1910,'placebo2'])/(1910-1906-1))


std[15,'year']<- 'std_5' 
std[15,'country'] <- sqrt(sum(std[std$year<=1911,'country'])/(1911-1906-1))
std[15,'placebo1'] <- sqrt(sum(std[std$year<=1911,'placebo1'])/(1911-1906-1))
std[15,'placebo2'] <- sqrt(sum(std[std$year<=1911,'placebo2'])/(1911-1906-1))

density <- as.data.frame(c("1year", "2year", "3year", "4year", "5year", "10year"))
colnames(density) <- "effect"
density[density$effect=="1year", "ATE_country"]<- effects[effects$year==1907,"country"]
density[density$effect=="2year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908,"country"])/2
density[density$effect=="3year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909,"country"])/3
density[density$effect=="4year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910,"country"])/4
density[density$effect=="5year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910 | effects$year==1911,"country"])/5
density[density$effect=="10year", "ATE_country"]<- sum(effects[effects$year>1906,"country"])/(1916-1906)

density[density$effect=="1year", "ATE_placebo1"]<- effects[effects$year==1907,"placebo1"]
density[density$effect=="2year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908,"placebo1"])/2
density[density$effect=="3year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909,"placebo1"])/3
density[density$effect=="4year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910,"placebo1"])/4
density[density$effect=="5year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910 | effects$year==1911,"placebo1"])/5
density[density$effect=="10year", "ATE_placebo1"]<- sum(effects[effects$year>1906,"placebo1"])/(1916-1906)

density[density$effect=="1year", "ATE_placebo2"]<- effects[effects$year==1907,"placebo2"]
density[density$effect=="2year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908,"placebo2"])/2
density[density$effect=="3year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909,"placebo2"])/3
density[density$effect=="4year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910,"placebo2"])/4
density[density$effect=="5year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910 | effects$year==1911,"placebo2"])/5
density[density$effect=="10year", "ATE_placebo2"]<- sum(effects[effects$year>1906,"placebo2"])/(1916-1906)

norm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_country'],
    sd = std[std$year=='std_10', 'country']
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_IN_DEN_10Y.png")
plot(density(norm_data),main = '10-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.2), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

norm_data <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_country'],
    sd = std[std$year=='std_5', 'country']
  )

norm_datap1 <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_placebo1'],
    sd = std[std$year=='std_5', 'placebo1']
  )

norm_datap2 <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_placebo2'],
    sd = std[std$year=='std_5', 'placebo2']
  )

 
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_IN_DEN_5Y.png")
plot(density(norm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.2), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_IN_DEN_4Y.png")
plot(density(norm_data),main = '4-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.2), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
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


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_IN_DEN_3Y.png")
plot(density(norm_data),main = '3-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.2), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
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

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_IN_DEN_2Y.png")
plot(density(norm_data),main = '2-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.15), lwd=c(2))
lines(density(norm_datap1), col='blue', lty=2)
lines(density(norm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()




##### IN WORLD ####
dbw <- db1[which(db1$country %in% norway_c),]
dbw <- dbw[dbw$year>1894 & dbw$year<1917,]
table <- as.data.frame(table(dbw$codenum))
table <- table[table$Freq>=22,]
table$Var1 <- as.factor(table$Var1)
c <- as.vector(table$Var1)
c <- as.numeric(c)
dbw <- dbw[which((dbw$codenum %in% c)),]


dataprep.outw <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1900:1906 ,
  special.predictors = list(
    list("averagegdppc", seq(1895,1905,5), "mean"),
    list("primary_barro", seq(1895,1905,5), "mean"),
    list("ggdp", 1903:1906, "mean")
    #,list("trade", 1905:1906, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 95,
  controls.identifier = c(5, 7, 13, 16, 22, 26, 28, 43, 47, 49, 58, 59, 63, 65, 81, 88, 
                          101, 104, 122, 123, 124, 135, 136, 137),
  time.optimize.ssr = 1900:1906,
  time.plot = 1900:1916)

dataprep.outw$X1
dataprep.outw$Z1

synth.outw <- synth(dataprep.outw)

synth.tablesw <- synth.tab(dataprep.res = dataprep.outw, synth.res = synth.outw)
synth.tablesw$tab.w
synth.tablesw$tab.pred

Y1 <- dataprep.outw$Y1
Y1synth <- dataprep.outw$Y0 %*% synth.outw$solution.w

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR.png")
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-3, 10), Legend = c("Norway",
                                       "Synthetic Norway"), Legend.position = "topleft")
abline(v=c(1906), col=c("black"), lwd=c(2))
dev.off()

xtable(synth.tablesw$tab.w)
xtable(synth.tablesw$tab.pred)


#Placebos in-Place: 

#Switzerland
dataprep.out2 <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1900:1906 ,
  special.predictors = list(
    list("averagegdppc", seq(1895,1905,5), "mean"),
    list("primary_barro", seq(1895,1905,5), "mean"),
    list("ggdp", 1903:1906, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 124,
  controls.identifier = c(5, 7, 13, 16, 22, 26, 28, 43, 47, 49, 58, 59, 63, 65, 81, 88, 
                          101, 104, 122, 123, 135, 136, 137),
  time.optimize.ssr = 1900:1906,
  time.plot = 1900:1916)

# Correr a função synth()
synth.out2 <- synth(data.prep.obj = dataprep.out2, method = "BFGS")

# Extração dos resultados da optimização feita pela função synth(). Replicação parcial da Tabela 3.
synth.tables2 <- synth.tab(dataprep.res = dataprep.out2, synth.res = synth.out2)
synth.tables2$tab
synth.tables2$tab.pred

# Replicação da Figura 4
Y1placebo <- dataprep.out2$Y1
Y1synthplacebo <- dataprep.out2$Y0 %*% synth.out2$solution.w
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_PLC1.png")
path.plot(synth.res = synth.out2, dataprep.res = dataprep.out2,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-5, 10), Legend = c("Sweden",
                                       "Synthetic Sweden"), Legend.position = "bottomleft")
dev.off()


#2- Netherlands
dataprep.out3 <- dataprep(
  foo = dbw,
  predictors = c("child_mort", "popg") ,
  predictors.op = c("mean"),
  time.predictors.prior = 1900:1906 ,
  special.predictors = list(
    list("averagegdppc", seq(1895,1905,5), "mean"),
    list("primary_barro", seq(1895,1905,5), "mean"),
    list("ggdp", 1903:1906, "mean")
  ),
  dependent = "ggdp",
  unit.variable = "codenum",
  unit.names.variable = "countrycode",
  time.variable = "year",
  treatment.identifier = 88,
  controls.identifier = c(5, 7, 13, 16, 22, 26, 28, 43, 47, 49, 58, 59, 63, 65, 81, 
                          101, 104, 122, 123, 135, 136, 137),
  time.optimize.ssr = 1900:1906,
  time.plot = 1900:1916)

# Correr a função synth()
synth.out3 <- synth(data.prep.obj = dataprep.out3, method = "BFGS")

# Extração dos resultados da optimização feita pela função synth(). Replicação parcial da Tabela 3.
synth.tables3 <- synth.tab(dataprep.res = dataprep.out3, synth.res = synth.out3)
synth.tables3$tab
synth.tables3$tab.pred

# Replicação da Figura 4
Y1placebo2 <- dataprep.out3$Y1
Y1synthplacebo2 <- dataprep.out3$Y0 %*% synth.out3$solution.w
{plot(1900:1916, Y1placebo2, ylim=c(-15,15), type="l", xlab = "Year", ylab = "Netherlands GDP per capita growth rate")
  lines(1900:1916, Y1synthplacebo2, type="l", lty=2)}

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_PLC2.png")
path.plot(synth.res = synth.out3, dataprep.res = dataprep.out3,
          Ylab = "Real GDP per capita Growth Rate", Xlab = "Year",
          Ylim = c(-7, 6), Legend = c("The Netherlands",
                                      "Synthetic Netherlands"), Legend.position = "bottomleft")
dev.off()


#PLACEBO - final:
png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-COUntries/Tese/Img/NOR_ALL.png")
{plot(1900:1916, Y1, ylim=c(-5,10), type="l", xlab = "Year", ylab = "Real GDP per capita Growth Rate", col="red", lwd=c(2))
  lines(1900:1916, Y1placebo, type="l", lty=2, col="blue")
  lines(1900:1916, Y1placebo2, type="l", lty=2, col="blue4")
  legend("bottomleft", legend = c("Norway", "Sweden", "The Netherlands"),
         col = c("red", "blue", "blue4"), lty = c(1,2,2), cex = 0.8)
  abline(v=c(1906), col=c("black"), lwd=c(2))}
dev.off()


#### DENSITY #####
effects<- as.data.frame(Y1 - Y1synth)
effects <- cbind(newColName = rownames(effects), effects)
colnames(effects)<-c("year", "country")
effects['placebo1'] <- (Y1placebo - Y1synthplacebo)
effects['placebo2'] <- (Y1placebo2 - Y1synthplacebo2)

std <- as.data.frame(effects[effects$year>1906, "year"])
colnames(std) <- "year"
mean_country <- sum(effects[effects$year>1906,'country'])/10
mean_p1 <- sum(effects[effects$year>1906,'placebo1'])/10
mean_p2 <- sum(effects[effects$year>1906,'placebo2'])/10

for (i in seq(1,10)){
  std[i,'country'] <- (effects[effects$year==std[i,'year'],'country']-mean_country)^2
  std[i,'placebo1'] <- (effects[effects$year==std[i,'year'],'placebo1']-mean_p1)^2
  std[i,'placebo2'] <- (effects[effects$year==std[i,'year'],'placebo2']-mean_p2)^2
}

std[11,'year']<- 'std_10' 
std[11,'country'] <- sqrt(sum(na.omit(std$country))/(nrow(std)-2))
std[11,'placebo1'] <- sqrt(sum(na.omit(std$placebo1))/(nrow(std)-2))
std[11,'placebo2'] <- sqrt(sum(na.omit(std$placebo2))/(nrow(std)-2))

std[12,'year']<- 'std_2' 
std[12,'country'] <- sqrt(sum(std[std$year<=1908,'country'])/(1908-1906-1))
std[12,'placebo1'] <- sqrt(sum(std[std$year<=1908,'placebo1'])/(1908-1906-1))
std[12,'placebo2'] <- sqrt(sum(std[std$year<=1908,'placebo2'])/(1908-1906-1))

std[13,'year']<- 'std_3' 
std[13,'country'] <- sqrt(sum(std[std$year<=1909,'country'])/(1909-1906-1))
std[13,'placebo1'] <- sqrt(sum(std[std$year<=1909,'placebo1'])/(1909-1906-1))
std[13,'placebo2'] <- sqrt(sum(std[std$year<=1909,'placebo2'])/(1909-1906-1))


std[14,'year']<- 'std_4' 
std[14,'country'] <- sqrt(sum(std[std$year<=1910,'country'])/(1910-1906-1))
std[14,'placebo1'] <- sqrt(sum(std[std$year<=1910,'placebo1'])/(1910-1906-1))
std[14,'placebo2'] <- sqrt(sum(std[std$year<=1910,'placebo2'])/(1910-1906-1))


std[15,'year']<- 'std_5' 
std[15,'country'] <- sqrt(sum(std[std$year<=1911,'country'])/(1911-1906-1))
std[15,'placebo1'] <- sqrt(sum(std[std$year<=1911,'placebo1'])/(1911-1906-1))
std[15,'placebo2'] <- sqrt(sum(std[std$year<=1911,'placebo2'])/(1911-1906-1))

density <- as.data.frame(c("1year", "2year", "3year", "4year", "5year", "10year"))
colnames(density) <- "effect"
density[density$effect=="1year", "ATE_country"]<- effects[effects$year==1907,"country"]
density[density$effect=="2year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908,"country"])/2
density[density$effect=="3year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909,"country"])/3
density[density$effect=="4year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910,"country"])/4
density[density$effect=="5year", "ATE_country"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910 | effects$year==1911,"country"])/5
density[density$effect=="10year", "ATE_country"]<- sum(effects[effects$year>1906,"country"])/(1916-1906)

density[density$effect=="1year", "ATE_placebo1"]<- effects[effects$year==1907,"placebo1"]
density[density$effect=="2year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908,"placebo1"])/2
density[density$effect=="3year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909,"placebo1"])/3
density[density$effect=="4year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910,"placebo1"])/4
density[density$effect=="5year", "ATE_placebo1"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910 | effects$year==1911,"placebo1"])/5
density[density$effect=="10year", "ATE_placebo1"]<- sum(effects[effects$year>1906,"placebo1"])/(1916-1906)

density[density$effect=="1year", "ATE_placebo2"]<- effects[effects$year==1907,"placebo2"]
density[density$effect=="2year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908,"placebo2"])/2
density[density$effect=="3year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909,"placebo2"])/3
density[density$effect=="4year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910,"placebo2"])/4
density[density$effect=="5year", "ATE_placebo2"]<- sum(effects[effects$year==1907 | effects$year==1908| effects$year==1909 | effects$year==1910 | effects$year==1911,"placebo2"])/5
density[density$effect=="10year", "ATE_placebo2"]<- sum(effects[effects$year>1906,"placebo2"])/(1916-1906)

dbwm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_country'],
    sd = std[std$year=='std_10', 'country']
  )

dbwm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo1'],
    sd = std[std$year=='std_10', 'placebo1']
  )

dbwm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='10year','ATE_placebo2'],
    sd = std[std$year=='std_10', 'placebo2']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_DEN_10Y.png")
plot(density(dbwm_data),main = '10-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.2), lwd=c(2))
lines(density(dbwm_datap1), col='blue', lty=2)
lines(density(dbwm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

dbwm_data <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_country'],
    sd = std[std$year=='std_5', 'country']
  )

dbwm_datap1 <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_placebo1'],
    sd = std[std$year=='std_5', 'placebo1']
  )

dbwm_datap2 <-
  rnorm(
    n = 10000,
    mean = density[density$effect=='5year','ATE_placebo2'],
    sd = std[std$year=='std_5', 'placebo2']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_DEN_5Y.png")
plot(density(dbwm_data),main = '5-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.2), lwd=c(2))
lines(density(dbwm_datap1), col='blue', lty=2)
lines(density(dbwm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

dbwm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_country'],
    sd = std[std$year=='std_4', 'country']
  )

dbwm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo1'],
    sd = std[std$year=='std_4', 'placebo1']
  )

dbwm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='4year','ATE_placebo2'],
    sd = std[std$year=='std_4', 'placebo2']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_DEN_4Y.png")
plot(density(dbwm_data),main = '4-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.2), lwd=c(2))
lines(density(dbwm_datap1), col='blue', lty=2)
lines(density(dbwm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()


dbwm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_country'],
    sd = std[std$year=='std_3', 'country']
  )

dbwm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_placebo1'],
    sd = std[std$year=='std_3', 'placebo1']
  )

dbwm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='3year','ATE_placebo2'],
    sd = std[std$year=='std_3', 'placebo2']
  )


png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_DEN_3Y.png")
plot(density(dbwm_data),main = '3-years ATE Density ',  col="red", xlim=c(-20,20), ylim=c(0, 0.2), lwd=c(2))
lines(density(dbwm_datap1), col='blue', lty=2)
lines(density(dbwm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()

dbwm_data <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_country'],
    sd = std[std$year=='std_2', 'country']
  )

dbwm_datap1 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_placebo1'],
    sd = std[std$year=='std_2', 'placebo1']
  )

dbwm_datap2 <-
  rnorm(
    n = 1000000,
    mean = density[density$effect=='2year','ATE_placebo2'],
    sd = std[std$year=='std_2', 'placebo2']
  )

png(filename="C:/Users/paben/OneDrive/Ambiente de Trabalho/Tese/Final Data/Thesis/SCM-Countries/Tese/Img/NOR_DEN_2Y.png")
plot(density(dbwm_data),main = '2-years ATE Density ',  col="red", xlim=c(-30,30), ylim=c(0, 0.15), lwd=c(2))
lines(density(dbwm_datap1), col='blue', lty=2)
lines(density(dbwm_datap2), col='green', lty=2)
legend("topleft", legend = c("Norway", "Sweden ", "The Netherlands " ),
       col = c("red", "blue", "lightblue", "green"), lty = c(1,2,2,2), cex = 0.8)
abline(v=c(0), col=c("black"), lwd=c(2))
dev.off()



