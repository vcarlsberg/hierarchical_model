library(gsheet)
library(lme4)
library(lmerTest)
library(nlme)
library(ggplot2)
library(tidyverse)
library(dplyr)

url<-"https://drive.google.com/file/d/1DM2yWceOGOPbz2ws0BtorkzIMT01D742/view?usp=sharing"
#gsheet2tbl(url)
a <- gsheet2text(url, format='csv')
dataset <- read.csv(text=a, stringsAsFactors=FALSE)
dataset$Nama_Prov<-as.factor(dataset$Nama_Prov)

ggplot_shiny(dataset)

h<-ggplot(dataset, aes(x = Nama_Prov,y=PDRB))+
  geom_bar(position = 'dodge', stat='identity')+
  geom_text(aes(label=PDRB), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title="PDRB per Propinsi", 
       x="Nama Propinsi",y="PDRB")

  
h

hhh<-dataset %>% count(Nama_Prov)
  
g <- ggplot(hhh, aes(x = Nama_Prov,y=n))+
  geom_bar(position = 'dodge', stat='identity')+
  labs(title="Sebaran Data tiap Provinsi", 
       x="Nama Propinsi",y="Jumlah Data")+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)
g

hhh<-dataset
h<-ggplot(hhh, aes(x = Nama_Prov,y=UMK))+geom_boxplot()+
  geom_hline(aes(yintercept = mean(UMK)), color="red")+
  labs(title="Banyaknya UMK di tiap Kota/Kab per Propinsi",
       x="Nama Propinsi",
       y="Jumlah UMK per Kota/Kab")
h

hhh<-dataset
k<-ggplot(hhh, aes(x = Nama_Prov,y=UMB))+geom_boxplot()+
  labs(title="Banyaknya UMB di tiap Kota/Kab per Propinsi",
       x="Nama Propinsi",
       y="Jumlah UMB per Kota/Kab")
k

hhh<-dataset
m<-ggplot(hhh, aes(x = Nama_Prov,y=IPM))+geom_boxplot()+
  labs(title="Tingkat IPM di tiap Kota/Kab per Propinsi",
       x="Nama Propinsi",
       y="IPM per Kota/Kab")
m


#hist(dataset$UMK,breaks = 20)

#linear.model<-lm(PDRB~UMK+UMB+IPM, data = dataset)
#summary(linear.model)
summary(dataset)

filter_dataset<-dataset %>% filter(Nama_Prov == "Jawa Timur")

qqnorm(filter_dataset$UMK, pch = 1, frame = FALSE)
qqline(filter_dataset$UMK, col = "steelblue", lwd = 2)

shapiro.test(filter_dataset$UMB)

lm.model<-lm(IPM~UMB+UMK,data=dataset)
summary(lm.model)
plot(lm.model)
plot(dataset$IPM,dataset$UMB)
abline(lm.model)

abline(lm(IPM~UMK+UMB,data=dataset))


lme.model<-lme(fixed=IPM~UMK+UMB, random=~UMK+UMB|Nama_Prov, data=dataset)
summary(lme.model)
coef(lme.model)
fixef(lme.model)
ranef(lme.model)
confint(lme.model)
intervals(lme.model)
qqnorm(lme.model)

lmer.model<-lmer(IPM~UMK+UMB+(UMK+UMB|Nama_Prov), data = dataset)
summary(lmer.model)
coef(lmer.model)
fixef(lmer.model)
ranef(lmer.model)
confint(lmer.model,level=0.95)

url_l2<-"https://drive.google.com/file/d/1DM2yWceOGOPbz2ws0BtorkzIMT01D742/view?usp=sharing"
a <- gsheet2text(url_l2, format='csv',sheetid = 735654266)
dataset_l2 <- read.csv(text=a, stringsAsFactors=FALSE)
dataset_l2$Nama_Prov<-as.factor(dataset_l2$Nama_Prov)
lm.model.l2<-lm(PDRB~Intercept+Slope_UMK+Slope_UMB,data=dataset_l2)
summary(lm.model.l2)

######################bawah sini coret2an##################

fit <- brm(IPM~UMK+UMB+(UMK+UMB|Nama_Prov), 
           data = dataset)
summary(fit)
lmer.model@vcov_beta

anova(linear.model, lmer.model)

lmm.data <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
MLexamp.6 <- lmer(extro ~ open + agree + social + (1|school), data=lmm.data,
                  REML = FALSE,
                  control=lmerControl(optimizer="bobyqa"))
summary(MLexamp.6)

MLexamp.7 <- lmer(extro ~ open + agree + social + (1|school) + (1|class), 
                  data=lmm.data,
                  REML = FALSE)
summary(MLexamp.7)

MLexamp.8 <- lmer(extro ~ open + agree + social + (1|school/class), data=lmm.data)

