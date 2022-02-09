library(readxl)
getwd()
SR_Nivelle <- read_excel("C:/Users/tewan/OneDrive/Bureau/Master 2/Cours/ptut/SR_NIvelle sous R/Modèle SR_Nivelle/SR_Nivelle.xlsx")

attach(SR_Nivelle)

#Modification des noms des variables, on assimile le stock et le recrutement aux densit?s pour notre analyse
Stock<-d_oeufs
Recrutement<-`d_0+`
Nivelle_libre <- SR_Nivelle[7:36,]


#observation des donn?es
#Stock en fonction de l'ann?e
plot(Stock~Cohorte)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
ggplot(SR_Nivelle, aes(Cohorte, Stock)) + 
  geom_bar(stat = "identity",position=position_dodge(),fill = ifelse(Cohorte < 1991,'#404080','#69b3a2')) +
  ggtitle("Stock en fonction de l'annee")+theme(plot.title = element_text(hjust = 0.5))
 

#Recrutement en fonction de l'annee
plot(Recrutement~Cohorte)
library(ggplot2)
ggplot(SR_Nivelle, aes(Cohorte, Recrutement)) + 
  geom_bar(stat = "identity",position=position_dodge(),fill = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+
  ggtitle("Recrutement en fonction de l'ann?e")+theme(plot.title = element_text(hjust = 0.5))

#Stock en fonction du recrutement
plot(Stock~Recrutement)
library(ggplot2)
ggplot(SR_Nivelle, aes(Stock, Recrutement)) + 
  geom_point(col = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+
  ggtitle("Stock en fonction du recrutement")+theme(plot.title = element_text(hjust = 0.5))
#mise en valeur de la capacit? d'accueil

#Stock en fonction du recrutement sans les 5 premières années
plot(Nivelle_libre$d_oeufs~Nivelle_libre$`d_0+`)
library(ggplot2)
ggplot(Nivelle_libre, aes(d_oeufs, `d_0+`)) + 
  geom_point()+
  ggtitle("Stock en fonction du recrutement")+theme(plot.title = element_text(hjust = 0.5))
#mise en valeur de la capacit? d'accueil

#graphique des densit?s de stock et de recrutement
library(cowplot)
plot(Stock~Cohorte)
plot(Recrutement~Cohorte)
d1<-ggplot(data=SR_Nivelle, aes(x=Cohorte,y=Stock),add=TRUE) +
  geom_line(color="orange") +
  geom_point(col = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+
  ggtitle("Densite de stock en fonction des annees")+theme(plot.title = element_text(hjust = 0.5))
  
d2<-ggplot(data=SR_Nivelle, aes(x=Cohorte,y=Recrutement),add=TRUE) +
  geom_line(color="Steelblue") +
  geom_point(col = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+
  ggtitle("Densite de recrutement en fonction des annees")+theme(plot.title = element_text(hjust = 0.5))

plot_grid(d1, d2, ncol = 2, nrow = 1)

#superposition des deux r?gressions (lin?aire et polynomiale) pour le stock en fonction de l'ann?e
ggplot(SR_Nivelle, aes(Cohorte, Stock)) + 
  geom_bar(stat = "identity",fill = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+ 
  geom_smooth(method = "loess", se = T,level=0.95)+
  geom_smooth(method = "lm", se = T,level=0.95,col="red")+
  ggtitle("Stock en fonction de l'ann?e")+theme(plot.title = element_text(hjust = 0.5))

#superposition des deux r?gressions (lin?aire et polynomiale) pour le recrutement en fonction de l'ann?e
ggplot(SR_Nivelle, aes(Cohorte, Recrutement)) + 
  geom_bar(stat = "identity",fill = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+ 
  geom_smooth(method = "loess", se = T,level=0.95)+
  geom_smooth(method = "lm", se = T,level=0.95,col="red")+
  ggtitle("Recrutement en fonction de l'ann?e")+theme(plot.title = element_text(hjust = 0.5))

#superposition des deux r?gressions (lin?aire et polynomiale) pour le recrutement en fonction du stock
pol_2 <- ggplot(SR_Nivelle, aes(Stock, Recrutement)) + 
  geom_point(col = ifelse(Cohorte < 1991,'#404080','#69b3a2'))+ 
  geom_smooth(method = "loess", se = T,level=0.95)+
  geom_smooth(method = "lm", se = T,level=0.95,col="red")+
  ggtitle("Stock en fonction du recrutement")+theme(plot.title = element_text(hjust = 0.5))

pol_2

#superposition des deux r?gressions (lin?aire et polynomiale) pour le recrutement en fonction du stock
pol_3 <- ggplot(Nivelle_libre, aes(Nivelle_libre$d_oeufs, Nivelle_libre$`d_0+`)) + 
  geom_point()+ 
  geom_smooth(method = "loess", se = T,level=0.95)+
  geom_smooth(method = "lm", se = T,level=0.95,col="red")+
  ggtitle("Stock en fonction du recrutement")+theme(plot.title = element_text(hjust = 0.5))

pol_3
Nivelle_libre$d_oeufs<-log(Nivelle_libre$d_oeufs)
Nivelle_libre$`d_0+`<-log(Nivelle_libre$`d_0+`)
#tester en excluant les 5 premières années

############################TEST D'IMPLEMENTATION DU MODELE
#######RICKER
###BETA1
library(ggplot2)
library(R2OpenBUGS)
modelSR<-"Modèle_saumonRICKER.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0,057775347)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out1R<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out1R)
plot(out1R)
res<-out1R$summary[4:39,1]
plot(out1R$summary[4:39,1])#juste sur les moyennes
boxplot(out1R$sims.list$e) #boxplot de toute la distribution

#######BETA2
  modelSR<-"Modèle_saumonRICKER.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0,115550694)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out2R<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out2R)
plot(out2R)
plot(out2R$summary[4:39,1]) #juste sur les moyennes
boxplot(out2R$sims.list$e) #boxplot de toute la distribution

####BETA5
  modelSR<-"Modèle_saumonRICKER.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0.288876735)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out5R<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out5R)
plot(out5R)
plot(out5R$summary[4:39,1])
abline(lm(out5R$summary[4:39,1]~SR_Nivelle$Cohorte))
#juste sur les moyennes
boxplot(out5R$sims.list$e) #boxplot de toute la distribution

###BETA10
  modelSR<-"Modèle_saumonRICKER.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0,57775347)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out10R<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out10R)
plot(out10R)
plot(out10R$summary[4:39,1]) #juste sur les moyennes
boxplot(out10R$sims.list$e) #boxplot de toute la distribution
  
####################BH
###BETA1
   modelSR<-"Modèle_saumonBH.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0,057775347)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out1BH<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out1BH)
plot(out1BH)
plot(out1BH$summary[4:39,1]) #juste sur les moyennes
boxplot(out1BH$sims.list$e) #boxplot de toute la distribution

#####BETA2
  modelSR<-"Modèle_saumonBH.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0,115550694)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out2BH<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out2BH)
plot(out2BH)
plot(out2BH$summary[4:39,1]) #juste sur les moyennes
boxplot(out2BH$sims.list$e) #boxplot de toute la distribution

####BETA5
  modelSR<-"Modèle_saumonBH.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0.288876735)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out5BH<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out5BH)
plot(out5BH)
plot(out5BH$summary[4:39,1]) #juste sur les moyennes
boxplot(out5BH$sims.list$e) #boxplot de toute la distribution
  
######BETA10
  modelSR<-"Modèle_saumonBH.odc"
#définir les données
T<-nrow(as.matrix(Cohorte))
donnees<-list(S=S,R=R,T=T)

#définir les paramètres
params<-c("a","Rmax","sigma","e")

#Initialisation des chaînes
inits<-function(){
  list(a=0,57775347)
  list(Rmax= 0.277)
  list(sigma=1)
}

#simulation
out10BH<-bugs(donnees,inits,params,modelSR,n.chains=3,debug=TRUE,n.iter=10000,working.directory=getwd())
print(out10BH)
plot(out10BH)
plot(out10BH$summary[4:39,1]) #juste sur les moyennes
boxplot(out10BH$sims.list$e) #boxplot de toute la distribution

#tous les DIC

data.frame(out1R$DIC,out2R$DIC,out5R$DIC,out10R$DIC,out1BH$DIC,out2BH$DIC,out5BH$DIC,out10BH$DIC)

#problème d'estimation avec BH mais le modèle est mieux ajusté
#manque d'infos aux très faibles déposes d'oeufs

#graphique d'interpétation des modèles
#RICKER
#prior a
boxplot(out1R$sims.list$a,out2R$sims.list$a,out5R$sims.list$a,out10R$sims.list$a,out1BH$sims.list$a,out2BH$sims.list$a,out5BH$sims.list$a,out10BH$sims.list$a,ylab="mean",main="Boîtes à moustache des moyennes de a")+
  mtext(c("R1","R2","R5","R10","BH1","BH2","BH5","BH10"),side=1,at = c(1:8),line = 1)
#que ricker
boxplot(out1R$sims.list$a,out2R$sims.list$a,out5R$sims.list$a,out10R$sims.list$a,ylab="mean",main="Boîtes à moustache des moyennes de a")
#prior Rmax
boxplot(out1R$sims.list$Rmax,out2R$sims.list$Rmax,out5R$sims.list$Rmax,out10R$sims.list$Rmax,out1BH$sims.list$Rmax,out2BH$sims.list$Rmax,out5BH$sims.list$Rmax,out10BH$sims.list$Rmax,ylab="mean",main="Boîtes à moustache des moyennes de Rmax",ylim=c(0.05,0.25))+
mtext(c("R1","R2","R5","R10","BH1","BH2","BH5","BH10"),side=1,at = c(1:8),line = 1)
#sensibilité de BH au prior a sur rmax
#prior sigma
boxplot(out1R$sims.list$sigma,out2R$sims.list$sigma,out5R$sims.list$sigma,out10R$sims.list$sigma,out1BH$sims.list$sigma,out2BH$sims.list$sigma,out5BH$sims.list$sigma,out10BH$sims.list$sigma,ylab="mean",main="Boîtes à moustache des moyennes de sigma")+
  mtext(c("R1","R2","R5","R10","BH1","BH2","BH5","BH10"),side=1,at = c(1:8),line = 1)


#refaire tourner le modèle pour BH1

boxplot(out2R$sims.list[-4],ylab="mean")
boxplot(out2R$sims.list$a,xlab="a",ylab="mean")

boxplot(out5R$sims.list[-4],ylab="mean")
boxplot(out5R$sims.list$a,xlab="a",ylab="mean")

boxplot(out10R$sims.list[-4],ylab="mean")
boxplot(out10R$sims.list$a,xlab="a",ylab="mean")

#BH
boxplot(out1BH$sims.list[-4],ylab="mean")
boxplot(out1BH$sims.list$a,xlab="a",ylab="mean")

boxplot(out2BH$sims.list[-4],ylab="mean")
boxplot(out2BH$sims.list$a,xlab="a",ylab="mean")

boxplot(out5BH$sims.list[-4],ylab="mean")
boxplot(out5BH$sims.list$a,xlab="a",ylab="mean")

boxplot(out10BH$sims.list[-4],ylab="mean")
boxplot(out10BH$sims.list$a,xlab="a",ylab="mean")

###Ajout des résidus au modèle et loess
#ricker beta1
Rr51<-mean(out1R$sims.list$e[,1])
Rr52<-mean(out1R$sims.list$e[,2])
Rr53<-mean(out1R$sims.list$e[,3])
Rr54<-mean(out1R$sims.list$e[,4])
Rr55<-mean(out1R$sims.list$e[,5])
Rr56<-mean(out1R$sims.list$e[,6])
Rr57<-mean(out1R$sims.list$e[,7])
Rr58<-mean(out1R$sims.list$e[,8])
Rr59<-mean(out1R$sims.list$e[,9])
Rr510<-mean(out1R$sims.list$e[,10])
Rr511<-mean(out1R$sims.list$e[,11])
Rr512<-mean(out1R$sims.list$e[,12])
Rr513<-mean(out1R$sims.list$e[,13])
Rr514<-mean(out1R$sims.list$e[,14])
Rr515<-mean(out1R$sims.list$e[,15])
Rr516<-mean(out1R$sims.list$e[,16])
Rr517<-mean(out1R$sims.list$e[,17])
Rr518<-mean(out1R$sims.list$e[,18])
Rr519<-mean(out1R$sims.list$e[,19])
Rr520<-mean(out1R$sims.list$e[,20])
Rr521<-mean(out1R$sims.list$e[,21])
Rr522<-mean(out1R$sims.list$e[,22])
Rr523<-mean(out1R$sims.list$e[,23])
Rr524<-mean(out1R$sims.list$e[,24])
Rr525<-mean(out1R$sims.list$e[,25])
Rr526<-mean(out1R$sims.list$e[,26])
Rr527<-mean(out1R$sims.list$e[,27])
Rr528<-mean(out1R$sims.list$e[,28])
Rr529<-mean(out1R$sims.list$e[,29])
Rr530<-mean(out1R$sims.list$e[,30])
Rr531<-mean(out1R$sims.list$e[,31])
Rr532<-mean(out1R$sims.list$e[,32])
Rr533<-mean(out1R$sims.list$e[,33])
Rr534<-mean(out1R$sims.list$e[,34])
Rr535<-mean(out1R$sims.list$e[,35])
Rr536<-mean(out1R$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Ricker beta 1")+theme(plot.title = element_text(hjust = 0.5))

#ricker beta2
Rr51<-mean(out2R$sims.list$e[,1])
Rr52<-mean(out2R$sims.list$e[,2])
Rr53<-mean(out2R$sims.list$e[,3])
Rr54<-mean(out2R$sims.list$e[,4])
Rr55<-mean(out2R$sims.list$e[,5])
Rr56<-mean(out2R$sims.list$e[,6])
Rr57<-mean(out2R$sims.list$e[,7])
Rr58<-mean(out2R$sims.list$e[,8])
Rr59<-mean(out2R$sims.list$e[,9])
Rr510<-mean(out2R$sims.list$e[,10])
Rr511<-mean(out2R$sims.list$e[,11])
Rr512<-mean(out2R$sims.list$e[,12])
Rr513<-mean(out2R$sims.list$e[,13])
Rr514<-mean(out2R$sims.list$e[,14])
Rr515<-mean(out2R$sims.list$e[,15])
Rr516<-mean(out2R$sims.list$e[,16])
Rr517<-mean(out2R$sims.list$e[,17])
Rr518<-mean(out2R$sims.list$e[,18])
Rr519<-mean(out2R$sims.list$e[,19])
Rr520<-mean(out2R$sims.list$e[,20])
Rr521<-mean(out2R$sims.list$e[,21])
Rr522<-mean(out2R$sims.list$e[,22])
Rr523<-mean(out2R$sims.list$e[,23])
Rr524<-mean(out2R$sims.list$e[,24])
Rr525<-mean(out2R$sims.list$e[,25])
Rr526<-mean(out2R$sims.list$e[,26])
Rr527<-mean(out2R$sims.list$e[,27])
Rr528<-mean(out2R$sims.list$e[,28])
Rr529<-mean(out2R$sims.list$e[,29])
Rr530<-mean(out2R$sims.list$e[,30])
Rr531<-mean(out2R$sims.list$e[,31])
Rr532<-mean(out2R$sims.list$e[,32])
Rr533<-mean(out2R$sims.list$e[,33])
Rr534<-mean(out2R$sims.list$e[,34])
Rr535<-mean(out2R$sims.list$e[,35])
Rr536<-mean(out2R$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Ricker beta 2")+theme(plot.title = element_text(hjust = 0.5))

#ricker beta5
Rr51<-mean(out5R$sims.list$e[,1])
Rr52<-mean(out5R$sims.list$e[,2])
Rr53<-mean(out5R$sims.list$e[,3])
Rr54<-mean(out5R$sims.list$e[,4])
Rr55<-mean(out5R$sims.list$e[,5])
Rr56<-mean(out5R$sims.list$e[,6])
Rr57<-mean(out5R$sims.list$e[,7])
Rr58<-mean(out5R$sims.list$e[,8])
Rr59<-mean(out5R$sims.list$e[,9])
Rr510<-mean(out5R$sims.list$e[,10])
Rr511<-mean(out5R$sims.list$e[,11])
Rr512<-mean(out5R$sims.list$e[,12])
Rr513<-mean(out5R$sims.list$e[,13])
Rr514<-mean(out5R$sims.list$e[,14])
Rr515<-mean(out5R$sims.list$e[,15])
Rr516<-mean(out5R$sims.list$e[,16])
Rr517<-mean(out5R$sims.list$e[,17])
Rr518<-mean(out5R$sims.list$e[,18])
Rr519<-mean(out5R$sims.list$e[,19])
Rr520<-mean(out5R$sims.list$e[,20])
Rr521<-mean(out5R$sims.list$e[,21])
Rr522<-mean(out5R$sims.list$e[,22])
Rr523<-mean(out5R$sims.list$e[,23])
Rr524<-mean(out5R$sims.list$e[,24])
Rr525<-mean(out5R$sims.list$e[,25])
Rr526<-mean(out5R$sims.list$e[,26])
Rr527<-mean(out5R$sims.list$e[,27])
Rr528<-mean(out5R$sims.list$e[,28])
Rr529<-mean(out5R$sims.list$e[,29])
Rr530<-mean(out5R$sims.list$e[,30])
Rr531<-mean(out5R$sims.list$e[,31])
Rr532<-mean(out5R$sims.list$e[,32])
Rr533<-mean(out5R$sims.list$e[,33])
Rr534<-mean(out5R$sims.list$e[,34])
Rr535<-mean(out5R$sims.list$e[,35])
Rr536<-mean(out5R$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Ricker beta 5")+theme(plot.title = element_text(hjust = 0.5))

#ricker beta10
Rr51<-mean(out10R$sims.list$e[,1])
Rr52<-mean(out10R$sims.list$e[,2])
Rr53<-mean(out10R$sims.list$e[,3])
Rr54<-mean(out10R$sims.list$e[,4])
Rr55<-mean(out10R$sims.list$e[,5])
Rr56<-mean(out10R$sims.list$e[,6])
Rr57<-mean(out10R$sims.list$e[,7])
Rr58<-mean(out10R$sims.list$e[,8])
Rr59<-mean(out10R$sims.list$e[,9])
Rr510<-mean(out10R$sims.list$e[,10])
Rr511<-mean(out10R$sims.list$e[,11])
Rr512<-mean(out10R$sims.list$e[,12])
Rr513<-mean(out10R$sims.list$e[,13])
Rr514<-mean(out10R$sims.list$e[,14])
Rr515<-mean(out10R$sims.list$e[,15])
Rr516<-mean(out10R$sims.list$e[,16])
Rr517<-mean(out10R$sims.list$e[,17])
Rr518<-mean(out10R$sims.list$e[,18])
Rr519<-mean(out10R$sims.list$e[,19])
Rr520<-mean(out10R$sims.list$e[,20])
Rr521<-mean(out10R$sims.list$e[,21])
Rr522<-mean(out10R$sims.list$e[,22])
Rr523<-mean(out10R$sims.list$e[,23])
Rr524<-mean(out10R$sims.list$e[,24])
Rr525<-mean(out10R$sims.list$e[,25])
Rr526<-mean(out10R$sims.list$e[,26])
Rr527<-mean(out10R$sims.list$e[,27])
Rr528<-mean(out10R$sims.list$e[,28])
Rr529<-mean(out10R$sims.list$e[,29])
Rr530<-mean(out10R$sims.list$e[,30])
Rr531<-mean(out10R$sims.list$e[,31])
Rr532<-mean(out10R$sims.list$e[,32])
Rr533<-mean(out10R$sims.list$e[,33])
Rr534<-mean(out10R$sims.list$e[,34])
Rr535<-mean(out10R$sims.list$e[,35])
Rr536<-mean(out10R$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Ricker beta 10")+theme(plot.title = element_text(hjust = 0.5))

#########
#BH beta1
Rr51<-mean(out1BH$sims.list$e[,1])
Rr52<-mean(out1BH$sims.list$e[,2])
Rr53<-mean(out1BH$sims.list$e[,3])
Rr54<-mean(out1BH$sims.list$e[,4])
Rr55<-mean(out1BH$sims.list$e[,5])
Rr56<-mean(out1BH$sims.list$e[,6])
Rr57<-mean(out1BH$sims.list$e[,7])
Rr58<-mean(out1BH$sims.list$e[,8])
Rr59<-mean(out1BH$sims.list$e[,9])
Rr510<-mean(out1BH$sims.list$e[,10])
Rr511<-mean(out1BH$sims.list$e[,11])
Rr512<-mean(out1BH$sims.list$e[,12])
Rr513<-mean(out1BH$sims.list$e[,13])
Rr514<-mean(out1BH$sims.list$e[,14])
Rr515<-mean(out1BH$sims.list$e[,15])
Rr516<-mean(out1BH$sims.list$e[,16])
Rr517<-mean(out1BH$sims.list$e[,17])
Rr518<-mean(out1BH$sims.list$e[,18])
Rr519<-mean(out1BH$sims.list$e[,19])
Rr520<-mean(out1BH$sims.list$e[,20])
Rr521<-mean(out1BH$sims.list$e[,21])
Rr522<-mean(out1BH$sims.list$e[,22])
Rr523<-mean(out1BH$sims.list$e[,23])
Rr524<-mean(out1BH$sims.list$e[,24])
Rr525<-mean(out1BH$sims.list$e[,25])
Rr526<-mean(out1BH$sims.list$e[,26])
Rr527<-mean(out1BH$sims.list$e[,27])
Rr528<-mean(out1BH$sims.list$e[,28])
Rr529<-mean(out1BH$sims.list$e[,29])
Rr530<-mean(out1BH$sims.list$e[,30])
Rr531<-mean(out1BH$sims.list$e[,31])
Rr532<-mean(out1BH$sims.list$e[,32])
Rr533<-mean(out1BH$sims.list$e[,33])
Rr534<-mean(out1BH$sims.list$e[,34])
Rr535<-mean(out1BH$sims.list$e[,35])
Rr536<-mean(out1BH$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1")+theme(plot.title = element_text(hjust = 0.5))

#BH beta2
Rr51<-mean(out2BH$sims.list$e[,1])
Rr52<-mean(out2BH$sims.list$e[,2])
Rr53<-mean(out2BH$sims.list$e[,3])
Rr54<-mean(out2BH$sims.list$e[,4])
Rr55<-mean(out2BH$sims.list$e[,5])
Rr56<-mean(out2BH$sims.list$e[,6])
Rr57<-mean(out2BH$sims.list$e[,7])
Rr58<-mean(out2BH$sims.list$e[,8])
Rr59<-mean(out2BH$sims.list$e[,9])
Rr510<-mean(out2BH$sims.list$e[,10])
Rr511<-mean(out2BH$sims.list$e[,11])
Rr512<-mean(out2BH$sims.list$e[,12])
Rr513<-mean(out2BH$sims.list$e[,13])
Rr514<-mean(out2BH$sims.list$e[,14])
Rr515<-mean(out2BH$sims.list$e[,15])
Rr516<-mean(out2BH$sims.list$e[,16])
Rr517<-mean(out2BH$sims.list$e[,17])
Rr518<-mean(out2BH$sims.list$e[,18])
Rr519<-mean(out2BH$sims.list$e[,19])
Rr520<-mean(out2BH$sims.list$e[,20])
Rr521<-mean(out2BH$sims.list$e[,21])
Rr522<-mean(out2BH$sims.list$e[,22])
Rr523<-mean(out2BH$sims.list$e[,23])
Rr524<-mean(out2BH$sims.list$e[,24])
Rr525<-mean(out2BH$sims.list$e[,25])
Rr526<-mean(out2BH$sims.list$e[,26])
Rr527<-mean(out2BH$sims.list$e[,27])
Rr528<-mean(out2BH$sims.list$e[,28])
Rr529<-mean(out2BH$sims.list$e[,29])
Rr530<-mean(out2BH$sims.list$e[,30])
Rr531<-mean(out2BH$sims.list$e[,31])
Rr532<-mean(out2BH$sims.list$e[,32])
Rr533<-mean(out2BH$sims.list$e[,33])
Rr534<-mean(out2BH$sims.list$e[,34])
Rr535<-mean(out2BH$sims.list$e[,35])
Rr536<-mean(out2BH$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 2")+theme(plot.title = element_text(hjust = 0.5))

#BH beta5
Rr51<-mean(out5BH$sims.list$e[,1])
Rr52<-mean(out5BH$sims.list$e[,2])
Rr53<-mean(out5BH$sims.list$e[,3])
Rr54<-mean(out5BH$sims.list$e[,4])
Rr55<-mean(out5BH$sims.list$e[,5])
Rr56<-mean(out5BH$sims.list$e[,6])
Rr57<-mean(out5BH$sims.list$e[,7])
Rr58<-mean(out5BH$sims.list$e[,8])
Rr59<-mean(out5BH$sims.list$e[,9])
Rr510<-mean(out5BH$sims.list$e[,10])
Rr511<-mean(out5BH$sims.list$e[,11])
Rr512<-mean(out5BH$sims.list$e[,12])
Rr513<-mean(out5BH$sims.list$e[,13])
Rr514<-mean(out5BH$sims.list$e[,14])
Rr515<-mean(out5BH$sims.list$e[,15])
Rr516<-mean(out5BH$sims.list$e[,16])
Rr517<-mean(out5BH$sims.list$e[,17])
Rr518<-mean(out5BH$sims.list$e[,18])
Rr519<-mean(out5BH$sims.list$e[,19])
Rr520<-mean(out5BH$sims.list$e[,20])
Rr521<-mean(out5BH$sims.list$e[,21])
Rr522<-mean(out5BH$sims.list$e[,22])
Rr523<-mean(out5BH$sims.list$e[,23])
Rr524<-mean(out5BH$sims.list$e[,24])
Rr525<-mean(out5BH$sims.list$e[,25])
Rr526<-mean(out5BH$sims.list$e[,26])
Rr527<-mean(out5BH$sims.list$e[,27])
Rr528<-mean(out5BH$sims.list$e[,28])
Rr529<-mean(out5BH$sims.list$e[,29])
Rr530<-mean(out5BH$sims.list$e[,30])
Rr531<-mean(out5BH$sims.list$e[,31])
Rr532<-mean(out5BH$sims.list$e[,32])
Rr533<-mean(out5BH$sims.list$e[,33])
Rr534<-mean(out5BH$sims.list$e[,34])
Rr535<-mean(out5BH$sims.list$e[,35])
Rr536<-mean(out5BH$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 5")+theme(plot.title = element_text(hjust = 0.5))

#BH beta10
Rr51<-mean(out10BH$sims.list$e[,1])
Rr52<-mean(out10BH$sims.list$e[,2])
Rr53<-mean(out10BH$sims.list$e[,3])
Rr54<-mean(out10BH$sims.list$e[,4])
Rr55<-mean(out10BH$sims.list$e[,5])
Rr56<-mean(out10BH$sims.list$e[,6])
Rr57<-mean(out10BH$sims.list$e[,7])
Rr58<-mean(out10BH$sims.list$e[,8])
Rr59<-mean(out10BH$sims.list$e[,9])
Rr510<-mean(out10BH$sims.list$e[,10])
Rr511<-mean(out10BH$sims.list$e[,11])
Rr512<-mean(out10BH$sims.list$e[,12])
Rr513<-mean(out10BH$sims.list$e[,13])
Rr514<-mean(out10BH$sims.list$e[,14])
Rr515<-mean(out10BH$sims.list$e[,15])
Rr516<-mean(out10BH$sims.list$e[,16])
Rr517<-mean(out10BH$sims.list$e[,17])
Rr518<-mean(out10BH$sims.list$e[,18])
Rr519<-mean(out10BH$sims.list$e[,19])
Rr520<-mean(out10BH$sims.list$e[,20])
Rr521<-mean(out10BH$sims.list$e[,21])
Rr522<-mean(out10BH$sims.list$e[,22])
Rr523<-mean(out10BH$sims.list$e[,23])
Rr524<-mean(out10BH$sims.list$e[,24])
Rr525<-mean(out10BH$sims.list$e[,25])
Rr526<-mean(out10BH$sims.list$e[,26])
Rr527<-mean(out10BH$sims.list$e[,27])
Rr528<-mean(out10BH$sims.list$e[,28])
Rr529<-mean(out10BH$sims.list$e[,29])
Rr530<-mean(out10BH$sims.list$e[,30])
Rr531<-mean(out10BH$sims.list$e[,31])
Rr532<-mean(out10BH$sims.list$e[,32])
Rr533<-mean(out10BH$sims.list$e[,33])
Rr534<-mean(out10BH$sims.list$e[,34])
Rr535<-mean(out10BH$sims.list$e[,35])
Rr536<-mean(out10BH$sims.list$e[,36])

Residus<-c(Rr51,Rr52,Rr53,Rr54,Rr55,Rr56,Rr57,Rr58,Rr59,Rr510,Rr511,Rr512,Rr513,Rr514,Rr515,Rr516,Rr517,Rr518,Rr519,Rr520,Rr521,Rr522,Rr523,Rr524,Rr525,Rr526,Rr527,Rr528,Rr529,Rr530,Rr531,Rr532,Rr533,Rr534,Rr535,Rr536)
plot(Cohorte,Residus)
ggplot(SR_Nivelle,aes(x = Cohorte,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 10")+theme(plot.title = element_text(hjust = 0.5))


#################################################
###Observation des résidus en fonction du débit
##Moyenne annuelle
ggplot(Residus_Debit,aes(x = Residus_Debit$`Moyenne Annuelle`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit annuel")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Moyenne Annuelle`,Residus,method = "pearson")

#Moyenne sur la période entière
ggplot(Residus_Debit,aes(x = Residus_Debit$`Moyenne Periode`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit de décembre à mars")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Moyenne Periode`,Residus,method = "pearson")

#Moyenne période décembre à mi-janvier
ggplot(Residus_Debit,aes(x = Residus_Debit$`Moyenne Période 1`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit de décembre à mi-janvier")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Moyenne Période 1`,Residus,method = "pearson")

#Moyenne période mi-janvier à mi-février
ggplot(Residus_Debit,aes(x = Residus_Debit$`Moyenne Période 2`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit de mi-janvier à mi-février")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Moyenne Période 2`,Residus,method = "pearson")

#Moyenne période mi-février à mars
ggplot(Residus_Debit,aes(x = Residus_Debit$`Moyenne Période 3`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit de mi-février à mars")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Moyenne Période 3`,Residus,method = "pearson")

###############
##Maximum annuelle
ggplot(Residus_Debit,aes(x = Residus_Debit$`Maximum Annuelle`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit maximum annuel")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Maximum Annuelle`,Residus,method = "pearson")

#Maximum sur la période entière
ggplot(Residus_Debit,aes(x = Residus_Debit$`Maximum Periode`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit maximum de décembre à mars")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Maximum Periode`,Residus,method = "pearson")

#Maximum période décembre à mi-janvier
ggplot(Residus_Debit,aes(x = Residus_Debit$`Maximum Période 1`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit maximum de décembre à mi-janvier")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Maximum Période 1`,Residus,method = "pearson")

#Maximum période mi-janvier à mi-février
ggplot(Residus_Debit,aes(x = Residus_Debit$`Maximum Période 2`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit maximum de mi-janvier à mi-février")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Maximum Période 2`,Residus,method = "pearson")

#Maximum période mi-février à mars
ggplot(Residus_Debit,aes(x = Residus_Debit$`Maximum Période 3`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit maximum de mi-février à mars")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Maximum Période 3`,Residus,method = "pearson")

##Moyenne estivale
ggplot(Residus_Debit,aes(x = Residus_Debit$`Moyenne estivale`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit d'août à septembre")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Moyenne estivale`,Residus,method = "pearson")


##Minimum estivale
ggplot(Residus_Debit,aes(x = Residus_Debit$`Minimum estivale`,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus modèle Berverton-Hold beta 1 en fonction du débit minimum d'août à septembre")+theme(plot.title = element_text(hjust = 0.5))

cor(Residus_Debit$`Minimum estivale`,Residus,method = "pearson")



#############################échelle LOG
lmeanA<-log(Residus_Debit$`Moyenne Annuelle`)
lmeanP<-log(Residus_Debit$`Moyenne Periode`)
lmeanP1<-log(Residus_Debit$`Moyenne Période 1`)
lmeanP2<-log(Residus_Debit$`Moyenne Période 2`)
lmeanP3<-log(Residus_Debit$`Moyenne Période 3`)
lmaxA<-log(Residus_Debit$`Maximum Annuelle`)
lmaxP<-log(Residus_Debit$`Maximum Periode`)
lmaxP1<-log(Residus_Debit$`Maximum Période 1`)
lmaxP2<-log(Residus_Debit$`Maximum Période 2`)
lmaxP3<-log(Residus_Debit$`Maximum Période 3`)
lmeanE<-log(Residus_Debit$`Moyenne estivale`)
lminE<-log(Residus_Debit$`Minimum estivale`)

##Moyenne annuelle
ggplot(Residus_Debit,aes(x = lmeanA,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit annuel")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log de la moyenne annuelle")
cor(lmeanA,Residus,method = "pearson")

#Moyenne sur la période entière
ggplot(Residus_Debit,aes(x = lmeanP,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit de décembre à mars")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log de la moyenne de décembre à mars")

cor(lmeanP,Residus,method = "pearson")

#Moyenne période décembre à mi-janvier
ggplot(Residus_Debit,aes(x = lmeanP1,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit de décembre à mi-janvier")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log de la moyenne de décembre à mi-janvier")

cor(lmeanP1,Residus,method = "pearson")

#Moyenne période mi-janvier à mi-février
ggplot(Residus_Debit,aes(x = lmeanP2,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit de mi-janvier à mi-février")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log de la moyenne de mi-janvier à mi-février")

cor(lmeanP2,Residus,method = "pearson")

#Moyenne période mi-février à mars
ggplot(Residus_Debit,aes(x = lmeanP3,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit de mi-février à mars")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log de la moyenne de mi-février à mars")

cor(lmeanP3,Residus,method = "pearson")

###############
##Maximum annuel
ggplot(Residus_Debit,aes(x = lmaxA,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit maximum annuel")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log du maximum annuel")

cor(lmaxA,Residus,method = "pearson")

#Maximum sur la période entière
ggplot(Residus_Debit,aes(x = lmaxP,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit maximum de décembre à mars")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log du maximum de décembre à mars")

cor(lmaxP,Residus,method = "pearson")

#Maximum période décembre à mi-janvier
ggplot(Residus_Debit,aes(x = lmaxP1,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit maximum de décembre à mi-janvier")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log du maximum de décembre à mi-janvier")

cor(lmaxP1,Residus,method = "pearson")

#Maximum période mi-janvier à mi-février
ggplot(Residus_Debit,aes(x = lmaxP2,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit maximum de mi-janvier à mi-février")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log du maximum de mi-janvier à mi-février")

cor(lmaxP2,Residus,method = "pearson")

#Maximum période mi-février à mars
ggplot(Residus_Debit,aes(x = lmaxP3,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit maximum de mi-février à mars")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log du maximum de mi-février à mars")

cor(lmaxP3,Residus,method = "pearson")

##Moyenne estivale
ggplot(Residus_Debit,aes(x =lmeanE,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit d'août à septembre")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log de la moyenne d'août à septembre")

cor(lmeanE,Residus,method = "pearson")


##Minimum estivale
ggplot(Residus_Debit,aes(x = lminE,y=Residus))+
  geom_point()+
  geom_smooth(method = "loess", se = T,level=0.95)+
  ggtitle("Distribution des résidus du modèle BH1 en fonction du log débit minimum d'août à septembre")+theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Log du minimum d'août à septembre")

cor(lminE,Residus,method = "pearson")

