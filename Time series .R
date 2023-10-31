library(FactoMineR)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(aTSA)
library(tseries)
library(readr)
library(vars)
library(mFilter)
library(TSstudio)
library(forecast)
library(tidyverse)
library(urca)

data_indicateur <- read_excel("INSEA/3A/2nd part/banquire et monétaire/data_indicateur.xlsx")
attach(data_indicateur)
# <- read.table(file = "INSEA/3A/2nd part/banquire et monétaire/data_indicateur.xlsx", sep = "\t", header=TRUE)
View(data_indicateur)
data <- data_indicateur[, sapply(data, is.numeric)]

# Perform PCA
pca <- prcomp(data, scale = TRUE)
plot(pca)
summary(pca)
#
res <- PCA(data
           , ncp = Inf                 #number of dimensions kept in the results
           , scale.unit = TRUE        #if TRUE then data are scaled to unit variance
           #           , quanti.sup = c(2:3)     #a vector indicating the indexes of the quantitative supplementary variables
           , graph = TRUE
           , axes=c(1,2))
res <- na.omit(res)
# Matrix with eigenvalues
Eigenvalue <- as.data.table(res$eig)
Eigenvalue

# Correlations between variables and PCs
Correlation <- as.data.table(res$var$cor)
Correlation

res.PCA <- data %>% 
  cbind(res$ind$coord)


#Contribution (%) of each variable to each Dimension
Contribution <- as.data.table(res$var$contrib)
Contribution
# Dimensions
dimdesc(res, axes=c(1:2))

# Coordenates by individuals
Coordenates <- as.data.table(res$var$coord)
Coordenates

#### EXPORT PCA RESULTS
path2data<-file.path("c:","Users","PC","Documents","INSEA","3A","2nd part","banquire et monétaire")
fwrite(res.PCA
       , file = file.path(path2data, "res.PCA.xls")
       , quote = FALSE
       , sep = "\t"
       , row.names = FALSE)

fwrite(Eigenvalue
       , file = file.path(path2data, "PCA_Eigenvalue.xls")
       , quote = FALSE
       , sep = "\t"
       , row.names = FALSE)

fwrite(Correlation
       , file = file.path(path2data, "PCA_Correlation.xls")
       , quote = FALSE
       , sep = "\t"
       , row.names = FALSE)

fwrite(Coordenates
       , file = file.path(path2data, "PCA_Coordenates.xls")
       , quote = FALSE
       , sep = "\t"
       , row.names = FALSE)

fwrite(Contribution
       , file = file.path(path2data, "PCA_Contribution.xls")
       , quote = FALSE
       , sep = "\t"
       , row.names = FALSE)
data1<-read_excel("INSEA/3A/2nd part/banquire et monétaire/data_manadir.xlsx")
attach(data1)
# <- read.table(file = "INSEA/3A/2nd part/banquire et monétaire/data_indicateur.xlsx", sep = "\t", header=TRUE)
data1 <- data1[, sapply(data1, is.numeric)]
View(data1)
#modèle VAR
#conversion time serie 
PIB<- ts(data1$PIB, start = c(2010,1), frequency= 1)
ts.plot(PIB)
DEP<- ts(data1$DEP, start = c(2010,1), frequency= 1)
ts.plot(DEP)
INF<- ts(data1$INF, start = c(2010,1), frequency= 1)
ts.plot(INF)
POP<- ts(data1$POP, start = c(2010,1), frequency= 1)
ts.plot(POP)
CH<- ts(data1$CH, start = c(2010,1), frequency= 1)
ts.plot(CH)
OUV<- ts(data1$OUV, start = c(2010,1), frequency= 1)
ts.plot(OUV)
Dim1<- ts(data1$dim1, start = c(2010,1), frequency= 1)
ts.plot(Dim1)

#stationarité
stationary.test(INF, method="adf", nlag = 1 )
stationary.test(PIB, method="adf", nlag = 1 )
stationary.test(DEP, method="adf", nlag = 1 )
stationary.test(POP, method="adf", nlag = 1 )
stationary.test(CH, method="adf", nlag = 1 )
stationary.test(OUV, method="adf", nlag = 1 )
stationary.test(Dim1, method="adf", nlag = 1 )
#logérenciation 
lINF <- log(INF)
lPIB <- log(PIB)
lDEP <- log(DEP)
lPOP <- log(POP)
lCH <- log(CH)
lOUV <- log(OUV)
lDim1 <- log(Dim1)

# creation modele VAR
var<- cbind(lDim1,lOUV,lDEP,lPIB)
colnames(var)<-cbind("lDim1", "lOUV","lDEP","lPIB")
# ordre optimale:
#var<-abs(var)
lagselect<- VARselect(var, lag.max=2, type= "trend")
lagselect$selection
#modelisation;estimation des para
model<-model<-VAR(var)
summary(model)
# validation du modele:
# les racines unitaires
root.comp<- Im(roots(model,modules= FALSE))
root.real<- Re(roots(model,modules= FALSE))
# cercle unitaire
x= seq(-1,1,lenght= 1000) # intervale -1;1
y1=sqrt(1-(x^2))
y2=-sqrt(1-(x^2))
plot(c(x,x),c(y1,y2),xlab="partie real",ylab="partie imagnaire", type="l",
     main="cercle uniataire",ylim=c(-2,2),xlim=c(-2,2))
abline(h=0)
abline(v=0)
points(room.comp,room.real,pch=20)
legend(-1.5,-1,legend = "valeur propre",pch=20)
# teste d'autocorrelation
autocorr<-serial.test(model,lags.pt=5, type="PT.asymptotic")
autocorr
#test d'h?t?rosc?dasticit?
hetero<- arch.test(model)
hetero
#SIMULATION D'impact
ipcirf<- irf(model,impulse="lPIB",reponse=c("lPIB","lOUV","lDEP","lDim1"),n.ahead=20)
plot(ipcirf)
ipcirf1<- irf(model,impulse="dipc",reponse=c("TCER"),n.ahead=20)
plot(ipcirf1)