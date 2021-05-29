###VERÝYÝ ANLAMA

library(readr)
prc <- read_csv("~/R/kanser.csv")
View(prc)
stringsAsFactors = FALSE    #Stringi factore çeviriyor
str(prc)         #Verinin yapýlandýrýlmýþ olup olmadýðýný kontrol etmek için
prc <- prc [-1]       #gereksiz sütunlarý kaldý?ma
prc <- prc [-1]
prc$diagnosis <- factor(prc$diagnosis, levels = c("B","M"), labels = c("Iyi Huylu","Kotu Huylu"))
table(prc$diagnosis)     # iyi huylu ve kötü huyludan kaç tane oldugunu sorduk
round(prop.table(table(prc$diagnosis)) * 100, digits = 1) #Y??zdelik olarak hesabý

##############################################################################################################


###VERÝYÝ HAZIRLAMA

#Eksik Deðer Sorulama
is.na(prc$diagnosis) 

#Dummy
table(prc$diagnosis )  
dummy_tani <- as.data.fr?me(model.matrix(~ 0 + diagnosis , data = prc))
dummy <- cbind(prc$diagnosis , dummy_tani)
head(dummy)    #Dummy verilerimizdeki Ýyi huylu ve Kötü huylu deðiþkenlerini 0 , 1 olarak yazdýrýr

#Normalize

normalize <- function(x) { return ((x - min(x)) / (max?x) - min(x))) }
prc_n <- as.data.frame(lapply(prc[2:31], normalize))
summary(prc_n$radius_mean)       ##RADÝUS_MEAN tablosunun özetini alýr
summary(prc_n)       ##normalize deðerlerinin özetini verir
prc_train <- prc_n[1:350,]             ## knn modeli içi? test tablosu ve öðrenme tablosu oluþturma
prc_test <- prc_n[351:569,]
prc_train_labels <- prc[1:350,1]
prc_test_labels <- prc[351:569,1] 

#En çok tekrar eden veri

prc2 <- table(prc$diagnosis)
encok_deger <- names(prc2[which.max(prc2)]) 
encok_deger #Ýyi?huylunun listede daha çok olduðunu gösteriyor

summary(prc$area_mean) ##AREA_MEAN tablosunun özetini alýr

#Grafikler

pairs(~perimeter_mean + radius_mean + texture_mean, data=prc )

{uzaklik <- prc$radius_mean
  hist(uzaklik, col="blue",main = "En cok goz?ken tumor boyutu")} #veri setimizde en çok gözüken tümör boyutu 10 ila 15 arasýdýr


boxplot(radius_mean~perimeter_mean,data=prc,xlab="merkezden çevredeki noktalara uzaklik ortalama",  #Tümörlerin hangi uzaklýkta ortalama ne boyutta olduðunu gösterir
     ?  ylab = "tumorunun ortalama boyutu" , main="tumor ile ort. noktalara uzaklik")    #veri setimize göre merkezden uzakta olan tümörün boyutu daha büyüktür


####################################################################################################?########

###MODELLEME
#1. Modelleme KNN algoritmasý

install.packages("class")
library(class)
set.seed(1)
prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=24)  # ???569 = 23,85 ??? 24 
head(prc_test_pred)                                                                   # k deðeri veri setindeki gözlem sayýsýnýn kare kökü olarak belirledik
head(prc_test)

(my_table <- table(prc_test_pred, prc_test[[30]], dnn = c("Tahminler", "Gerçek?Referans")))


ornek_veri <- function(){
  set.seed(1)
  test <- sample(c("Kotu Huylu", "Iyi Huylu"), 569, replace = TRUE)
  set.seed(2)
  diagnosis <- sample(c("Kotu Huylu", "Iyi Huylu"), 569, replace = TRUE)
  
  test <- as.factor(test)
  diagnosis <- as?factor(diagnosis)
  data2 <- data.frame(test, diagnosis)
  return(data2)
}

veri2 <- ornek_veri()
head(veri2$test)
head(veri2$diagnosis)



plot(x=1:length(prc_test_pred), y = prc_test_pred, type = "o", col = "blue", xlab = "k", ylab = "Accuracy",main = "P?rformance Evaluation")

grid(NA, 5, lwd = 2)

text(x=1:length(prc_train_labels), y = prc_train_labels,  round(prc_test$radius_mean,2), cex=1, pos=3,col="red")
summary(prc_test$radius_mean)



#2. Modelleme (Navibayes)

install.packages ("e1071")
library (e?071)
naiveB_model <- naiveBayes (prc [, 1: 2], prc [[1]])
naiveB_model

nb_predictions <- predict(naiveB_model, prc_test[,1:2])
nb_probs <- predict(naiveB_model, prc_test[,1:2], "raw")
results <- data.frame(prc_test[[1]], nb_predictions, nb_probs)
results
?#3.Modelleme
#Karar Aðacý
install.packages("caret")
library(caret)
set.seed(1)
egitimIndisleri <- createDataPartition(y = prc$diagnosis, p = .70, list = FALSE) 
egitim_C45 <- prc[egitimIndisleri,]

test_C45 <- prc[-egitimIndisleri,]


numeric_coloumns<-c(2?32)
for(i in 1:ncol(prc)){
  if(i %in%numeric_coloumns)
    prc[,i]<-as.numeric(prc[,i])
  else
    prc[,i]<- as.factor(prc[,i])
}
summary(prc)

install.packages("RWeka")
library(RWeka)
C45_modeli <- J48(diagnosis ~., data=egitim_C45)
#C45_modeli <- J48(pr? ~., data=egitim_C45, control = Weka_control(R = true, M = 6))  #Control etmek için
print(summary(C45_modeli))
print(C45_modeli)

# Niteliklerin önem derecesi

install.packages("FSelector")
library(FSelector)
information.gain(diagnosis ~., data=egitim_C45)?gain.ratio(diagnosis ~., data=egitim_C45)

install.packages("partykit")
library(partykit)
plot(C45_modeli)



###########################################################################################################

###PERFORMANS DEÐERLENDÝRME

tahminle?<-predict(C45_modeli, newdata = test_C45[,-1])

confusionMatrix(tahminler, test_C45$diagnosis)

i <- 5
(yeniveri_tahmini <- predict(C45_modeli, newdata = prc[i,-1]))
prc[i,1]

myConfMatrix <- table(veri2$diagnosis, prc$diagnosis, dnn = c("Model Tahminleri"? "Gerçek/Referans"))
myConfMatrix


dp <- myConfMatrix[1]
dp
yp <- myConfMatrix[3]
yp
yn<- myConfMatrix[2]
yn
dn <- myConfMatrix[4]
dn

dogruluk_orani <- (dp+dn)/(dp+yp+yn+dn)
dogruluk_orani
hata_orani <- 1 - dogruluk_orani
hata_orani

duyarlilik <- dp/(dp?yn)
duyarlilik
belirleyicilik <- dn/(dn+yp)
belirleyicilik

FNR <- 1 - duyarlilik
FNR
FPR <- 1 - belirleyicilik
FPR
PPV <- dp/(dp+yp)
PPV
NPV <- dn/(dn+yn)
NPV

FMeasure <- (2*duyarlilik*PPV)/(duyarlilik+PPV)
FMeasure

#caret::confusionMatrix(data = veri2$?iagnosis, reference = prc$diagnosis)
caret::confusionMatrix(data = veri2$diagnosis, reference = prc$diagnosis, mode = "everything")

###Üç yollu bölme
#install.packages("caret")
library(caret)
set.seed(1)
my_indexes1 <- caret::createDataPartition(y = prc$d?agnosis, times = 1, p = .80, list = F)

training_ <- as.data.frame(prc[my_indexes1,])
test <- as.data.frame(prc[-my_indexes1,])

set.seed(1)
my_indexes2 <- caret::createDataPartition(y = training_$radius_mean, times = 1, p = .80, list = F)

training <- as.?ata.frame(training_[my_indexes2,])
validation <- as.data.frame(training_[-my_indexes2,])

table(prc$diagnosis)
table(training$diagnosis)
table(validation$diagnosis)
table(test$diagnosis)






