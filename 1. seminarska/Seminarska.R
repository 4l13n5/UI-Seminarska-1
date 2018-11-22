library(CORElearn)
team.stats <- function(name,tab,until){
  t<-tab[1:until,]
  stats<-c(sum(t$HPTS[t$HOME==name])+sum(t$APTS[t$AWAY==name]),
           sum(t$H2PM[t$HOME==name])+sum(t$A2PM[t$AWAY==name]),
           sum(t$H2PA[t$HOME==name])+sum(t$A2PA[t$AWAY==name]),
           sum(t$H3PM[t$HOME==name])+sum(t$A3PM[t$AWAY==name]),
           sum(t$H3PA[t$HOME==name])+sum(t$A3PA[t$AWAY==name]),
           sum(t$HFTM[t$HOME==name])+sum(t$AFTM[t$AWAY==name]),
           sum(t$HFTA[t$HOME==name])+sum(t$AFTA[t$AWAY==name]),
           sum(t$HORB[t$HOME==name])+sum(t$AORB[t$AWAY==name]),
           sum(t$HDRB[t$HOME==name])+sum(t$ADRB[t$AWAY==name]),
           sum(t$HAST[t$HOME==name])+sum(t$AAST[t$AWAY==name]),
           sum(t$HSTL[t$HOME==name])+sum(t$ASTL[t$AWAY==name]),
           sum(t$HTOV[t$HOME==name])+sum(t$ATOV[t$AWAY==name]),
           sum(t$HBLK[t$HOME==name])+sum(t$ABLK[t$AWAY==name]),
           sum(t$HPF[t$HOME==name])+sum(t$APF[t$AWAY==name]))
  stats<-stats/(sum(nrow(t[t$HOME==name,])+nrow(t[t$AWAY==name,])))
  return(stats)
}

mk.data <- function(tab){
  for(i in nrow(tab):1){
    row <- tab[i,]
    home<-team.stats(row[[3]],tab,i)
    away<-team.stats(row[[18]],tab,i)
    for(j in 1:14){
      row[[j+3]]<-home[[j]]
      row[[j+18]]<-away[[j]]
    }
    tab[i,]<-row
  }
  return(tab)
}
data <- read.table("regular.txt", sep=",",header=T)
data$WIN<-NA
data$WIN[data$HPTS > data$APTS] <- 1
data$WIN[data$HPTS < data$APTS] <- 0
data$WIN<-as.factor(data$WIN)

set<-mk.data(data)



set$H2PM<-set$H2PM/set$H2PA
set$H3PM<-set$H3PM/set$H3PA
set$HFTM<-set$HFTM/set$HFTA
set$A2PM<-set$A2PM/set$A2PA
set$A3PM<-set$A3PM/set$A3PA
set$AFTM<-set$AFTM/set$AFTA
set$H2PA<-NULL
set$H3PA<-NULL
set$HFTA<-NULL
set$A2PA<-NULL
set$A3PA<-NULL
set$AFTA<-NULL
set$SEASON<-NULL
set$DATE<-NULL
set$HOME<-NULL
set$AWAY<-NULL


for(i in 1:11){
  set[[i]]<-set[[i]]/set[[i+11]]
}
for(i in 12:22){
  set[[12]]<-NULL
}

#razdeli na uèno in testno množico
n<-nrow(set)*0.8

learn <- head(set,n)
test <- tail(set,(nrow(set)-n))

maj.class <- which.max(table(test$WIN))
ca.vals <- table(test$WIN)[maj.class]/nrow(test)

modelGAUSSIAN<- CoreModel(WIN ~ ., learn, model="knnKernel")
modelBAYES<- CoreModel(WIN ~ ., learn, model="bayes")
modelKNN<- CoreModel(WIN ~ ., learn, model="knn")
modelRF<-CoreModel(WIN ~ ., learn, model="rf")
modelTREE <- CoreModel(WIN ~ ., learn, model="tree")

predicted1<-predict.CoreModel(modelGAUSSIAN,test,type="class")
predicted2<-predict.CoreModel(modelBAYES,test,type="class")
predicted3<-predict.CoreModel(modelKNN,test,type="class")
predicted4 <- predict.CoreModel(modelTREE, test, type="class")
predicted5 <- predict.CoreModel(modelRF,test,type="class")
observed<-tail(data$WIN,(nrow(data)-n))

tab1 <- table(observed, predicted1)
tab2 <- table(observed, predicted2)
tab3 <- table(observed, predicted3)
tab4 <- table(observed, predicted4)
tab5 <- table(observed,predicted5)

ca.vals <- c(ca.vals, sum(diag(tab1))/sum(tab1),sum(diag(tab2))/sum(tab2),sum(diag(tab3))/sum(tab3),sum(diag(tab4))/sum(tab4),sum(diag(tab5))/sum(tab5))

barplot(ca.vals, xlab="models", ylab="Classification accuracy", main="Results")
