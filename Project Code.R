rm(list=ls())

library(readxl)
library(rpart)
library(rpart.plot)
library(zoo)
library(tree)
library(class)
library(stats)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(plotly)
library(dplyr)
library(heatmaply)
library(caret)
library(purrr)
library(tidyr)
library(reshape2)

######FUNCTIONS - CONFUSION MATRIX AND MODELS######

confmat<- function(table){
  tp = table[4]
  fn = table[3]
  fp = table[2]
  tn = table[1]
  total_recs<- sum(tp, tn, fn, fp)
  recall_rate<- round(tp/(tp+fn),5)
  precision_rate<- round(tp/(tp+fp),5)
  accuracy_rate<- round((tp+tn)/total_recs,5)
  misclassification_rate<- round((fp+fn)/total_recs,5)
  lis<- c(as.numeric(recall_rate), as.numeric(precision_rate), as.numeric(accuracy_rate), as.numeric(misclassification_rate))
  return(lis)
}

models<- function(train, mort){
  summstats<- as.data.frame(matrix(c(NA), ncol=5))
  name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
  names(summstats)<- name
  #decision trees (classification) - rpart
  mo<- rpart(train$ABOVE25_MORT ~., data=train, method="class")
  
  dcpredict<- predict(mo, newdata = mort, type="class")
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  stats<- as.data.frame(matrix(c("Decision Tree", mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  summstats<- summstats[complete.cases(summstats),]

  #pruning
  minval<- mo$cptable[which.min(mo$cptable[,"xerror"]),"xerror"]
  vals<- mo$cptable[mo$cptable[,"xerror"]==minval,]
  if (class(vals) != "numeric"){
    min_cp<- min(vals[, "CP"])
  }
  else{
    min_cp<- mo$cptable[which.min(mo$cptable[,"xerror"]), "CP"]
  }
  tree_prune<- prune(mo, cp =min_cp)
  
  dcpredict<- predict(tree_prune, newdata = mort, type=c("class"))
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  dc_prune<- paste("Decision Tree (Pruned, CP = ", round(min_cp, digits=5), ")", sep="", collapse=NULL)
  
  stats<- as.data.frame(matrix(c(dc_prune,  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)

  #stopping criteria: minsplit=5
  mo_stop<- rpart(train$ABOVE25_MORT ~., data=train, method="class", control = rpart.control(minsplit=5))
  
  dcpredict<- predict(mo_stop, newdata = mort, type=c("class"))
  dctab<- table(dcpredict, mort[, "ABOVE25_MORT"])
  
  mat<- confmat(dctab)
  stats<- as.data.frame(matrix(c("Decision Tree (Minsplit = 5)",  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  
  #KNN
  
  knnstats<- as.data.frame(matrix(c(NA), ncol=5))
  name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
  names(knnstats)<- name
  
  for (i in 1:10){
    knnmodel<- knn(as.data.frame(train[,-which(names(train) %in% drop2)]),
                   as.data.frame(mort[,-which(names(mort) %in% drop2)]), train$ABOVE25_MORT, k=i)
    
    knntab <- table(knnmodel,mort$ABOVE25_MORT)
    
    #print(str(i))
    mat<- confmat(knntab)
    knntype<- paste("KNN (", i, ")", sep="", collapse=NULL)
    
    stats<- as.data.frame(matrix(c(knntype, mat[1], mat[2], 
                                   mat[3], mat[4]), ncol=5))
    
    names(stats)<- name
    knnstats<- rbind(knnstats, stats)
  }
  
  max_acc_knn<- knnstats[order(knnstats$Accuracy, decreasing=T),]
  max_acc_knn<- max_acc_knn[complete.cases(max_acc_knn),]
  
  summstats<- rbind(summstats, max_acc_knn[1,])

  #random forest
  require(randomForest)
  train$ABOVE25_MORT<- as.factor(train$ABOVE25_MORT)
  
  rand_train<- randomForest(train$ABOVE25_MORT ~., data=train, na.action = na.omit, importance=TRUE)
  predTrain<- predict(rand_train, mort, type="class")
  randtab<- table(predTrain, mort$ABOVE25_MORT)
  
  mat<- confmat(randtab)
  
  stats<- as.data.frame(matrix(c("Random Forest (ntree=500)",  mat[1], mat[2], 
                                 mat[3], mat[4]), ncol=5))
  
  names(stats)<- name
  summstats<- rbind(summstats, stats)
  
  
  
  lis<- list("Statistics" = summstats, "DecTree"= mo, "Pruned DecTree"= tree_prune, "Stopped DecTree"= mo_stop,
             "RF"= rand_train, "KNN stats"= max_acc_knn)
  return(lis)
  
}

#####DATA UPLOAD#####
mort<- data.frame(read_xlsx("C:/Users/phoen/Downloads/2017 Under-Five Mortality.xlsx"))
#View(mort)

for (i in 3:ncol(mort)-1){
  mort[,i]<- as.numeric(mort[,i])
}
summary(mort)
track<- data.frame("Name" = NA, "NAs" = NA)
mort[, "POVERTY"]<- sub("-", NA, mort[,"POVERTY"])
mort<- mort[is.na(mort$UNDER5_MORT) == FALSE,]

mort<- mort[, which(colMeans(!is.na(mort)) > 0.5)]
for (i in 1:ncol(mort)){
  track1<- data.frame("Name" = names(mort)[i], 
                      "NAs" =sum(!complete.cases(mort[,i])))
  track<- rbind(track, track1)

}

track<- track[complete.cases(track),]
track["Percent"]<- track[, "NAs"]/nrow(mort)
track<- track[order(track$Percent, decreasing=TRUE),]
track

mort_rate<- mort[,'UNDER5_MORT']
print(mean(mort_rate))
mort[, 'ABOVE25_MORT']<- 0
mort[which(mort$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

summary(mort_rate)
lis<- c("Indicator")
mort<- mort[,-which(names(mort) %in% lis)]

change<- function(x){
  train<- x
  for (i in 1:ncol(train)){
    train[, i]<- sub("-", NA, train[,i])
    train[,i]<- as.numeric(train[,i])
  }
  return(train)
}

#import of training data
t2016<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=1))
t2015<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=3))
t2014<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=4))
t2013<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=5))
t2012<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=6))
t2011<- data.frame(read_xlsx("C:/Users/phoen/Downloads/GLOBAL_DATAFLOW_2011-2016.xlsx", sheet=7))

t2016<- change(t2016)
t2016<- t2016[is.na(t2016$UNDER5_MORT) == FALSE,]
t2016<- t2016[, which(colMeans(!is.na(t2016)) > 0.5)]
t2016<- na.aggregate(t2016)

t2015<- change(t2015)
t2015<- t2015[is.na(t2015$UNDER5_MORT) == FALSE,]
t2015<- t2015[, which(colMeans(!is.na(t2015)) > 0.5)]
t2015<- na.aggregate(t2015)

t2014<- change(t2014)
t2014<- t2014[is.na(t2014$UNDER5_MORT) == FALSE,]
t2014<- t2014[, which(colMeans(!is.na(t2014)) > 0.5)]
t2014<- na.aggregate(t2014)

t2013<- change(t2013)
t2013<- t2013[is.na(t2013$UNDER5_MORT) == FALSE,]
t2013<- t2013[, which(colMeans(!is.na(t2013)) > 0.5)]
t2013<- na.aggregate(t2013)

t2012<- change(t2012)
t2012<- t2012[is.na(t2012$UNDER5_MORT) == FALSE,]
t2012<- t2012[, which(colMeans(!is.na(t2012)) > 0.5)]
t2012<- na.aggregate(t2012)

t2011<- change(t2011)
t2011<- t2011[is.na(t2011$UNDER5_MORT) == FALSE,]
t2011<- t2011[, which(colMeans(!is.na(t2011)) > 0.5)]
t2011<- na.aggregate(t2011)

train<- rbind(t2016, rbind(t2015, rbind(t2014, rbind(t2013, rbind(t2012, t2011)))))
names(train)
train<- train[,which(names(train) %in% names(mort))]
mort<- mort[,which(names(mort) %in% names(train))]

mort[, 'ABOVE25_MORT']<- 0
mort[which(mort$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1

train[, 'ABOVE25_MORT']<- 0
train[which(train$UNDER5_MORT>= 25), 'ABOVE25_MORT'] <- 1
summary(train)

summstats<- as.data.frame(matrix(c(NA), ncol=5))
name<- c("Model", "Recall", "Precision", "Accuracy", "Misclassification")
names(summstats)<- name

drop<- c("UNDER5_MORT")
drop2<- c("ABOVE25_MORT")
u5_train<- as.data.frame(train[,"UNDER5_MORT"])
u5_mort<- as.data.frame(mort[,"UNDER5_MORT"])
names(u5_train)<- c("UNDER5_MORT")
names(u5_mort)<- c("UNDER5_MORT")
train<- train[,-which(names(train) %in% drop)]
mort<- mort[,-which(names(mort) %in% drop)]

for (i in 1:ncol(train)){
  train[is.na(train[,i]),i] <- mean(train[,i], na.rm=TRUE)
  mort[is.na(mort[,i]),i] <- mean(mort[,i], na.rm=TRUE)
}

train_u5<- cbind(train, u5_train)
mort_u5<- cbind(mort, u5_mort)

total<- rbind(train_u5, mort_u5)

####EDA#####
#Examine under 5 mortality rate
hist(total$UNDER5_MORT, main="Histogram of Mortality Rate",
     xlab = "Mortality Rate")

total$ABOVE25_MORT<- as.factor(total$ABOVE25_MORT)

#see how hygenic factors are different between low and high mortality countries
summary(total)
str(total)

boxplot(AT_LEAST_BASIC_SANIT~ABOVE25_MORT,total, col="pink")
boxplot(AT_LEAST_BASIC_DRINK~ABOVE25_MORT,total, col="lightblue")

ggplot(total, aes(x=ABOVE25_MORT, y=AT_LEAST_BASIC_SANIT))+
  geom_violin(alpha=.2)

ggplot(total, aes(x=ABOVE25_MORT, y=AT_LEAST_BASIC_DRINK))+
  geom_violin(alpha=.2)

dropthis<- c("ABOVE25_MORT")

t1<- total[, -which(names(total) %in% dropthis)]
corr<- as.data.frame(cor(t1))
heatmaply_cor(corr)


#####BASE MODEL #######
set.seed(2)
base_list= models(train, mort)
basestats <- base_list$Statistics
basestats
write.csv(basestats, "c:/Users/phoen/Dropbox/MGMT 6962/Project Results/basestats.csv")

base_tree<- base_list$DecTree
rpart.plot(base_tree)

pruned<- base_list$`Pruned DecTree`
rpart.plot(pruned)

stopped<- base_list$`Stopped DecTree`
rpart.plot(stopped)

####GINI AND ACCURACY DATAFRAME REDUCTION#####
dev.off()
par(mar= c(4,15,1,1),las=1, cex.axis = .7)

#view importance by mean gini decrease variables
rand_train<- base_list$RF
vargini<- importance(rand_train, sort=FALSE)[,"MeanDecreaseGini"]
dat<- vargini[order(vargini, decreasing=FALSE)]
barplot(dat, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Gini", col=blues9)

#reduce dataframe
vargini1<- as.data.frame(importance(rand_train, sort=FALSE)[,"MeanDecreaseGini"])
n<- as.data.frame(row.names(vargini1))
vargini1<- cbind(n, vargini1)
new_name<- c("Var", "MeanDecreaseGini")
names(vargini1)<- new_name
row.names(vargini1) = NULL
dat_new<- vargini1[order(vargini1$MeanDecreaseGini, decreasing = T),]

gini_vars<- dat_new$Var[1:5]
gini_vars<- gini_vars[!is.na(gini_vars)]
#view importance by Mean Decrease Accuracy variables
varacc<- importance(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"]
dat1<- varacc[order(varacc, decreasing=FALSE)]
barplot(dat1, horiz=TRUE, main="Random Forest Importance", xlab="Mean Decrease Accuracy", col=blues9)

varacc1<- as.data.frame(importance(rand_train, sort=FALSE)[,"MeanDecreaseAccuracy"])
n<- as.data.frame(row.names(varacc1))
varacc1<- cbind(n, varacc1)
new_name<- c("Var", "MeanDecreaseAccuracy")
names(varacc1)<- new_name
row.names(varacc1) = NULL
dat_new1<- varacc1[order(varacc1$MeanDecreaseAccuracy,decreasing = T),]

acc_vars<- dat_new1$Var[1:5]
acc_vars<- acc_vars[!is.na(acc_vars)]

dev.off()

######CLUSTERING######
set.seed(2)
predkmeans<- kmeans(total[,-which(names(total) %in% c("ABOVE25_MORT", "UNDER5_MORT"))], centers=2, iter.max = 1000)
print(table(total$ABOVE25_MORT, predkmeans$cluster))
#attacj cluster membership
total[,'Cluster']<- factor(predkmeans$cluster)

clust_mult_centers<- as.data.frame(predkmeans$centers)

clust_dist<- data.frame("Name" = NA, "Dist" = NA)
for (i in 1:dim(clust_mult_centers)[2]){
  dist1<- data.frame("Name" = names(clust_mult_centers)[i],
                      "Dist" = (clust_mult_centers[1, i] - clust_mult_centers[2, i])^2)
  #print(track1)
  clust_dist<- rbind(clust_dist, dist1)
}
#look at cluster distance
clust_dist<- clust_dist[complete.cases(clust_dist),]
clust_dist <- clust_dist[order(clust_dist$Dist, decreasing = TRUE), ]
clust_dist

ggplot() +
  geom_point(data = total,
             mapping = aes(x =IMPROVED_DRINK_PREMISES,
                           y = AT_LEAST_BASIC_SANIT,
                           colour = Cluster))+
  geom_point(data = clust_mult_centers, aes(x =IMPROVED_DRINK_PREMISES,
                      y = AT_LEAST_BASIC_SANIT,
                  color = 'Center'),size = 5)


ggplot() +
  geom_point(data = total,
             mapping = aes(x =IMPROVED_DRINK_PREMISES,
                           y = AT_LEAST_BASIC_SANIT,
                           colour = ABOVE25_MORT))
total$ABOVE25_MORT<- as.factor(total$ABOVE25_MORT)


ggplot(total, aes(x=IMPROVED_DRINK_PREMISES, y=AT_LEAST_BASIC_SANIT, fill=ABOVE25_MORT))+
  geom_violin(alpha=.2)

total$Cluster<- as.factor(total$Cluster)
clust1 <- total[total$Cluster == 1,]
clust2 <- total[total$Cluster == 2,]

boxplot(AT_LEAST_BASIC_SANIT~ABOVE25_MORT,total, col="pink" )
boxplot(IMPROVED_DRINK_PREMISES~ABOVE25_MORT,total, col="lightblue" )

######GINI INCREASE DATAFRAME#####

##gini_vars

#gini df
set.seed(2)
gini_train<- train[,c(gini_vars, "ABOVE25_MORT")]
gini_mort<- mort[, c(gini_vars, "ABOVE25_MORT")]

gini_list= models(gini_train, gini_mort)
ginistats <- gini_list$Statistics

write.csv(ginistats, "c:/Users/phoen/Dropbox/MGMT 6962/Project Results/ginistats.csv")

base_tree<- gini_list$DecTree
rpart.plot(base_tree)

pruned<- gini_list$`Pruned DecTree`
rpart.plot(pruned)

stopped<- gini_list$`Stopped DecTree`
rpart.plot(stopped)

###MEAN ACCRUACY DECREASE DATAFRAME######

##acc_vars
set.seed(2)
acc_train<- train[,c(acc_vars, "ABOVE25_MORT")]
acc_mort<- mort[, c(acc_vars, "ABOVE25_MORT")]

acc_list= models(acc_train, acc_mort)
accstats <- acc_list$Statistics

write.csv(accstats, "c:/Users/phoen/Dropbox/MGMT 6962/Project Results/accstats.csv")

base_tree<- acc_list$DecTree
rpart.plot(base_tree)

pruned<- acc_list$`Pruned DecTree`
rpart.plot(pruned)

stopped<- acc_list$`Stopped DecTree`
rpart.plot(stopped)



####Clustering after gini and accstats#####

total1<- total

#Gini variables clustering
total<-total1[, names(total1) %in% c(gini_vars, "ABOVE25_MORT", "UNDER5_MORT")]
set.seed(2)
predkmeans<- kmeans(total[,-which(names(total) %in% c("ABOVE25_MORT", "UNDER5_MORT"))], centers=2, iter.max = 1000)
print(table(total$ABOVE25_MORT, predkmeans$cluster))
total[,'Cluster']<- factor(predkmeans$cluster)

clust_mult_centers<- as.data.frame(predkmeans$centers)

clust_dist<- data.frame("Name" = NA, "Dist" = NA)
for (i in 1:dim(clust_mult_centers)[2]){
  dist1<- data.frame("Name" = names(clust_mult_centers)[i],
                     "Dist" = (clust_mult_centers[1, i] - clust_mult_centers[2, i])^2)
  #print(track1)
  clust_dist<- rbind(clust_dist, dist1)
}

clust_dist<- clust_dist[complete.cases(clust_dist),]
clust_dist <- clust_dist[order(clust_dist$Dist, decreasing = TRUE), ]
clust_dist

ggplot() +
  geom_point(data = total,
             mapping = aes(x =IMPROVED_DRINK_PREMISES,
                           y = AT_LEAST_BASIC_SANIT,
                           colour = Cluster))+
  geom_point(data = clust_mult_centers, aes(x =IMPROVED_DRINK_PREMISES,
                                            y = AT_LEAST_BASIC_SANIT,
                                            color = 'Center'),size = 5)


ggplot() +
  geom_point(data = total,
             mapping = aes(x =IMPROVED_DRINK_PREMISES,
                           y = AT_LEAST_BASIC_SANIT,
                           colour = ABOVE25_MORT))
total$ABOVE25_MORT<- as.factor(total$ABOVE25_MORT)

ggplot(total, aes(x=IMPROVED_DRINK_PREMISES, y=AT_LEAST_BASIC_SANIT, fill=ABOVE25_MORT))+
  geom_violin(alpha=.2)

total$Cluster<- as.factor(total$Cluster)
clust1 <- total[total$Cluster == 1,]
clust2 <- total[total$Cluster == 2,]

boxplot(AT_LEAST_BASIC_SANIT~ABOVE25_MORT,total, col="pink" )
boxplot(IMPROVED_DRINK_PREMISES~ABOVE25_MORT,total, col="lightblue" )

hist(clust1$UNDER5_MORT)
hist(clust2$UNDER5_MORT)

## accuracy variables clustering

total<-total1[, names(total1) %in% c(acc_vars, "ABOVE25_MORT", "UNDER5_MORT")]
set.seed(2)
predkmeans<- kmeans(total[,-which(names(total) %in% c("ABOVE25_MORT", "UNDER5_MORT"))], centers=2, iter.max = 1000)
print(table(total$ABOVE25_MORT, predkmeans$cluster))
total[,'Cluster']<- factor(predkmeans$cluster)

clust_mult_centers<- as.data.frame(predkmeans$centers)

clust_dist<- data.frame("Name" = NA, "Dist" = NA)
for (i in 1:dim(clust_mult_centers)[2]){
  dist1<- data.frame("Name" = names(clust_mult_centers)[i],
                     "Dist" = (clust_mult_centers[1, i] - clust_mult_centers[2, i])^2)
  #print(track1)
  clust_dist<- rbind(clust_dist, dist1)
}

clust_dist<- clust_dist[complete.cases(clust_dist),]
clust_dist <- clust_dist[order(clust_dist$Dist, decreasing = TRUE), ]
clust_dist

ggplot() +
  geom_point(data = total,
             mapping = aes(x =IMPROVED_DRINK_PREMISES,
                           y = AT_LEAST_BASIC_SANIT,
                           colour = Cluster))+
  geom_point(data = clust_mult_centers, aes(x =IMPROVED_DRINK_PREMISES,
                                            y = AT_LEAST_BASIC_SANIT,
                                            color = 'Center'),size = 5)


ggplot() +
  geom_point(data = total,
             mapping = aes(x =IMPROVED_DRINK_PREMISES,
                           y = AT_LEAST_BASIC_SANIT,
                           colour = ABOVE25_MORT))
total$ABOVE25_MORT<- as.factor(total$ABOVE25_MORT)

ggplot(total, aes(x=IMPROVED_DRINK_PREMISES, y=AT_LEAST_BASIC_SANIT, fill=ABOVE25_MORT))+
  geom_violin(alpha=.2)

total$Cluster<- as.factor(total$Cluster)
clust1 <- total[total$Cluster == 1,]
clust2 <- total[total$Cluster == 2,]

boxplot(AT_LEAST_BASIC_SANIT~ABOVE25_MORT,total, col="pink" )
boxplot(IMPROVED_DRINK_PREMISES~ABOVE25_MORT,total, col="lightblue" )

hist(clust1$UNDER5_MORT)
hist(clust2$UNDER5_MORT)

#####FINAL ANALYSIS GRAPHS#####

total<- rbind(train, mort)

all_vars<- union(gini_vars, acc_vars)

total_imp_vars<- total[, c(all_vars, "ABOVE25_MORT")]
tot_all <- melt(total_imp_vars, id = "ABOVE25_MORT")   

dev.off()

tot_all$ABOVE25_MORT<- as.factor((tot_all$ABOVE25_MORT))

ggplot(tot_all, aes(x = variable, y = value, color = ABOVE25_MORT)) +  # ggplot function
  geom_boxplot()+theme(axis.text.x=element_text(angle=50))

ggplot(tot_all, aes(x = variable, y = value, color = ABOVE25_MORT)) +  # ggplot function
    geom_boxplot()+
  facet_wrap(~ variable, scales = "free")
  

