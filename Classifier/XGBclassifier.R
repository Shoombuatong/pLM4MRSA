internal = read.csv("Dtr.csv", header = TRUE)
external = read.csv("Dts.csv", header = TRUE)
internal = internal[,-1]
external = external[,-1]
prediction <- data.frame()
testsetCopy <- data.frame()
set.seed(123)

for (h in 1:k){
train <- subset(internal, id !=   c(h))
test <-  subset(internal, id  ==  c(h))
M <- train(Activity ~ ., data = train, method = "xgbTree", trControl = cctrl, tuneGrid = GridXGB, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(internal)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)
F1CV = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])

M <- train(Activity ~ ., data = internal, method = "xgbTree", trControl = cctrl, tuneGrid = GridXGB, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, external)
predprob <- predict(M, external, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(external[,ncol(external)])))$auc
Dat <-  table(data.frame( pred3, external$Activity))
result <- data.frame( pred3, external$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(external)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))
F1IND = (2*Dat[1])/(2*Dat[1] + Dat[2]+ Dat[3])
