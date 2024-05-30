F1 = read.csv("ESM320.csv", header = TRUE)
F2 = read.csv("ESM480.csv", header = TRUE)
F3 = read.csv("ESM640.csv", header = TRUE)
F4 = read.csv("ESM1280.csv", header = TRUE)
F5 = read.csv("ProtT5_Full.csv", header = TRUE)
F6 = read.csv("ProtT5_BFD.csv", header = TRUE)

Cdes = data.frame(F1[,-1])
D = data.frame(Cdes, Activity = Class[,3])
D = remove_empty(D, "cols")
D$Activity  <- as.factor(D$Activity)
Dtr = D[1:796, ]
Dts = D[797:nrow(D), ]
m <- SMOTE(x = Dtr[, -ncol(Dtr)], y = Dtr[, ncol(Dtr)])
Dtrnew = data.frame(m$x_new, Activity = m$y_new)
Dnew = rbind(Dtrnew,Dts ) 
Dnew3= remove_empty(Dnew, "cols")
pca = prcomp(Dnew3[, -ncol(Dnew3)])
Dpca = data.frame(pca$x, Activity = Dnew3[, ncol(Dnew3)])
Dtr_PCA = Dpca[1:1356, ]
Dts_PCA = Dpca[1357:nrow(Dpca), ]
