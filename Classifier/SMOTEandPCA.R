Cdes = Descriptor
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
