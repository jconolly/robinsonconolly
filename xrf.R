library(MASS)
library(ggfortify)
library(MuMIn)
xrf <- read.table("xrf.csv", header=T, sep=",")
xrf$Period <- factor(xrf$Period)
xrf$Region <- factor(xrf$Region)
xrf$Site <- factor(xrf$Site)
#PCA
pca_res <- prcomp(xrf[5:20], scale. = TRUE)
autoplot(pca_res, data = xrf, colour = 'Period')
#GLM
model <- glm(Period ~ Ti + Mn + Fe + Ni + Cu + Zn + Rb + Sr + Y + Zr + Nb + Ba + Pb + CaO + TiO2 + MnO2, data = xrf, family=binomial(logit), maxit=100, na.action = "na.fail")
model.dredge <- dredge(model)
model.dredge
# Model 19 has the lowest df (8) 
model.19 <- glm(Period ~ Ba + CaO + Fe + Mn + MnO2 + Y + Zn, data = xrf, family=binomial(logit), maxit=100, na.action = "na.fail")
summary(model.19)
# All terms significant
# Does Region generate for significant coefficients?
model.19 <- glm(Region ~ Ba + CaO + Fe + Mn + MnO2 + Y + Zn, data = xrf, family=binomial(logit), maxit=100, na.action = "na.fail")
summary(model.19)
# Only for BA and Ca0 -> consider dropping these? # NO (breaks the model)
# Final PCA on Reduced Dataset
pca_res <- prcomp(xrf[,c(16,18,7,6,20,13,10)], scale. = TRUE)
autoplot(pca_res, data = xrf, colour = 'Period')
pca_res <- prcomp(xrf[,c(16,18,7,6,20,13,10)], scale. = TRUE)
autoplot(pca_res, data = xrf, colour = 'Region')
## PLOTS FOR PUBLICATION
# Scree
plot(c(1:7),pca_res$sdev^2, xlab="Principal component", ylab="Eigenvalue")
lines(c(1:7),pca_res$sdev^2)
# PCA by Period
plot(pca_res$x[,1], pca_res$x[,2], pch=c(21,19)[as.numeric(xrf$Period)], xlab="PC1 [30.9%]", ylab="PC2 [27.4%]")
legend("topleft", 
       legend = c("Middle Woodland", "Early Late Woodland"), 
       pch = c(21,19), 
       bty = "n", 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.0))
# PCA by Region
plot(pca_res$x[,1], pca_res$x[,2], pch=c(21,19)[as.factor(xrf$Region)], xlab="PC1 [30.9%]", ylab="PC2 [27.4%]")
legend("topleft", 
       legend = c("Pigeon Lake", "Rice Lake"), 
       pch = c(21,19), 
       bty = "n", 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.0))
# PCA by Site
plot(pca_res$x[,1], pca_res$x[,2], pch=c(1,17,20,3)[as.factor(xrf$Site)], xlab="PC1 [30.9%]", ylab="PC2 [27.4%]")
legend("topleft", 
        legend = c("CH-1", "JI-2", "Richardson", "Scott"), 
        pch = c(1,17,20,3), 
       bty = "n", 
        text.col = "black", 
        horiz = F , 
        inset = c(0.05, 0.0))
# Table of rotations
pca_res[2]$rotation[,1:2]        
## TESTS of PC1 Loadings
shapiro.test(pca_res[5]$x[,1][xrf$Period=="0"])
shapiro.test(pca_res[5]$x[,1][xrf$Period=="1"])
t.test(pca_res[5]$x[,1][xrf$Period=="0"], pca_res[5]$x[,1][xrf$Period=="1"])
# F-test for variance
var.test(pca_res[5]$x[,1][xrf$Period=="0"], pca_res[5]$x[,1][xrf$Period=="1"])

