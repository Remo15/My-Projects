#======================================================================= 
# 
# Factor Hair Analysis- Advanced Statistics - Predictive Modelling
# 
#======================================================================= 

# Setting the directory
getwd()
setwd("C:/2020/RProgramming/Advanced Statistics")
install.packages("psych")
install.packages("corrplot")

library(psych)
library(corrplot)


# Importing the data
Factor_Hair <- read.csv("Factor-Hair-Revised.csv")
View(Factor_Hair)

#Variable Identification
dim(Factor_Hair)
names(Factor_Hair)
str(Factor_Hair)
head(Factor_Hair)
tail(Factor_Hair)

# Removing the first variable-ID from dataset
Factor_Hair_1 <- Factor_Hair[,-1]
Factor_Hair_1
View(Factor_Hair_1)
names(Factor_Hair_1)

#Attaching the dataset
attach(Factor_Hair_1)

#Univariate Analysis using multi.hist
multi.hist(Factor_Hair_1,density=F,col="blue")

#Bivariate Analysis
par(mfrow = c(4,3))#Convert Plotting space
for (i in c(1:11)) {
  plot(Factor_Hair_1[,i],Satisfaction, 
       xlab = names(Factor_Hair_1[i]), ylab = NULL, col = "magenta", 
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
       xlim = c(0,10),ylim = c(0,10))
  abline(lm(formula = Satisfaction ~ Factor_Hair_1[,i]),col = "dark green")
}

#Outlier Identification - Boxplot
par(mfrow =c(4,5))#Convert Plotting space
boxplot(Factor_Hair_1[],las=2,main="Boxplot- Outlier Identification", col="navy blue")

boxplot.stats(Factor_Hair_1$Ecom)$out
boxplot.stats(Factor_Hair_1$SalesFImage)$out
boxplot.stats(Factor_Hair_1$OrdBilling)$out
boxplot.stats(Factor_Hair_1$DelSpeed)$out

# Checking for missing values
sum(is.na(Factor_Hair_1))

#Check for multi-collinearity
#Correlation Matrix & Corrplot
corlMatrix <- cor(Factor_Hair_1[-12])
corrplot(corlMatrix,method="number")

# Pair wise scatter plot
pairs(Factor_Hair_1[1:11],pch=20,col=c("dark blue","dark green","magenta","aquamarine3"))

# Perform Simple Linear Regression against Satisfaction

model_ProdQual = lm(Satisfaction ~ ProdQual)
summary(model_ProdQual)

model_Ecom = lm(Satisfaction ~ Ecom)
summary(model_Ecom)

model_TechSup = lm(Satisfaction ~ TechSup)
summary(model_TechSup)

model_CompRes = lm(Satisfaction ~ CompRes)
summary(model_CompRes)

model_Adv = lm(Satisfaction ~ Advertising)
summary(model_Adv)

model_ProdLine = lm(Satisfaction ~ ProdLine)
summary(model_ProdLine)

model_SalesF = lm(Satisfaction ~ SalesFImage)
summary(model_SalesF)

model_ComPrice = lm(Satisfaction ~ ComPricing)
summary(model_ComPrice)

model_Warty = lm(Satisfaction ~ WartyClaim)
summary(model_Warty)

model_OrdBill = lm(Satisfaction ~ OrdBilling)
summary(model_OrdBill)

model_DelSpeed = lm(Satisfaction ~ DelSpeed)
summary(model_DelSpeed)

#Perform rincipal Component Analysis

# Eigen Value calculation
ev = eigen(corlMatrix)
EigenValue = ev$values
EigenValue

# Plotting Screeplot
plot(EigenValue, main = "Scree Plot", xlab = "Factors", ylab = "Eigen Values", pch = 19, col = "blue")
lines(EigenValue, col = "red")

# Removing the dependent variable -Satisfaction from the dataset for PCA
Factor_Hair_2 <- Factor_Hair_1[-12]
names(Factor_Hair_2)

# Principal Component Extraction
pca_Factor_Hair <- prcomp(Factor_Hair_2)
summary(pca_Factor_Hair)


# Factor Analysis

# Perform Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy(MSA > 0.5)
KMO(corlMatrix)

# Perform Bartlett Sphericity Test  < 0.05
cortest.bartlett(corlMatrix, 100)

# FA With No Rotation (no: of factors = 4)
Factor_NoRotate <- principal(Factor_Hair_2,nfactors=4,rotate = "none")
Factor_NoRotate 
fa.diagram(Factor_NoRotate)

# FA with Rotation- Varimax(no: of factors = 4)
Factor_Rotate <- principal(Factor_Hair_2,nfactors=4,rotate = "varimax")
Factor_Rotate
fa.diagram(Factor_Rotate)

# Obtaining Factor Scores from FA o/p with Rotation
Factor_Hair_2_Scores <- Factor_Rotate$scores
Factor_Hair_2_Scores 


# Creating a new Dataframe with 5 variables
Factor_Hair_3 <- cbind(Factor_Hair_2_Scores,Factor_Hair_1[12] )
Factor_Hair_3
# Adding new names to the columns
colnames(Factor_Hair_3) <- c("PurchaseDetails","Marketing","AfterServices","ProdDetails","CustSatisfaction")

Factor_Hair_3

# Perform Correlation with new variables 
corlMatrix_2 <- cor(Factor_Hair_3[-5])
round((corlMatrix_2),2) 
corrplot(corlMatrix_2,method="number")

# Multiple Linear Regression (MLR)

linearModel_new = lm(CustSatisfaction ~., data = Factor_Hair_3)
summary(linearModel)

###############################THE END####################################
