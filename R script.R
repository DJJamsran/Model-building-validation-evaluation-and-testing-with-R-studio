
# Explore the first few rows in the data set
data <- read.csv("C:/Users/davja/Downloads/automobileEDA.csv")
head(data)
str(data)

data$aspiration = as.factor(data$aspiration)
data$make <- as.factor(data$make)
data$num.of.doors <- as.factor(data$num.of.doors)
data$body.style <- as.factor(data$body.style)
data$drive.wheels <- as.factor(data$drive.wheels)
data$engine.location <- as.factor(data$engine.location)
data$engine.type <- as.factor(data$engine.type)
data$num.of.cylinders <- as.factor(data$num.of.cylinders)
data$horsepower.binned <- as.factor(data$horsepower.binned)
data$fuel.system <- as.factor(data$fuel.system)
data$gas <- as.factor(data$gas)
data$diesel <- as.factor(data$diesel)
data$symboling <- as.factor(data$symboling)

str(data)
summary(data)

# Data Pre-Processing and Data Exploration 
sapply(data, class)
View(data)
attach(data)


#Creating new data set with numeric variables
newdata <- data.frame(normalized.losses, wheel.base, length, width, height,
                      curb.weight, engine.size, bore, stroke, compression.ratio,
                      horsepower, peak.rpm, city.mpg, highway.mpg, city.L.100km,
                      price)
head(newdata)
sapply(newdata, class)

median_stroke <- mean(stroke, na.rm = TRUE)
print(median_stroke)
#[1] 3.256904 ~ 3.26

boxplot(stroke, horizontal = FALSE, 
        col = "lightgreen",xlab = "Stroke", ylab = "",
        main = "Median of Stroke")
dev.off()

newdata[is.na(newdata)] <- 3.26
is.na(newdata)
#No more NA inputs.

#Calculating correlation matrix 
cor(newdata)
cov(newdata)
# The correlation between price and the numeric variables, the following variables
# a strong positive relationship with Price:
# Length ~ 0.69062838
# Width ~ 0.75126534
# Curb.weight ~ 0.83441453
# Engine.size ~ 0.87233517
# Horsepower ~ 0.80957457
# City.L.100km ~ 0.789897514

# However, the following variables indicates the strong negative relationship
# with Price: 
#City.mpg ~ 0.68657101
#Highway.mpg ~ 0.70469227

# Visualizing the linear relationship between a target variable and independent
# variables 
library('ggplot2')

ggplot(data = newdata, mapping = aes(x = engine.size, y = price)) +
  geom_point(size = 0.9) +
  geom_smooth(method = 'lm', se = FALSE, size = 0.9)

ggplot(data = newdata, mapping = aes(x = peak.rpm, y = price)) +
  geom_point(size = 0.9) +
  geom_smooth(method = 'lm', se = FALSE, size = 0.9)

dev.off


strong.var <- data.frame(length, width,curb.weight, engine.size,
                       horsepower, city.mpg, highway.mpg, city.L.100km,
                       price)

#Pairwise plotting 
pairs(strong.var, panel = panel.smooth)


# Model 1: Multiple Linear Regression 

# Building model 

# Selecting variables by using Backward 
backward <- step(lm(price~length+width+curb.weight
                    +engine.size+horsepower+city.L.100km+city.mpg+highway.mpg), direction = 
                   "backward", data = newdata)

summary(backward)


#K-10 Cross Validation to pick the best model 
library('boot')

mod1 <- glm(price~engine.size+city.L.100km+city.mpg+curb.weight+width,
              data = newdata)
mod2 <- glm(price~engine.size+city.L.100km+city.mpg+curb.weight, 
              data = newdata)
mod3 <- glm(price~engine.size+city.L.100km+city.mpg+width, 
              data = newdata)
mod4 <- glm(price~engine.size+city.L.100km+city.mpg, data = newdata)


model <- c(1:4)
cv_errorKF = rep(0,4)
set.seed(100)
cv_errorKF[1] <- cv.glm(newdata,mod1,K=5)$delta[1]
cv_errorKF[2] <- cv.glm(newdata,mod2,K=5)$delta[1]
cv_errorKF[3] <- cv.glm(newdata,mod3,K=5)$delta[1]
cv_errorKF[4] <- cv.glm(newdata,mod4,K=5)$delta[1]

print(cv_errorKF)
#[1] 11349911 11980183 11059839 12291696


#Plotting the errors
par(mfrow=c(2,2))
plot(cv_errorKF~model, type = 'b', col = 'blue', xlab='Model',
     ylab = 'Model Errors', main = 'Cross Validation Error')

dev.off()

#Finalizing model 

finalmodel <- lm(price~engine.size+city.L.100km+city.mpg+width,
                 data = newdata)
summary(finalmodel)

#Call:
#  lm(formula = price ~ engine.size + city.L.100km + city.mpg + 
#       width, data = newdata)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-10120.6  -1569.1    -90.5   1240.3  14050.5 

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -76750.70   12578.95  -6.102 5.51e-09 ***
# engine.size      94.75      10.00   9.472  < 2e-16 ***
# city.L.100km   1954.74     350.46   5.578 8.01e-08 ***
# city.mpg        431.64     122.30   3.529 0.000519 ***
# width         52047.07   12362.77   4.210 3.89e-05 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3323 on 196 degrees of freedom
#Multiple R-squared:  0.8287,	Adjusted R-squared:  0.8252 
#F-statistic:   237 on 4 and 196 DF,  p-value: < 2.2e-16

#Anova analysis
anova(finalmodel)
qf(0.95,4,196)
#[1] 2.417725


#Histogram plot of residuals 
hist(finalmodel$residuals)

#Residual plotting
par(mfrow=c(2,2))
plot(finalmodel)
dev.off()

# Model 2: Classification analysis 

df <- data.frame(length,width,curb.weight,engine.size,horsepower,
                  city.L.100km,city.mpg, highway.mpg, aspiration,
                  num.of.doors,engine.location,diesel,gas)
head(df)
median <- median(price)
print(median)
# [1] 10295
price_category=ifelse(price<=median,"Low","High")
str(price_category)
# chr [1:201] "High" "High" "High" "High" "High" "High" "High" "High" "High" 

#Converting to the format as factor 
price_category=as.factor(price_category)
str(price_category)
#Factor w/ 2 levels "High","Low": 1 1 1 1 1 1 1 1 1 1 ...

price_median <- data.frame(price, price_category)
par(mfrow=c(2,2))
boxplot(price~price_category, data = price_median)



#Creating a new data set
data.class <- data.frame(df,price_category)
head(data.class)
#length     width curb.weight engine.size horsepower city.L.100km city.mpg highway.mpg aspiration num.of.doors
#1 0.8111485 0.8902778        2548         130        111    11.190476       21          27        std          two
#2 0.8111485 0.8902778        2548         130        111    11.190476       21          27        std          two
#3 0.8226814 0.9097222        2823         152        154    12.368421       19          26        std          two
#4 0.8486305 0.9194444        2337         109        102     9.791667       24          30        std         four
#5 0.8486305 0.9222222        2824         136        115    13.055556       18          22        std         four
#6 0.8519942 0.9208333        2507         136        110    12.368421       19          25        std          two
#engine.location diesel gas price_category
#1           front      0   1           High
#4           front      0   1           High
#5           front      0   1           High
#6           front      0   1           High


library('tree')
Price_tree <- tree(price_category~., data = data.class)
plot(Price_tree)
text(Price_tree, pretty=0)
summary(Price_tree)
#Misclassification error rate: 0.04478   

#Classification tree:
#  tree(formula = price_category ~ ., data = data.class)
#Variables actually used in tree construction:
#  [1] "curb.weight"  "width"        "highway.mpg"  "city.L.100km"
#Number of terminal nodes:  8 
#Residual mean deviance:  0.1917 = 37.01 / 193 
#Misclassification error rate: 0.04478 = 9 / 201 

#Validation 
set.seed(1000)
tr <- sample(1:nrow(data.class), nrow(data.class)*0.8)
train_set <- data.class[tr,]
test_set <- data.class[-tr,]


#Training the classification model
treemodel_train <- tree(price_category~., train_set)
par(mfrow=c(2,2))
plot(treemodel_train)
text(treemodel_train, pretty=0)

summary(treemodel_train)
dev.off()

#Classification tree:
#  tree(formula = price_category ~ ., data = train_set)
#Variables actually used in tree construction:
#  [1] "curb.weight"  "city.L.100km" "width"        "length"       "engine.size" 
#Number of terminal nodes:  9 
#Residual mean deviance:  0.1527 = 23.06 / 151 
#Misclassification error rate: 0.0375 = 6 / 160  


#Cross Validation 
tree_predict <- predict(treemodel_train, test_set, type = 'class')
actual_class <- test_set[,'price_category']
tab1 <- table(tree_predict, actual_class)
tab1

Misclassification_rate <- (tab1[1,2]+tab1[2,1])/sum(tab1)
print(Misclassification_rate)
# [1] 0.1219512
#           actual_class
#tree_predict High Low
#High         22   3
#Low          2  14


#Pruned Model 
dev.off()
set.seed(1000)
cv_trainmodel <- cv.tree(treemodel_train, FUN = prune.misclass)
cv_trainmodel
par(mfrow=c(2,2))
plot(cv_trainmodel$size, cv_trainmodel$dev, type ='b', col = 'blue',
     xlab = 'Size of Tree', ylab = 'Error', main = 'Cross Validation Plot')

# Best size for the tree is 7

# Best Tree Model

best_tree <- prune.misclass(treemodel_train, best=7)
plot(best_tree)  
text(best_tree, pretty = 0)

summary(best_tree)

#Classification tree:
#  snip.tree(tree = treemodel_train, nodes = 2L)
#Variables actually used in tree construction:
#  [1] "curb.weight"  "city.L.100km" "length"       "engine.size" 
#Number of terminal nodes:  7 
#Residual mean deviance:  0.2262 = 34.6 / 153 
#Misclassification error rate: 0.0375 = 6 / 160 

#Cross validation

prunedtree_predict <- predict(best_tree, test_set, type = 'class')
actual_class <- test_set[,'price_category']
tab2 <- table(prunedtree_predict, actual_class)
Besttree_Misclassification_rate <- (tab2[1,2]+tab2[2,1])/sum(tab2)
print(Besttree_Misclassification_rate)
#[1] 0.1219512
#> tab2
#                actual_class
#prunedtree_predict High Low
#High                22   3
#Low                  2  14



#Unsupervised Learning: Principal Component Analysis 

# Original data set has 29 variables. For PCA analysis, we excluded
# the categorical values, and With 15 numerical variables excluding the target
# variables, it seems complex, Therefore, we execute PCA analysis only on the 
# variables which have strong linear relations with the target variables. 

str(strong.var)

#'data.frame':	201 obs. of  9 variables:
#$ length      : num  0.811 0.811 0.823 0.849 0.849 ...
#$ width       : num  0.89 0.89 0.91 0.919 0.922 ...
#$ curb.weight : int  2548 2548 2823 2337 2824 2507 2844 2954 3086 2395 ...
#$ engine.size : int  130 130 152 109 136 136 136 136 131 108 ...
#$ horsepower  : num  111 111 154 102 115 110 110 110 140 101 ...
#$ city.mpg    : int  21 21 19 24 18 19 19 19 17 23 ...
#$ highway.mpg : int  27 27 26 30 22 25 25 25 20 29 ...
#$ city.L.100km: num  11.19 11.19 12.37 9.79 13.06 ...
#$ price       : num  13495 16500 16500 13950 17450 ...


#Removing the target variable

pca_df <- strong.var[,-9]
head(pca_df)
#     length     width curb.weight engine.size horsepower city.mpg highway.mpg city.L.100km
#1 0.8111485 0.8902778        2548         130        111       21          27    11.190476
#2 0.8111485 0.8902778        2548         130        111       21          27    11.190476
#3 0.8226814 0.9097222        2823         152        154       19          26    12.368421
#4 0.8486305 0.9194444        2337         109        102       24          30     9.791667
#5 0.8486305 0.9222222        2824         136        115       18          22    13.055556
#6 0.8519942 0.9208333        2507         136        110       19          25    12.368421

sapply(pca_df, class)
#length        width  curb.weight  engine.size   horsepower     city.mpg  highway.mpg city.L.100km 
#"numeric"    "numeric"    "integer"    "integer"    "numeric"    "integer"    "integer"    "numeric" 



#Examining the mean and variance of the variables
sapply(pca_df, mean)
sapply(pca_df, var)

#Mean 
#length       width     curb.weight   engine.size  horsepower    city.mpg     highway.mpg   city.L.100km 
#0.8371023    0.9151258 2555.6666667  126.8756219  103.4055339   25.1791045   30.6865672    9.9441455 

#Variance 
#length       width       curb.weight    engine.size horsepower   city.mpg     highway.mpg  city.L.100km 
#3.506151e-03 8.518865e-04 2.675959e+05 1.726139e+03 1.396195e+03 4.125776e+01 4.644627e+01 6.424193e+00




#Perform Principal Component Analysis
#It is important that we use scale is True, because using scale true makes sure
# our data is standardized and avoid from bias. 
pr.outt <- prcomp(pca_df, scale. = TRUE)
summary(pr.outt)

#Importance of components:
#  PC1    PC2     PC3     PC4     PC5     PC6     PC7     PC8
#Standard deviation     2.5231 0.8977 0.66605 0.38231 0.32362 0.25979 0.21836 0.13597
#Proportion of Variance 0.7957 0.1007 0.05545 0.01827 0.01309 0.00844 0.00596 0.00231
#Cumulative Proportion  0.7957 0.8965 0.95193 0.97020 0.98329 0.99173 0.99769 1.00000


#Elements of PCA object
names(pr.outt)
#[1] "sdev"     "rotation" "center"   "scale"    "x" 


#Eigenvectors 
pr.outt$rotation
#               PC1           PC2         PC3         PC4         PC5         PC6         PC7
#length        0.3332897  0.4766261  0.32551680  0.49422027 -0.42209007  0.34804899  0.02562255
#width         0.3349434  0.4814955  0.12517231 -0.79044028  0.01408091  0.05130477  0.09945659
#curb.weight   0.3708339  0.2871132 -0.05449399  0.25501568  0.18507604 -0.77598658 -0.24037931
#engine.size   0.3417099  0.1439066 -0.67681708  0.17227879  0.44435811  0.41201561  0.07328211
#horsepower    0.3503835 -0.3049415 -0.44657404 -0.09895905 -0.66764011 -0.20375211  0.29336777
#city.mpg     -0.3594670  0.3798938 -0.32629992 -0.04803211 -0.09414733 -0.14344419 -0.10921057
#highway.mpg  -0.3657836  0.2929573 -0.32950789 -0.05019111 -0.36469529  0.09319563 -0.46385932
#city.L.100km  0.3696818 -0.3386946  0.05333191 -0.14703195 -0.04821558  0.18314229 -0.78303405
#                PC8
#length       -0.10759191
#width         0.05281134
#curb.weight   0.13362293
#engine.size   0.04671855
#horsepower   -0.04120580
#city.mpg     -0.75915651
#highway.mpg   0.55895458
#city.L.100km -0.27413589

#Original std Dev and mean of variables used in the PCA analysis 
pr.outt$center
#length        width  curb.weight  engine.size   horsepower     city.mpg  highway.mpg city.L.100km 
#0.8371023    0.9151258 2555.6666667  126.8756219  103.4055339   25.1791045   30.6865672    9.9441455 

pr.outt$scale
#length        width  curb.weight  engine.size   horsepower     city.mpg  highway.mpg city.L.100km 
#0.05921276   0.02918709 517.29672658  41.54683445  37.36569950   6.42322047   6.81514994   2.53459926 


#Standard Deviation of each principal component
pr.outt$sdev
#[1] 2.5230893 0.8976941 0.6660453 0.3823072 0.3236242 0.2597885 0.2183583 0.1359684


install.packages('factoextra')
library('factoextra')

#Variance of each principal component
pr.varr <- pr.outt$sdev^2
pr.varr
#[1] 6.36597962 0.80585476 0.44361640 0.14615879 0.10473263 0.06749005 0.04768035
#[8] 0.01848740

#Proportional Variance explained by each principal component
pvee <- pr.varr/sum(pr.varr)
pvee
#[1] 0.795747453 0.100731845 0.055452049 0.018269849 0.013091579 0.008436256 0.005960044
#[8] 0.002310925

# Visualizing the proportional variance by each principal component
fviz_eig(pr.outt, addlabels = TRUE)


#Cumulative proportion

cumsumm <- cumsum(pvee)
cumsumm

#Plots

#Plot for Proportion of Variance Explained
plot(pvee, xlab = 'Principal Component', ylab = 'Proportion of Variance Explained',
     ylim = c(0,1), type = 'b')


plot(cumsumm, xlab = 'Principal Component', ylab = 'Cumulative Proportion of Variance Explained',
     ylim = c(0,1), type = 'b')


#Biplot
biplot(pr.outt, scale = 0)
dev.off()

fviz_pca_biplot(pr.outt)

fviz_pca_biplot(pr.outt, label = 'var',
                col.ind = 'black',
                col.var = 'contrib',
                gradient.cols = c("blue","green","red"))

#Rotation the plot for simplicity
pr.outt$rotation = -pr.outt$rotation
pr.outt$x = -pr.outt$x
biplot(pr.outt, scale = 0)





