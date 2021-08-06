setwd("~/R/PracDataSci")
#coffee chromotography dataset
data = read.table("coffee-origin.txt") #LC-MS data
info = read.table("origin-info.txt", header =TRUE) #info

#Q1 PCA : scores for PC1, PC2, coloured by origin

cofpc = prcomp(data)

plot(cofpc$x[ ,1], cofpc$x[ ,2], main = "PCA of LC-MS Data, Unscaled", cex.main =1, xlab= "PC1", ylab= "PC2", col =as.factor(info$origin),pch =19)
#text(cofpc$x[,1], cofpc$x[,2]+2,labels=info$origin, cex=0.5) #adds label above each point, red is W
#used to check colours match up etc
legend("topright",inset = 0.02,title = "Country of Origin", legend = unique(info$origin), col =c(1,2,3,4,5), pch =19) #fill are the colours used
#scores for first 2 PCs, coloured by origin

#Q2
row.names(data) = info$name
coffee.df = data.frame(X=I(as.matrix(data)),Y=I(as.matrix(info[,5:10])))
library("pls")
#create train/test data
library("caret")

set.seed(123)

model <-plsr(Y ~., ncomp = 10, validation = "CV", scale = TRUE, data= coffee.df)#default method, CV, 10 comps, scaled
#predict Y?

summary(model)

#Q3 plot rmsep

plot(RMSEP(model),legendpos="topright")
#decide how many components would be best for classification
selectNcomp(model, plot=T) #doesnt work as not univariate
#see from plots that ncomp=6 gives low errors
model <-plsr(Y ~., ncomp = 6, validation = "CV", scale = TRUE, data= coffee.df)#default method, CV, 10 comps, scaled

#Q4
biplot(model, which = "scores", cex =0.8) #comps = 1:2 etc to specify comps
#which can be one of x, y, scores, loadings

#Q5
biplot(model,which="y", cex =0.8)

#Q6
summary(model)

#Q7
#weight loadings for each component
top = 36.37*model$loadings[,1]*model$loadings[,1] + 11.62*model$loadings[,2]*model$loadings[,2] +8.30*model$loadings[,3]*model$loadings[,3] + 4.81*model$loadings[,4]*model$loadings[,4]+ 6.45*model$loadings[,5]*model$loadings[,5]+ 2.90*model$loadings[,6]*model$loadings[,6]

#find the 40 variables with highest combined loadings
topvarnums = order(top)[18306:18345]
topvarnums
datasmall = data[,topvarnums] #reduce the data

#Q8
model <-lm(info$chocolate~ ., data = datasmall)

index = c(1:45) #because 45 obs
plot(index, model$fitted.values, ylab= "predicted values for chocolate", pch= 20, col = info$chocolate)
legend("bottomleft", pch= 20, legend = unique(info$chocolate), title = "chocolate level", col = unique(info$chocolate))

table(true = info$chocolate, predicted = round(model$fitted.values)) #rounds to nearest 0.5
#can see how many obs are predicted correctly if rounded to the nearest integer

summary(model)


newmodel = step(model)
summary(newmodel)

plot(index, newmodel$fitted.values, ylab= "predicted values for chocolate", pch= 20, col = info$chocolate)
legend("bottomleft", pch= 20, legend = unique(info$chocolate), title = "chocolate level", col = unique(info$chocolate))
table(true = info$chocolate, predicted = round(newmodel$fitted.values)) #rounds to nearest 0.5


#Q9
model <-lm(info$nuts~ ., data = datasmall)
plot(index, model$fitted.values, ylab= "predicted values for nuts", pch= 20, col = info$nuts)
legend("bottomleft", pch= 20, legend = unique(info$nuts), title = "nut level", col = unique(info$nuts))
table(true = info$nuts, predicted = round(model$fitted.values)) #rounds to nearest 0.5


model <-lm(info$citrus~ ., data = datasmall)
plot(index, model$fitted.values, ylab= "predicted values for citrus", pch= 20, col = info$citrus)
legend("topleft", pch= 20, legend = unique(info$citrus), title = "citrus level", col = unique(info$citrus))
table(true = info$citrus, predicted = round(model$fitted.values)) #rounds to nearest 0.5


model <-lm(info$floral~ ., data = datasmall)
plot(index, model$fitted.values, ylab= "predicted values for floral", pch= 20, col = info$floral)
legend("topleft", pch= 20, legend = unique(info$floral), title = "floral level", col = unique(info$floral))
table(true = info$floral, predicted = round(model$fitted.values)) #rounds to nearest 0.5

model$fitted.values[11] #would be predicted incorrectly
model$fitted.values[29]

model <-lm(info$berry~ ., data = datasmall)
plot(index, model$fitted.values, ylab= "predicted values for berry", pch= 20, col = info$berry)
legend("topleft", pch= 20, legend = unique(info$berry), title = "berry level", col = unique(info$berry))
table(true = info$berry, predicted = round(model$fitted.values)) #rounds to nearest 0.5

model <-lm(info$spice~ ., data = datasmall)
plot(index, model$fitted.values, ylab= "predicted values for spice", pch= 20, col = info$spice)
legend("topleft", pch= 20, legend = unique(info$spice), title = "spice level", col = unique(info$spice))
table(true = info$spice, predicted = round(model$fitted.values)) #rounds to nearest 0.5

#now we need to redo the floral model to try and improve it
model <-lm(info$floral~ ., data = datasmall)
newmodel = step(model)
summary(newmodel)

plot(index, newmodel$fitted.values, ylab= "predicted values for floral", pch= 20, col = info$floral)
legend("topleft", pch= 20, legend = unique(info$floral), title = "floral level", col = unique(info$floral))
table(true = info$floral, predicted = round(newmodel$fitted.values)) #check if it has correct predictions when rounded

