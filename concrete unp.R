library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(ggthemes)
library(dslabs)
library(caret)
library(tidyverse)
library(lubridate)
library(corrplot)
library(RColorBrewer)
library(hexbin)
library(tableplot)


#importing data
con=read.csv("Concrete_Data_csv_set.csv", header =TRUE, sep = ",")

#changing column names
colnames(con)=c("Cement", "BlastFurnace", "FlyAsh", "Water", 
                "SuperPlasticizer", "CoarseAggregate", "FineAggregate", "Age", "ConcreteStrength")
head(con)

#finding correlation between the variables

M=cor(con)
corrplot(M, method = 'number')
corrplot(M, method = 'number', diag = FALSE)
corrplot(M, method = 'color')
corrplot(M)
corrplot(M, order = 'AOE')
corrplot(M, method = 'shade')

cor_mat <- cor(con)
cor_mat

#EDA

#hexbin
a=hexbin(con$SuperPlasticizer,con$Water,xbins = 40)
plot(a)
rf <- colorRampPalette(rev(brewer.pal(11,'Set3')))
hexbinplot(con$Water~con$SuperPlasticizer, data=con,colramp=rf,main = "hexbin binning")

#scatter
plot(~ConcreteStrength+Cement+SuperPlasticizer+Water , data = con,col=brewer.pal(5,"Set1"), main="scatter plot")
pairs(~ConcreteStrength+Cement+SuperPlasticizer+Water , data = con, main="simple scatter plot")

plot(con,col=brewer.pal(9,"Set1"),main ="plots")
con %>% ggplot(aes(Cement,ConcreteStrength)) + geom_point(color="violet")
con %>% ggplot(aes(SuperPlasticizer,Water)) + geom_point(color="blue")

#histogram

hist(con$Cement,col=brewer.pal(12,"Set3"),main="HISTOGRAM OF CEMENT")
hist(con$Cement,col=brewer.pal(7,"Greys"),main="HISTOGRAM OF CEMENT")
hist(con$Cement,col=brewer.pal(7,"Purples"),main="HISTOGRAM OF CEMENT")
con %>% ggplot(aes(SuperPlasticizer)) + geom_histogram(binwidth = 3)
con %>% ggplot(aes(Cement)) + geom_histogram(binwidth = 10)



#boxplot
data(con)
boxplot(con,col="pink")
boxplot(con$Cement~con$SuperPlasticizer,col="purple")
boxplot(con,col=heat.colors(4))
boxplot(con,col=topo.colors(4),pars=list(boxwex = 0.7, staplewax = 0.5, outwex = 0.5))

con %>% ggplot(aes(group=SuperPlasticizer,Water)) + geom_boxplot()
summary(con)

#piechart
pie(table(con$SuperPlasticizer),radius = 1.05,main = "pie chart of Super Plasticizizer")

#fitting lst linear regression models
train <- con[1:800,]
test <- con[801:1005,]

linear1= lm(ConcreteStrength~ Cement + Water + SuperPlasticizer + BlastFurnace + FlyAsh +
              CoarseAggregate + FineAggregate + Age , data = train)
summary(linear1)

#Mean Squared Error on the linear1 on the test data

predict_strength1 = predict(linear1,test)
predict_strength1

mean((test1$ConcreteStrength-predict_strength1)^2)


#building another model by removing FineAggregate and CoarseAggregate


linear2= lm(ConcreteStrength~ Cement + Water + SuperPlasticizer + BlastFurnace 
            + FlyAsh + Age , data = train)
summary(linear2)

#Mean Squared Error on the linear2 on the test data
predict_strength2 = predict(linear2,test)
predict_strength2

mean((test$ConcreteStrength-predict_strength2)^2)

#removing fine aggregate and coarse aggregate there is a
#decrease the mean squared error of the model on the test.
#we go with second model linear2

#correlation between response in the test data and response of linear2 model
cor(test$ConcreteStrength,predict(linear2,test))


#testing a model on a single new data
new_data = data.frame(Cement=400,
                      BlastFurnace=50,
                      FlyAsh=50,
                      Water=190,
                      SuperPlasticizer=3,
                      Age=10)
predict(linear2,new_data)
predict(linear2,new_data,interval = "confidence")

#line graphs for predicted values vs original values
ggplot(data=test,aes(x=1:nrow(test)))+geom_line(aes(y=ConcreteStrength,color="blue"))+geom_line(aes(y=predict_strength2,color="red"))+labs(title="Actual Strength Vs Predicted Strength (Test Dataset)",x="",color="")+scale_color_manual(labels = c("Actual", "Predicted"), values = c("black", "red"))

ggplot(data=test,aes(x=1:nrow(test)))+geom_path(aes(y=ConcreteStrength,color="blue"))+geom_path(aes(y=predict_strength2,color="red"))+labs(title="Actual Strength Vs Predicted Strength (Test Dataset)",x="",color="")+scale_color_manual(labels = c("Actual", "Predicted"), values = c("black", "red"))

ggplot(data=test,aes(x=1:nrow(test)))+geom_step(aes(y=ConcreteStrength,color="blue"))+geom_step(aes(y=predict_strength2,color="red"))+labs(title="Actual Strength Vs Predicted Strength (Test Dataset)",x="",color="")+scale_color_manual(labels = c("Actual", "Predicted"), values = c("black", "red"))





