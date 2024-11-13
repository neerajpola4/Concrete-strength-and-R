library(dplyr)
library(ggplot2)
library(ggthemes)

con=read.csv("Concrete_Data_csv_set.csv", header =TRUE, sep = ",")

colnames(con)=c("Cement", "BlastFurnace", "FlyAsh", "Water", 
                "SuperPlasticizer", "CoarseAggregate", "FineAggregate", "Age", "ConcreteStrength")

#creating a test data
set.seed(5)
test = sample(nrow(con),200)

#linear regression model
lrm <- lm(ConcreteStrength~., data = con, subset=-test)
summary(lrm)
#Mean Squared Error on the lrm on the test data
predict_strength = predict(lrm,con[test,])
mean((con[test,]$ConcreteStrength-predict_strength)^2)

#another linear regression model 
lrm2 <- lm(ConcreteStrength~.-CoarseAggregate-FineAggregate, data = con, subset=-test)
summary(lrm2)
#Mean Squared Error on the lrm2 on the test data
predict_strength = predict(lrm2,con[test,])
mean((con[test,]$ConcreteStrength-predict_strength)^2)

cor(con[test,]$ConcreteStrength,predict(lrm,con[test,]))


