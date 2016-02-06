fit4 <- randomForest(as.factor(target) ~ ., data=strain, importance=TRUE, ntree=50, mtry=8)

t <- ctree(y_train$class ~ ss + s2 + s3, all_factors,
           controls = ctree_control(
             teststat="quad",
             testtype="Univariate",
             mincriterion=.95,
             minsplit=10, 
             minbucket=5
           )
)


sum(t@predict_response() == y_train$class)/nrow(y_train$class)

fit <- rpart(y_train$class ~ ss + s2 + s3, data=all_factors, method="class")
plot(fit)
text(fit)


install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)