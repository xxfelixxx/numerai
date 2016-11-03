source('func.R')

training   <- read.csv('numerai_training_data.csv')
tournament <- read.csv('numerai_tournament_data.csv')

target_0 <- which(training$target == 0)
target_1 <- which(training$target == 1)

# Binning into bands of 0.05, then using the ratio of 1/0 as estimator by looking up hdata
hdata <- NULL
for (i in seq(1,21)) {
    h0 <- hist(training[target_0,i])
    h1 <- hist(training[target_1,i])
    rat <- h1$counts / (h0$counts + h1$counts)
    hdata <- cbind(hdata,rat)
}
colnames(hdata) <- colnames(training)[1:21]
rownames(hdata) <- h0$mids

dm <- as.data.frame(cbind(tournament$t_id,tournament$t_id))
colnames(dm) <-  c("t_id", "probability")
for (i in seq(1,nrow(tournament))) {
    rates <- NULL
    for (f in seq(1,21)) {
        feature <- tournament[i,f]
        dbin <- abs(h0$mids - feature)
        bin <- which(dbin == min(dbin))
        rates <- c(rates, hdata[bin,f])
    }
    dm$probability[i] <- mean(rates)
}

colnames(dm) <-  c("t_id", "probability")
write.csv(out,"guess_bin.csv", row.names=FALSE)

## library(randomForest)

## ii <- c(1:100)
## fit <- randomForest(training$target[ii] ~ training$feature21[ii] + training$feature19[ii], data=training)

fit <- lm(target ~ feature1)

# All features
fit <- glm(target ~ feature1 + feature2 + feature3 + feature4 + feature5 + feature6 + feature7 + feature8 + feature9 + feature10 + feature11 + feature12 + feature13 + feature14 + feature15 + feature16 + feature17 + feature18 + feature19 + feature20 + feature21, data=training)

# "Significant" features
fit <- glm(target ~ feature6 + feature8 + feature9 + feature13 + feature15 + feature17 + feature19 + feature20 + feature21, data=training)

# "Significant" features ** or more
fit <- glm(target ~ feature6 + feature8 + feature9 + feature15 + feature17 + feature20 + feature21, data=training)

# "Significant" features *** only
fit <- glm(target ~ feature6 + feature8 + feature9 + feature20 + feature21, data=training)


#guess <- predict(fit)
#
#thresh <- FindThreshold(guess,training$target)
#out <- PredictProbability(tournament, fit, thresh)
#ValidateModel(training, fit, thresh)
out = PredictTournament( tournament, fit, training$target )
write.csv(out,"guess.csv", row.names=FALSE)

library("randomForest")
tree <- randomForest(target ~ ., training, ntree=50, norm.votes=FALSE, nodesize=1)

tree <- randomForest(target ~ feature6 + feature8 + feature9 + feature13 + feature15 + feature17 + feature19 + feature20 + feature21, training, ntree=50, norm.votes=FALSE, nodesize=1)


# kNN
library(class)
train <- training
train$target <- NULL
tour <- tournament
tour$t_id <- NULL
fit <- knn(train, tour, training$target, k=101, prob=TRUE)
prob <- attr(fit,'prob')
pred <- as.vector(fit)
new_prob <- prob
for (i in seq(1,length(prob))) {
    if ( pred[i] == 0 ) {
        new_prob[i] = 1-prob[i]
    }
}
out <- data.frame( tournament$t_id, new_prob )
colnames(out) <- c("t_id", "probability")
write.csv(out,"guess_kNN.csv", row.names=FALSE)
