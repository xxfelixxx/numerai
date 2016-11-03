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

significance <- 0.05

rows <- dim(training)[1]

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

# LogLoss Results (lower = better)
#
# GLM1             -> 0.69329 using all features
# GLM2             -> 0.69272 using features (6,8,9,13,15,17,19,20,21) *,**,***
# GLM3             -> 0.69325 using features (6,8,9,15,17,20,21)
# median bin       -> 0.69272
# mean bin         -> 0.69272
# randomTree (500) -> 0.69279 using all features
# randomTree (50)  -> 0.69272 using features (6,8,9,13,15,17,19,20,21)
# randomTree (50)  -> 0.69313 using features (6,8,9,13,15,17,19,20,21) v2
# kNN (101)        -> 0.69726
# GLM2 scale_0.5   -> 0.69280 out$probability <- out$probability/2+0.25135
# GLM2 scale_0.51  -> 0.69278 out$probability <- out$probability/2+0.25
# GLM2 scale_0.52  -> 0.69279 out$probability <- out$probability/2+0.24
# GLM2 scale_0.53  -> 0.69273 out$probability <- 0.5 + 1.05*(out$probability-0.5)
# GLM2 scale_0.54  -> 0.69271 out$probability <- 0.5 + 0.95*(out$probability-0.5)
# GLM2 scale_0.55  -> 0.69271 out$probability <- 0.5 + 0.90*(out$probability-0.5)
# GLM2 scale_0.56  -> 0.69270 out$probability <- 0.5 + 0.80*(out$probability-0.5)
# GLM2 scale_0.57  -> 0.69278 out$probability <- 0.5 + 0.50*(out$probability-0.5)
# GLM2 scale_0.58  -> 0.69271 out$probability <- 0.5 + 0.75*(out$probability-0.5)
# GLM2 scale_0.59  -> 0.69270 out$probability <- 0.5 + 0.83*(out$probability-0.5)
# GLM2 scale_0.59(new_data)  -> 0.69288 out$probability <- 0.5 + 0.83*(out$probability-0.5)
# GLM ENS123_0     -> 0.69326 average of guesses of fit1,fit2,fit3

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

# Plots of the features
if (0) {
for (feature in colnames(training)) {
    if(feature == 'target') next
    f0 <- training[target_0,eval(feature)]
    f1 <- training[target_1,eval(feature)]
    kolg_smirnov <- ks.test(f0,f1)

    print(paste("###",feature))
    print( ifelse(kolg_smirnov$p.value > significance,
                  "    SAME: feature -> 0 and feature -> 1",
                  "    DIFF: feature -> 0 and feature -> 1") )

    st0 <- shapiro.test(sample(f0,5000, replace=TRUE))
    print( ifelse(st0$p.value > significance,
                  "    NORM: f0 is normal",
                  "    ABBY: f0 is NOT normal") )

    st1 <- shapiro.test(sample(f1,5000, replace=TRUE))
    print( ifelse(st1$p.value > significance,
                  "    NORM: f1 is normal",
                  "    ABBY: f1 is NOT normal") )

    png_filename=paste(feature,".png",sep="")
    png(file=png_filename)
    plot(c(1,rows),c(0,1), main=feature, yaxt="n", xaxt="n", col="#FFFFFFFF",
         ylab="feature value [0 - 1]", xlab="observation",
         ylim=c(0,1))
    points(c(1:length(f0)),f0,col="red",  pch=20, cex=0.05)
    points(length(f0)+c(1:length(f1)),f1,col="blue", pch=20, cex=0.05)
    text(0.25 * rows, 1.02, "Target -> 0", col="red", cex=0.7)
    text(0.75 * rows, 1.02, "Target -> 1", col="blue", cex=0.7)    
    dev.off()

    png_filename=paste(feature,"_hist.png",sep="")
    png(file=png_filename)
    binsize <- 1/100
    bins <- seq(0,1,binsize)
    h0 <- hist(f0,bins)
    h1 <- hist(f1,bins)
    max_density <- max(h0$density, h1$density)
    xlabtxt <- paste("bins of size", binsize, "from 0 to 1", sep=" ")
    ymax <- ceiling(max_density)
    plot(c(0,1),c(0,ymax), main=paste(feature, "density"),
         yaxt="n", xaxt="n", col="#FFFFFFFF",
         ylab="density", xlab=xlabtxt,ylim=c(0,ymax))
    points(bins[-1],h0$density,col="red", pch=20, cex=0.5)
    points(bins[-1],h1$density,col="blue", pch=20, cex=0.5)
    text(0.25, ymax, "Target -> 0", col="red", cex=0.7)
    text(0.75, ymax, "Target -> 1", col="blue", cex=0.7)    
    dev.off()

    png_filename=paste(feature,"_hist_qq.png",sep="")
    png(file=png_filename)
    plot(c(0,ymax),c(0,ymax), main=paste(feature, "qq density"),
         yaxt="n", xaxt="n", col="#FFFFFFFF", ylab="Target -> 1", xlab="Target -> 0",
         ylim=c(0,ymax),xlim=c(0,ymax))
    points(h0$density,h1$density, pch=20, cex=0.5)
    lines(c(0,ymax),c(0,ymax))
    dev.off()
}
}

