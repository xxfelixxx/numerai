MultiLogLoss <- function(act, pred) {
    eps = 1e-15;
    act <- as.matrix(act)
    pred <- as.matrix(pred)
    nr <- nrow(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(nrow(act))
    ll <- trunc(ll*10^5)/10^5 # 5 digits
    return(ll);
}

FindThreshold <- function(guess,target,start=0.2,stop=0.8,resolution=0.001) {
    best <- 0
    besti <- 0
    data <- c(0,0)
    ii <- unique( c( seq(0,start,0.01), seq(start,stop,resolution), seq(stop,1,0.01) ) )
    for (i in ii) {
        wild_guess <- trunc(guess + i) # Set guess to 0 or 1
        tot <- wild_guess + target
        z0 <- length(which(tot == 0))  # True Negative (TN)
        z1 <- length(which(tot == 1))  # False Negative (FN) or False Positive (FP)
        z2 <- length(which(tot == 2))  # True Positive (TN)
        good <- z0 + z2                # Correct Prediction
        bad  <- z1                     # Incorrect Prediction
        per <- good/(good+bad)*100     # Accuracy (True Positives + True Negatives) / Total Population
        if (per > best) {
            best <- per                # Best so far
            besti <- i                 # New Threshold Candidate
        }
        data <- rbind(data,c(i,per))
    }
    data <- data[-1,] # Remove first (dummy) row
    desc <- "Apply a threshold to set predicted values to either 0 or 1, checking accuracy"
    thresh <- list(data=data, best_percent=best, threshold=besti, description=desc)
    return(thresh)
}

ValidateModel <- function(training, fit, thresh) {
    out <- PredictProbability(training, fit, thresh)
    ll <- MultiLogLoss(training$target,out$probability)
    return(ll)
}

PredictProbability <- function(tour, fit, thresh) {
    t_id <- NULL
    if ( "t_id" %in% colnames(tour) ) {
        # Passed in tournament data
        t_id <- tour$t_id
    } else {
        # Passed in training data
        t_id <- seq(1,nrow(tour))
    }

    new_guess <- predict(fit, tour)
    new_target <- trunc(new_guess + thresh$threshold)
    percent <- approx(thresh$data[,1], thresh$data[,2], new_guess, yleft=thresh$data[1,2], yright=thresh$data[nrow(thresh$data),2])
    new_percent <- percent$y
    dt <- data.frame(t_id, new_target, new_percent)
    z0 <- which(dt$new_target == 0)
    dt$percent_target1 <- dt$new_percent/100
    dt$percent_target1[z0] <- 1-(dt$new_percent[z0]/100)

    out <- data.frame(t_id, dt$percent_target1)
    colnames(out) <-  c("t_id", "probability")
    return(out)
}

PredictTournament <- function( fit, training, tournament) {
    # 'fit' is output of gml(...), or randomForest(...), or any other model
    # 'training' is data frame with feature[1-XX] and target columns
    # 'tournament' is data frame with feature[1-XX] and t_id columns
    guess <- predict(fit)
    thresh <- FindThreshold(guess,training$target)
    out <- PredictProbability(tournament, fit, thresh)
    ll <- ValidateModel(training, fit, thresh)
    writeLines(paste("LogLoss is ", ll, sep=''))
    return(out)
}

TransformFeatures <- function( training, nbin=100 ) {
    new_training <- training

    target_0 <- which(training$target == 0)
    target_1 <- which(training$target == 1)

    for (feature in colnames(training)) {
        if(feature == 'target') next

        f0 <- training[target_0,eval(feature)]
        f1 <- training[target_1,eval(feature)]

        binsize <- 1/nbin
        bins <- seq(0,1,binsize)
        h0 <- hist(f0,bins,plot=FALSE)
        h1 <- hist(f1,bins,plot=FALSE)
        h2 <- h1$counts / (h1$counts + h0$counts) * 100
        h2 <- runmed(h2,21)

        h2min <- min(h2)
        h2max <- max(h2)
        # Linear, what we want
        h2map <- seq(h2min,h2max,(h2max-h2min)/(length(bins) - 1 - 1))

        f2 <- training[,eval(feature)] # Raw feature values
        f2 <- round(100*f2)/100
        f3 <- 0*f2
        ibins <- bins[-1]
        for (i in seq(1,length(f3))) {
            metric <- abs(ibins - f2[i])
            ii <- which(metric == min(metric))
            metric <- abs(h2map - h2[ii])
            ii <- which(metric == min(metric))
            f3[i] <- ibins[ii]
        }

        new_training[,eval(feature)] <- f3
    }

    return( new_training )
}
