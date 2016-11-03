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

PredictTournament <- function( tournament, fit, target ) {
    # 'fit' is output of gml(...), or randomForest(...), or any other model
    # 'target' is vector of training classifcations 0/1
    # 'tournament' is data frame with feature[1-XX]and t_id columns
    guess <- predict(fit)
    thresh <- FindThreshold(guess,target)
    out <- PredictProbability(tournament, fit, thresh)
    ll <- ValidateModel(training, fit, thresh)
    writeLines(paste("LogLoss is ", ll, sep=''))
    return(out)
}
