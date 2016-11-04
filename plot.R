library(splines)
training   <- read.csv('numerai_training_data.csv')
tournament <- read.csv('numerai_tournament_data.csv')

target_0 <- which(training$target == 0)
target_1 <- which(training$target == 1)

significance <- 0.05
rows <- dim(training)[1]

# Plots of the features
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
    h2 <- h1$counts / (h1$counts + h0$counts) * 100
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

    png_filename=paste(feature,"_percent_bin",sep="")
    png(file=png_filename)
    ylim <- c(40,60)
    plot(c(0,1),ylim, main=paste(feature, "_percent_bin.png"),
         col="#FFFFFFFF", ylab="Percent Target -> 1", xlab="Feature Value",
         ylim=ylim,xlim=c(0,1))
    dz <- 1
    mid <- 50
    zz <- which(mid - dz < h2 & h2 < mid + dz)
    z0 <- which(h2 < mid - dz)
    z1 <- which(h2 > mid + dz)
    lines(c(0,1),c(50,50))
    points(bins[-1][zz],h2[zz], pch=20, cex=1, col="green")
    points(bins[-1][z0],h2[z0], pch=20, cex=1, col="red")
    points(bins[-1][z1],h2[z1], pch=20, cex=1, col="blue")
    text(0.25, ylim[2], "Target -> 0 more likely", col="red", cex=0.7)
    text(0.50, ylim[2], "Neutral", col="green", cex=0.7)
    text(0.75, ylim[2], "Target -> 1 more likely", col="blue", cex=0.7)
    dev.off()

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

    g0 <- f3[target_0]
    g1 <- f3[target_1]

    hh0 <- hist(g0,bins)
    hh1 <- hist(g1,bins)
    hh2 <- hh1$counts / (hh1$counts + hh0$counts) * 100

    png_filename=paste(feature,"_percent_bin_adj",sep="")
    png(file=png_filename)
    ylim <- c(40,60)
    plot(c(0,1),ylim, main=paste(feature, "_percent_bin_adj.png"),
         col="#FFFFFFFF", ylab="Percent Target -> 1", xlab="Feature Value",
         ylim=ylim,xlim=c(0,1))
    dz <- 1
    mid <- 50
    zz <- which(mid - dz < hh2 & hh2 < mid + dz)
    z0 <- which(hh2 < mid - dz)
    z1 <- which(hh2 > mid + dz)
    lines(c(0,1),c(50,50))
    points(bins[-1][zz],hh2[zz], pch=20, cex=1, col="green")
    points(bins[-1][z0],hh2[z0], pch=20, cex=1, col="red")
    points(bins[-1][z1],hh2[z1], pch=20, cex=1, col="blue")
    text(0.25, ylim[2], "Target -> 0 more likely", col="red", cex=0.7)
    text(0.50, ylim[2], "Neutral", col="green", cex=0.7)
    text(0.75, ylim[2], "Target -> 1 more likely", col="blue", cex=0.7)
    dev.off()
}
