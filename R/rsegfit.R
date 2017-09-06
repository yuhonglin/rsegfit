##' Do segmentation on "data"
##'
##' "Segment and fit" a sequence. Each sequence will be
##' described by y=a*x^b+c where x=1:seg.length (order==0)
##' or x = seg.length:1 (order==1). The segmentation is
##' to minimise the sum of mse of each segments plus
##' smp*numSegment.
##' @title segfit a sequence
##' @param data The series to be segfitted
##' @param smp  The smaller, the more segments will be found
##' @param lb   lower bound of parameter $b$ of each segment
##' @param ub   upper bound of parameter $b$ of each segment
##' @param maxiter maximum iteration in optimisation
##' @param factr   maximum function evalution
##' @param pgtol   tolerance used in optimisation
##' @return A "segfit" object. It contains the params of all
##'         the segments. For each segment, it contains the
##'         head index (hi), tail index (ei), parameter $a$ (a)
##'         parameter $b$ (b), parameter $c$ (c), fitting order
##'         (order), fitted values (fit), and fitting residuals
##'         (residual). It also has a attribute "data" for the
##'         original data
##' @export 
segfit <- function(data, smp=2.3, lb=-6., ub=6., maxiter=1000, factr=5000, pgtol=10e-5) {
    p = .Call("segfitc", s=as.numeric(data), smp=as.numeric(smp),
                btype=2., lb=as.numeric(lb), ub=as.numeric(ub), m=15.,
                maxiter=as.numeric(maxiter), factr=as.numeric(factr), pgtol=pgtol)

    ## compute fitted value and residuals
    ret = list()
    ratio = max(data)/100

    for (i in 1:ncol(p)) {

        fitted   = rep(NA, p[2,i] - p[1,i] + 1)
        residual = rep(NA, p[2,i] - p[1,i] + 1)

        for (j in 1:(p[2,i] - p[1,i] + 1)) {
            fitted[j] = (p[3,i] * j^p[4,i] + p[5,i])*ratio
            if (i==1)
                residual[j]  = data[j] - fitted[j]
            else
                residual[j]  = data[p[2,i-1]+j] - fitted[j]
        }

        if (p[6,i] == 1) {
            fitted = rev(fitted)
            residual = rev(residual)
        }
        
        ret[[i]] = list(
            hi = p[1,i],
            ti = p[2,i],
            a = p[3,i]*ratio,
            b = p[4,i],
            c = p[5,i]*ratio,
            order = p[6,i],
            fit = fitted,
            res = residual
        )
    }

    ## also append the original sequence, use attr to
    ## make "length(ret) == #segments"
    attr(ret, "data") <- data 

    class(ret) <- "segfit"
    return (ret);
}

##' Plot a segfit object
##'
##' Plot a segfit object
##' @title Plot a segfit object
##' @param x If x is a segfit object, input y is ignored.
##' @param y A segfit object, If not provided, x must
##'        be a segfit object.
##' @param col.data color of data
##' @param col.seg  color of segments
##' @param legend.pos legend positions
##' @export 
plot.segfit <- function(x, y='', col.data='black', col.seg='red', legend.pos="topleft") {
    if (y == '') {
        y = x;
        x = 1:length(attr(y,"data"))
    }

    plot.default(x, attr(y, "data"), type="l", col=col.data)
    
    for (i in 1:length(y)) {
        lines(x[y[[i]]$hi:y[[i]]$ti], y[[i]]$fit, col=col.seg)
    }

    legend(x='topleft', legend=c("data", "segments"), lty=c(1,1), col=c('black', 'red'))
}

##' Summarise a segfit object
##'
##' Summarise a segfit object
##' @title Summarise a segfit object
##' @param sf The segfit object to summarise
##' @export 
summary.segfit <- function(sf) {
    cat(sprintf('Number of segments: %d\n', length(sf)))

    cat(sprintf('% 8s% 8s% 8s% 8s% 8s% 8s% 8s% 8s\n',
                'Segment', 'hi', 'ti', 'a', 'b', 'c', 'mse', 'var.res'))

    for (i in 1:length(sf)) {
        cat(sprintf('% 8d% 8d% 8d%8.3f%8.3f%8.3f%8.3f%8.3f\n',
                      i, sf[[i]]$hi, sf[[i]]$ti, sf[[i]]$a, sf[[i]]$b,
                      sf[[i]]$c, mean(sf[[i]]$res^2), var(sf[[i]]$res)))
    }
}
