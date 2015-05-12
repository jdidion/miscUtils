# Convert a vector or table of values into a three (or more) column data frame: start,end,value.
# 'pos' is a vector of positions the same length as 'data. In each interval, 'data' has the same
# value at each position. If 'data' is a table, then the 'col' is used to determine the column
# of values to compare. If func is not NULL, it will be called with the subset of 'data' for each
# interval, and the expected return value is a vector that will be appended to the interval row.
as.intervals <- function(data, pos, col=1, func=NULL) {
    vec <- is.vector(data)
    if (vec) {
        v <- data
    }
    else {
        v <- data[,col]
    }
    n <- length(v)
    w <- c(0, which(v[2:n] != v[1:(n-1)]), n)
    start.idx <- w[1:(length(w)-1)] + 1
    end.idx <- w[2:length(w)]
    value <- v[start.idx]
    df <- data.frame(start=pos[start.idx], end=pos[end.idx], value=value)
    if (!vec & !is.null(func)) {
        df <- cbind(df, do.call(rbind, sapply(1:nrow(df),
            function(i) func(data[start.idx[i]:end.idx[i],]), simplify=FALSE)))
    }
    df
}

# Identify intervals of consecutive integers in a vector.
consecutive.as.intervals <- function(v) {
    v <- sort(v)
    d <- diff(v)
    w <- c(0, which(d > 1), length(v))
    start <- w[1:(length(w)-1)] + 1
    end <- w[2:length(w)]
    i <- as.data.frame(cbind(start=start, end=end, start.val=v[start], end.val=v[end]))
    i[i$end > i$start,]
}

# Collapse overlapping ranges. Expects a matrix or data
# frame with at least two columns: one defining the start
# of the range and the other defning the end.
compress.intervals <- function(x, start.col=2, end.col=3, val.cols=NULL) {
    if (nrow(x) < 2) {
        return(x)
    }
    na.rows <- c()
    for (i in 1:(nrow(x)-1)) {
        if (x[i, end.col] >= (x[i+1, start.col] - 1)) {
            if (is.null(val.cols) || x[i, val.cols] == x[i+1, val.cols]) {
                x[i+1, start.col] <- x[i, start.col]
                na.rows <- c(na.rows, i)
            }
        }
    }
    if (length(na.rows) > 0) {
        x <- x[-na.rows,]
    }
    return(x)
}

clip.intervals <- function(x, start, end, start.col=2, end.col=3) {
    w <- x[,start.col] < end & x[,end.col] > start
    x <- x[w,]
    if (nrow(x) > 0) {
        x[1, start.col] <- max(x[1, start.col], start)
        x[nrow(x), end.col] <- min(x[nrow(x), end.col], end)
    }
    x
}

make.windows <- function(start, end, size, slide) {
    last <- ceiling((end - size) / slide) * slide
    starts <- seq(start, last, slide)
    Intervals(cbind(starts, starts + size), closed=c(TRUE, FALSE))
}
