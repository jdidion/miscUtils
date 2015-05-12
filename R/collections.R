
# Safely apply a function to either a vector or table.
safe.apply <- function(data, dim, func) {
    d <- dim(data)
    if (is.null(d) || any(d <= 1)) {
        return(func(data))
    }
    else {
        return(apply(data, dim, func))
    }
}

# Reduce a vector to a scalar by iteratively applying a function to the two left-most
# elements in a vector.
reduce <- function(fn, args) {
    left <- args[[1]]
    for (right in 2:length(args)) {
        left <- fn(left, args[[right]])
    }
    return(left)
}

# Filter and rename a data frame to match a required format. Borrowed from ggplot2.
mapping <- function(...) {
    structure(as.list(match.call()[-1]), class="uneval")
}
eval.mapping <- function(data, mapping) {
    args <- Filter(Negate(is.null), lapply(mapping, function(x) eval(x, data)))
    args$stringsAsFactors <- FALSE
    do.call(data.frame, args)
}

# Split a matrix based on a column, apply a function that
# must return a vector, and cbind the results.
split.df <- function(m, col, fn, comb.fn=cbind) {
    v <- m[,col]
    f <- factor(v)
    l <- vector(mode="list", length=length(levels(f)))
    names(l) <- as.character(levels(f))
    for (val in levels(f)) {
        w <- f == val
        l[[val]] <- fn(m[w,])
    }
    do.call(cbind, l)
}