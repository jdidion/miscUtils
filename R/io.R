# Convenience function for appending lines to a file.
writeln <- function(f, str, newline=TRUE) {
    cat(str, file=f, append=TRUE)
    if (newline) {
        cat("\n", file=f, append=TRUE)
    }
}

# Load an R object file and return a specific object (or a list of all objects if name == NA)
# without affecting the current environment.
my.load <- function(f, obj.names=NULL, return.list=F) {
    e <- baseenv()
    if (is.null(obj.names)) {
        obj.names <- load(f, envir=e)
    }
    else {
        load(f, envir=e)
    }
    if (length(obj.names) > 1 || return.list) {
        return(mget(obj.names, envir=e))
    }
    else {
        return(get(obj.names[1], envir=e))
    }
}

my.save.image <- function (file) {
    .Internal(saveToConn(names(.GlobalEnv), gzfile(file, "wb"), FALSE, NULL, .GlobalEnv, TRUE))
}