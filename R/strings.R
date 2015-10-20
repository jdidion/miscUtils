str_format <- function(x, ...) {
    args <- list(...)
    parts.vec <- stringr::str_split(x, "[\\{\\}]")
    unlist(lapply(parts.vec, function(parts) {
        nparts <- length(parts)
        if (nparts == 1) {
            x
        }
        else {
            offset <- ifelse(stringr::str_sub(x, 1, 1) == "{", 1, 2)
            part.idx <- seq(offset, nparts, 2)
            key.fmt <- stringr::str_split_fixed(parts[part.idx], ":", n=2)
            parts[part.idx] <- sapply(1:nrow(key.fmt), function(i) {
                if (key.fmt[i,1] %in% names(args)) {
                    val <- args[[key.fmt[i,1]]]
                }
                else {
                    val <- args[[as.integer(key.fmt[i,1])]]
                }
                if (key.fmt[i,2] != "") {
                    val <- sprintf(paste0("%", key.fmt[i,2]), val)
                }
                val
            })
            paste(parts, collapse="")
        }
    }))
}