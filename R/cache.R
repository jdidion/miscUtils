# Simple cache class.
#
# Given a file name and an expression, evaluates the expression only if the file
# doesn't exist or the MD5 digest of the expression is different than the
# previous call with the same arguments. After evaluation, all variables set in
# the expression (i.e. the local environment) are stored to the output file.
#
# This is useful in scripts that you are developing and will be repeatedly
# executing, but you don't want to re-execute the same code over and over.
#
# @param file
# @param expr
# @param env the environment in which expr will be
# evaluated, and which will be cached (and loaded on
# subsequent calls).
# @param add.to.parent whether to copy the values from the
# created/loaded environment into the parent environment.
#
# @return new environment (invisibly)
#
# @example
# cache <- Cache$new(parent.env=environment())
# z <- cache$cache(
#     "results.RData",
#     expression({ print("evaluating") ; x=1 ; y=2 ; x+y }))
# => "evaluating"
# # The variables added to the environment within the expression are injected
# # into the parent environment by default.
# print(paste(x,y,z))
# => "1 2 3"
# # To not inject variables into the parent environment:
# result <- cache$cache(
#     "results.RData",
#     expression({ print("evaluating") ; x=1 ; y=2 ; x+y }),
#     add.to.parent=F)
# # Result is a list with the environment and the return value
# print(get('x', envir=result$env), get('y', envir=result$env))
# "1 2"
# print(result$result)
# => "3"
# # not evaluated the second time
# cache$cache(
#     "results.RData",
#     expression({ print("evaluating") ; x=1 ; y=2 ; x+y }))
# # evaluated because the file doesn't exist
# unlink("results.RData)
# cache$cache(
#     "results.RData",
#     expression({ print("evaluating") ; x=1 ; y=2 ; x+y }))
# => "evaluating"
# # evaluated because code changed
# cache$cache(
#     "results.RData",
#     expression({ print("evaluating") ; list(x=10, y=-1) }))
# => "evaluating"
# # Use 'cachevar' to get only the return value of the expression or the value
# # of a specific variable (values are not injected into the parent environment)
# z <- cache$cachevar('results.RData', expression({
#     x <- 2 * 5
#     y <- 6 / 3
#     x + y
# }))
# x <- cache$cachevar('results.RData', expression({
#     x <- 2 * 5
#     y <- 6 / 3
#     x + y
# }), 'x')
# print(x, z)
# => "10, 12"
Cache <- setRefClass("Cache",
    fields=list(parent.env="environment", cache.dir="character",
                digest="logical", store="ANY", store.file="character",
                eval.missing.hash="logical"),
    methods=list(
        initialize=function(parent.env, cache.dir=".",
                            digest=library(digest, logical.return=TRUE,
                                           quietly=TRUE),
                            store=NULL, store.file="~/.RHashStore",
                            eval.missing.hash=TRUE) {
            callSuper(parent.env=parent.env, cache.dir=cache.dir, digest=digest,
                      store=store, store.file=store.file,
                      eval.missing.hash=eval.missing.hash)
            if (!dir.exists(.self$cache.dir)) {
                dir.create(.self$cache.dir)
            }
            .self$.init.hash.store()
        },
        cache=function(file, expr, env=new.env(), add.to.parent=TRUE,
                       force=FALSE) {
            file <- file.path(.self$cache.dir, file)
            evl <- force || !file.exists(file)
            hash <- NULL
            if (!evl && .self$digest) {
                hash <- digest(expr)
                if (file %in% names(.self$store)) {
                    evl <- (.self$store[[file]] != hash)
                }
                else if (.self$eval.missing.hash) {
                    evl <- TRUE
                }
            }
            if (evl) {
                result <- eval(expr, envir=env)
                save(env, result, file=file)
                if (.self$digest) {
                    if (is.null(hash)) {
                        hash <- digest(expr)
                    }
                    .self$store[[file]] <- hash
                    .self$.save.hash.store()
                }
            }
            else {
                load(file)
            }
            if (add.to.parent) {
                for (n in ls(env)) {
                    assign(n, get(n, envir=env), envir=.self$parent.env)
                }
                result
            }
            else {
                invisible(list(env=env, result=result))
            }
        },
        cachevar=function(file, expr, var.name=NULL) {
            result <- .self$cache(file, expr, add.to.parent=FALSE)
            if (is.null(var.name)) {
                result$result
            }
            else {
                get(var.name, envir=result$env)
            }
        },
        .init.hash.store=function() {
            if (is.null(.self$store)) {
                if (!is.null(.self$store.file)) {
                    if (file.exists(.self$store.file)) {
                        env <- new.env()
                        load(.self$store.file, envir=env)
                        .self$store <- get("hash.store", envir=env)
                    }
                    else {
                        .self$store <- list()
                    }
                }
            }
        },
        .save.hash.store=function() {
            if (!is.null(.self$store.file)) {
                hash.store <- .self$store
                save(hash.store, file=.self$store.file)
            }
        }
    )
)

load.cached <- function(file, var.names=NULL, par.env=parent.frame()) {
    load(file)
    if (is.null(var.names)) {
        var.names <- ls(env)
    }
    for (n in var.names) {
        assign(n, get(n, envir=env), envir=par.env)
    }
    var.names
}
