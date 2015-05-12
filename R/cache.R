# Simple cache class. 
#
# Given a file name and an expression,
# evaluates the expression only if the file doesn't exist or
# the MD5 digest of the expression is different than the
# previous call with the same arguments. After evaluation, 
# all variables set in the expression (i.e. the local 
# environment) are stored to the output file.
#
# This is useful in scripts that you are developing 
# and will be repeatedly executing, but you don't 
# want to re-execute the same code over and over.
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
# cache$cache("results.RData", expression({ print("evaluating") ; list(x=1, y=2) }))
# => "evaluating"
# print(paste(x,y))
# => "1 2"
# cache$cache("results.RData", expression({ print("evaluating") ; list(x=1, y=2) })) # not evaluated the second time
# unlink("results.RData)
# cache$cache("results.RData", expression({ print("evaluating") ; list(x=1, y=2) })) # evaluated because the file doesn't exist
# => "evaluating"
# cache$cache("results.RData", expression({ print("evaluating") ; list(x=10, y=-1) })) # evaluated because code changed
# => "evaluating"
Cache <- setRefClass("Cache", 
    fields=list(parent.env="environment", digest="logical", store="ANY", store.file="character", 
                eval.missing.hash="logical"), 
    methods=list(
        initialize=function(parent.env, digest=library(digest, logical.return=TRUE, quietly=TRUE), 
                            store=NULL, store.file="~/.RHashStore", eval.missing.hash=TRUE) {
            callSuper(parent.env=parent.env, digest=digest, store=store, store.file=store.file,
                      eval.missing.hash=eval.missing.hash)
            .self$.init.hash.store()
        },
        cache=function(file, expr, env=new.env(), add.to.parent=TRUE, force=FALSE) {
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
                eval(expr, envir=env)
                save(env, file=file)
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
            }
            invisible(env)
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

load.cached <- function(file, par.env=parent.frame()) {
    load(file)
    var.names <- ls(env)
    for (n in ls(env)) {
        assign(n, get(n, envir=env), envir=par.env)
    }
    var.names
}
