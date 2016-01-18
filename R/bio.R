# requires: readxl, WriteXLS, and permute

# Make a plate template with the given number of rows and columns.
# If controls is non-NULL, the names should list coordinates (with
# rows having letter names and columns having numbers) and the
# values should be the name of the control sample to place in the well.
make.template <- function(nrow, ncol, controls=NULL) {
    templ <- matrix(NA, nrow=nrow, ncol=ncol, 
        dimnames=list(sapply(65:(65+nrow-1), intToUtf8), 1:ncol))
    if (!is.null(controls)) {
        rows <- substr(names(controls), 1, 1)
        cols <- substring(names(controls), 2)
        for (i in 1:length(controls)) {
            templ[rows[i], cols[i]] <- controls[[i]]
        }
    } 
    templ
}

cidr.controls <- rep("BLANK", 4)
names(cidr.controls) <- c("B4", "F10", "H4", "H8")
cidr.template <- make.template(8, 12, cidr.controls)

# Make plates from IDs. If ids is a list, each element is
# expected to be a vector of IDs, and IDs will be randomized
# within each plate. Otherwise, all IDs are randomized across
# all plates.
make.plates <- function(ids, template) {
    open.wells <- is.na(template)
    nwells <- sum(open.wells)
    
    make.plate <- function(plate.ids) {
        if (length(plate.ids) < nwells) {
            plate.ids <- c(plate.ids, rep("EMPTY", nwells - length(plate.ids)))
        }
        
        plate.matrix <- template
        plate.matrix[open.wells] <- plate.ids
        
        list(manifest=data.frame(SampleID=as.vector(plate.matrix), Position=apply(
                expand.grid(rownames(plate.matrix), 
                            sprintf("%02d", as.integer(colnames(plate.matrix)))),
                1, paste, collapse=""), stringsAsFactors=FALSE),
             layout=as.data.frame(plate.matrix))
    }
    
    if (is.list(ids)) {
        plates <- lapply(plates, function(plate.ids) {
            plate.ids <- plate.ids[permute::shuffle(length(ids))]
            make.plate(plate.ids)
        })
    }
    else {
        ids <- ids[permute::shuffle(length(ids))]
        nids <- length(ids)
        plates <- lapply(seq(1, nids, nwells), function(i) {
            make.plate(ids[i:min(nids, i+nwells-1)])
        })
    }
    
    names(plates) <- paste0("Plate", 1:length(plates))
    plates
}

# Mate plates from IDs in an excel sheet.
# path - path of excel sheet
# template - plate template created by make.template
# id.col - column with sample IDs
# ... additional arguments to pass to readxl::read_excel
make.plates.from.excel <- function(input.path, template, layout.path=NULL, manifest.path=NULL, id.col=1, ...) {
    ids <- readxl::read_excel(input.path, ...)[,id.col]
    plates <- make.plates(ids, template)
    if (!is.null(layout.path)) {
        WriteXLS::WriteXLS(lapply(plates, "[[", "layout"), layout.path, row.names=TRUE, col.names=TRUE)
    }
    if (!is.null(manifest.path)) {
        WriteXLS::WriteXLS(lapply(plates, "[[", "manifest"), manifest.path, row.names=FALSE, col.names=TRUE)
    }
}

make.cidr.plates <- function(xls.path, ...) {
    prefix <- sub("[.][^.]*$", "", xls.path) 
    make.plates.from.excel(xls.path, cidr.template, 
        paste0(prefix, "_layout.xls"), 
        paste0(prefix, "_manifest.xls"))
}