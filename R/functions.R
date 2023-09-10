fetch_datasets <-
function(dataset,
         dest.dir = NULL, ...) {

    ds <-  c(
        "Grant",
        "GrantWithAbstracts",
        "Person",
        "OutputdataScientificPublication",
        "OutputdataUseInspired",
        "OutputdataPublicCommunication",
        "OutputdataCollaboration",
        "OutputdataAcademicEvent",
        "OutputdataAward",
        "OutputdataDataSet",
        "OutputdataKnowledgeTransferEvent"
    )


    m <- match(tolower(dataset), tolower(ds), nomatch = 0)
    if (any(m == 0)) {
        message("unknown dataset ", dataset[m == 0])
    }
    dataset <- ds[m]

    if (!is.null(dest.dir) && !dir.exists(dest.dir)) {
        create.dir <- askYesNo(
            paste(sQuote("dest.dir"), "does not exist. Create it?"),
            default = FALSE)
        if (!isTRUE(create.dir))
            return(invisible(NULL))
        dir.create(dest.dir, recursive = TRUE)
    }

    date.rx <- "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z$"
    datasets <- dataset
    for (dataset in datasets) {

        f.name <- paste0(format(Sys.Date(), "%Y%m%d_"), dataset, ".csv")

        dataset <- paste0("https://data.snf.ch/exportcsv/", dataset, ".csv")
        f.path <- file.path(normalizePath(dest.dir), f.name)

        if (!file.exists(f.path)) {
            dl.result <- try(download.file(dataset, f.path, ...), silent = TRUE)
            if (inherits(dl.result, "try-error")) {
                warning("download failed with message ", sQuote(dl.result, FALSE))
                return(invisible(NULL))
            }
        } else
            dl.result <- 0

        if (dl.result != 0L) {
            warning("download failed with code ", dl.result, "; see ?download.file")
            return(invisible(NULL))
        }

        if (length(datasets) == 1) {
            res <- read.table(f.path,
                              header = TRUE,
                              sep = ";",
                              quote = '"',
                              comment.char = "")

            date <- which(
                apply(res, 2, function(x)
                    all(grepl(date.rx, x, perl = TRUE))))
            for (d in date) {
                res[[d]] <- as.Date(res[[d]])
            }

        }
    }

    if (length(datasets) == 1)
        res
    else
        invisible(NULL)
}

compare_datasets <-
function(dataset.old,
         dataset.new, ...) {

    old <- read.table(dataset.old,
                      header = TRUE,
                      sep = ";",
                      quote = '"',
                      comment.char = "",
                      ...)

    new <- read.table(dataset.new,
                      header = TRUE,
                      sep = ";",
                      quote = '"',
                      comment.char = "",
                      ...)

    on <- match(new$GrantNumber, old$GrantNumber, nomatch = 0)
    key.new <- apply(new[on > 0, ], 1,
                     function(x) paste(x, collapse = "--"))
    key.old <- apply(old[on, ], 1,
                     function(x) paste(x, collapse = "--"))

    changes <- which(key.new != key.old)

    ans.changes <- list()
    for (ch in changes) {
        gn <- new[on > 0, ][ch, "GrantNumber"]
        ch.col <- colnames(new)[new[on > 0, ][ch, ] != old[on, ][ch, ]]
        o.n <- cbind(old = t(old[on, ][ch, ch.col]),
                     new = t(new[on > 0, ][ch, ch.col]))
        row.names(o.n) <- ch.col
        ans.changes[[as.character(gn)]] <- o.n
    }

    ans <- list(added   = new[!new$GrantNumber %in% old$GrantNumber, ],
                removed = old[!old$GrantNumber %in% new$GrantNumber, ],
                changed = ans.changes)
    ans
}
