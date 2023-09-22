fetch_datasets <-
function(dataset,
         dest.dir = NULL,
         detect.dates = TRUE, ...) {

    ds <- c(
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
        unk <- dataset[m == 0]
        warning("unknown dataset",
                if (length(unk > 1L)) "s",
                ": ", paste(unk, collapse = ", "))
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

    if (is.null(dest.dir))
        dest.dir <- tempdir(TRUE)

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
            res <- read_dataset(f.path, detect.dates = detect.dates)
        }
    }

    if (length(datasets) == 1)
        res
    else
        invisible(NULL)
}



compare_datasets <-
function(filename.old,
         filename.new,
         match.column = "GrantNumber",
         ...) {

    old <- read.table(filename.old,
                      header = TRUE,
                      sep = ";",
                      quote = '"',
                      comment.char = "",
                      as.is = TRUE,
                      ...)

    new <- read.table(filename.new,
                      header = TRUE,
                      sep = ";",
                      quote = '"',
                      comment.char = "",
                      as.is = TRUE,
                      ...)

    on <- match(new[[match.column]],
                old[[match.column]],
                nomatch = 0)
    key.new <- apply(new[on > 0, ], 1,
                     function(x) paste(x, collapse = "--"))
    key.old <- apply(old[on, ], 1,
                     function(x) paste(x, collapse = "--"))

    changes <- which(key.new != key.old)

    ans.changes <- list()
    new. <- new[on > 0, ]
    old. <- old[on, ]
    for (ch in changes) {
        gn <- new.[ch, match.column]
        same <- (is.na(new.[ch, ])  &  is.na(old.[ch, ])) |
                 new.[ch, ] == old.[ch, ]
        ch.col <- colnames(new)[!same]
        o.n <- cbind(old = t(old.[ch, ch.col]),
                     new = t(new.[ch, ch.col]))
        row.names(o.n) <- ch.col
        colnames (o.n) <- c("old", "new")
        ans.changes[[as.character(gn)]] <- o.n
    }

    list(added   = new[!new[[match.column]] %in% old[[match.column]], ],
         removed = old[!old[[match.column]] %in% new[[match.column]], ],
         changed = ans.changes)
}



read_dataset <- function(filename,
                         detect.dates = TRUE,
                         ...) {

    if (is.logical(detect.dates)) {
        date.rx <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
    } else if (is.character(detect.dates)) {
        date.rx <- detect.dates
        detect.dates <- TRUE
    }

    res <- read.table(filename,
                      header = TRUE,
                      sep = ";",
                      quote = '"',
                      comment.char = "")

    if (detect.dates) {
        date <- which(
            apply(res, 2, function(x)
                all(grepl(date.rx, x, perl = TRUE))))
        for (d in date) {
            res[[d]] <- as.Date(res[[d]])
        }
    }
    res
}
