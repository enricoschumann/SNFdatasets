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

        if (length(datasets) == 1)
            res <- read.table(f.path,
                              header = TRUE,
                              sep = ";",
                              quote = '"',
                              comment.char = "")
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

    ans <- list(added   = new[!new$GrantNumber %in% old$GrantNumber, ],
                removed = old[!old$GrantNumber %in% new$GrantNumber, ],
                changed = list(NA))
    ans
}
