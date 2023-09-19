\name{SNSFdatasets}
\alias{SNSFdatasets-package}
\alias{SNSFdatasets}
\alias{compare_datasets}
\alias{fetch_datasets}
\alias{read_dataset}
\title{
  Download Datasets from the Swiss National Science Foundation
}
\description{
  Download datasets from the Swiss National Science Foundation
  (SNF, FNS, SNSF) in CSV format.
}
\usage{
fetch_datasets(dataset, dest.dir = NULL, detect.dates = TRUE, ...)
compare_datasets(dataset.old, dataset.new,
                 match.column = "GrantNumber", ...)

read_dataset(filename, detect.dates = TRUE, ...)
}
\arguments{
  \item{dataset}{%
    a character vector. When of length greater than one,
    datasets are only downloaded, but not read.  Currently
    supported are \code{Grant}, \code{GrantWithAbstracts},
    \code{Person}, \code{OutputdataScientificPublication},
    \code{OutputdataUseInspired},

    \code{OutputdataPublicCommunication},
    \code{OutputdataCollaboration},

    \code{OutputdataAcademicEvent}, \code{OutputdataAward},
    \code{OutputdataDataSet} and

    \code{OutputdataKnowledgeTransferEvent}.
  }
  \item{dest.dir}{%
    a directory; if \code{NULL}, a \code{\link{tempdir}} is used.
  }
  \item{detect.dates}{%
    logical: if \code{TRUE}, columns consisting of entries such as
    \code{2000-10-31T00:00:00Z} are converted to \code{\link{Date}}
  }
  \item{dataset.old}{%
    string: the filename
  }
  \item{dataset.new}{%
    string: the filename
  }
  \item{filename}{%
    string: the filename
  }
  \item{match.column}{%
    string: the name of the column to use for matching
    entries in old and new file
  }
  \item{\dots}{%
    arguments to be passed to \code{\link{download.file}}
  }
}
\details{

  \code{fetch_datasets} downloads datasets in CSV format from
  the SNSF's website and stores them, with a date prefix, in
  directory \code{dest.dir}.  If the latter is NULL, a
  temporary directory is used (through \code{\link{tempdir}});
  but much better is to use a more-persistant storage location.
  If a file with today's date exists in \code{dest.dir}, that
  file is read, and nothing is downloaded.  If more than one
  \code{dataset} is specified, those files are downloaded (if
  not current in \code{dest.dir}) but not read.

  For downloading, function \code{\link{download.file}} is
  used.  If it fails, \code{fetch_datasets} returns
  \code{NULL}.  Settings can be passed via \dots .  See
  \code{\link{download.file}} for options; in particular, see
  the hints about \code{timeout}.

  \code{compare_datasets} will match old and new \code{dataset} via
  column \code{GrantNumber} and report
  \itemize{
    \item added lines (in new, but not in old),
    \item removed lines (in old, but not in new), and
    \item changed lines (in old and new, but with differing content).
  }

}
\value{%
  A \code{\link{data.frame}} for \code{fetch_datasets} and
  {read_dataset}. For \code{compare_datasets}, a
  \code{\link{list}} of three components named \code{added},
  \code{removed} and \code{changed}.
}
\references{%
  \url{https://data.snf.ch/datasets}
}
\author{
  Silvia Martens and Enrico Schumann
}
\seealso{%
  \code{\link{download.file}}; \code{\link{options}}
  (\code{timeout}, in particular)
}
\examples{
\donttest{
## requires internet connection, and file may be large
dataset <- "Grant"

SNSF.dir  <- tempdir()  ## this is just an example.
                       ## In practice it's much more useful to
                       ## store files in a persistant location,
                       ## such as '~/Downloads/SNSFdatasets'.

data <- fetch_datasets(dataset = dataset, dest.dir = SNSF.dir)

## all research institutions
table(data[["ResearchInstitution"]])

## grants to University of Lucerne
data[data[["ResearchInstitution"]] == "University of Lucerne - LU", ]

## all keywords
kw <- sort(unique(unlist(strsplit(data$Keywords, " *; *"))))
}
}
