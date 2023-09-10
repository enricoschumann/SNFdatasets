\name{SNFdatasets}
\alias{SNFdatasets-package}
\alias{SNFdatasets}
\alias{compare_datasets}
\alias{fetch_datasets}
\title{
  Download Datasets from the Swiss National Science Foundation
}
\description{
  Download datasets from the Swiss National Science Foundation
  (SNF, FNS, SNSF) in CSV format.
}
\usage{
fetch_datasets(dataset, dest.dir = NULL, ...)
compare_datasets(dataset.old, dataset.new, ...)
}
\arguments{
  \item{dataset}{%
    a character vector. When of length greater than one,
    datasets are only downloaded, but not parsed.  Currently
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
  \item{dataset.old}{
    string: the filename
  }
  \item{dataset.new}{
    string: the filename
  }
  \item{\dots}{
    arguments to be passed to \code{\link{download.file}}
  }
}
\details{

  \code{fetch_datasets} downloads datasets in CSV format from
  the SNF's website and stores them, with a date prefix, in
  directory \code{dest.dir}.  If the latter is NULL, a
  temporary directory is used (through \code{\link{tempdir}});
  but much better is to use a more-permanent storage location.
  If a file with today's date exists in \code{dest.dir}, that
  file is read, and nothing is downloaded.

  For downloading, function \code{\link{download.file}} is
  used.  If it fails, \code{fetch_datasets} returns
  \code{NULL}.

  \code{compare_datasets} will match old and new dataset via
  column \code{GrantNumber} and report
  \itemize{
    \item added lines (in new, but not in old),
    \item removed lines (in old, but not in new), and
    \item changed lines (in old and new, but with differing content).
  }

}
\value{%
  A \code{\link{data.frame}} for \code{fetch_datasets}; and
  for \code{compare_datasets} a list of three components
  named \code{added}, \code{removed} and \code{changed}.
}
\references{
  \url{https://data.snf.ch/datasets}
}
%% \author{
%% %%  ~~who you are~~
%% }
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
\donttest{
## requires internet connection
dataset <- "Grant"
SNF.dir  <- "~/Downloads/SNFdatasets"
data <- fetch_datasets(dataset = dataset, dest.dir = SNF.dir)

## all research institutions
table(data[["ResearchInstitution"]])

## grants to University of Lucerne
data[data[["ResearchInstitution"]] == "University of Lucerne - LU", ]

## keywords
sort(unique(unlist(strsplit(g$Keywords, " *; *"))))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
