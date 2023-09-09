\name{fetch_datasets}
\alias{fetch_datasets}
\alias{SNFdatasets-package}
\alias{SNFdatasets}
\title{
Download Datasets from the Swiss National Science Foundation
}
\description{
  Download datasets from the Swiss National Science Foundation (SNF,
     FNS, SNSF) in CSV format.
}
\usage{
fetch_datasets(dataset, dest.dir = NULL, ...)
}
\arguments{
  \item{dataset}{
    a character vector
}
  \item{dest.dir}{
    a directory; if \code{NULL}, a \code{\link{tempdir}} is used.
}
  \item{\dots}{
    currently not used
}
}
\details{

  Datasets in CSV format are downloaded via \code{\link{download.file}}
  
}
\value{
  a \code{\link{data.frame}}
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
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
library("SNFdatasets")
data <- "Grant"
SNF.dir  <- "~/Downloads/SNFdatasets"
fetch_datasets(dataset = data, dest.dir = SNF.dir)
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
