% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OneDrive.R
\name{OneDrive_file}
\alias{OneDrive_file}
\title{Read files stored in an OneDrive}
\usage{
OneDrive_file(file, .function, ...)
}
\arguments{
\item{file}{File path}

\item{.function}{Function that will be used to reading the file}

\item{...}{\code{.function} args.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function reads files stored in an OneDrive
}
\examples{
 \dontrun{OneDrive_file("path/file.txt", read.delim, sep = "")}
}
