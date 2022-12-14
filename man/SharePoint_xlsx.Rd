% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SharePoint.R
\name{SharePoint_xlsx}
\alias{SharePoint_xlsx}
\title{Read '.xlsx' files stored in a SharePoint}
\usage{
SharePoint_xlsx(SharePoint, drive, file, ...)
}
\arguments{
\item{SharePoint}{url}

\item{drive}{SharePoint drive}

\item{file}{'.xlsx' file path}

\item{...}{\code{\link[readxl:read_excel]{readxl::read_excel()}} arguments}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function reads '.xlsx' files stored in a SharePoint
}
\note{
You may read several 'xlsx' sheets using \code{sheet = c('sheet1', 'sheet2')}
}
\examples{
\dontrun{
SharePoint_xlsx(
  SharePoint = "https://page.sharepoint.com",
  drive = "Documents",
  file = "Book.xlsx",
  sheet = c("Sheet1", "Sheet2")
  )
}
}
