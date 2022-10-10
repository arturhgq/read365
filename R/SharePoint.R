#' @title Get the drive names of a Sharepoint page
#' @description `r lifecycle::badge("experimental")`
#'
#' This function gets the drive names of a Sharepoint page
#' @param SharePoint [Microsoft365R::get_sharepoint_site()]
#' @export
get_drives <- function(SharePoint) {
  drives = SharePoint$list_drives()
  sapply(
    seq_along(drives),
    function(x) drives[[x]][["properties"]][["name"]]
  )
}

#' @title Read '.xlsx' files stored in a SharePoint
#' @description `r lifecycle::badge("experimental")`
#'
#' This function reads '.xlsx' files stored in a SharePoint
#' @param SharePoint url
#' @param drive SharePoint drive
#' @param file '.xlsx' file path
#' @param ... [readxl::read_excel()] arguments
#' @note You may read several 'xlsx' sheets using \code{sheet = c('sheet1', 'sheet2')}
#' @examples
#' \dontrun{
#' SharePoint_xlsx(
#'   SharePoint = "https://page.sharepoint.com",
#'   drive = "Documents",
#'   file = "Book.xlsx",
#'   sheet = c("Sheet1", "Sheet2")
#'   )
#' }
#' @export
SharePoint_xlsx <- function(SharePoint, drive, file, ...) {
  dots_args = list(...)
  fun_args = as.list(match.call())
  formal_args = formalArgs(download365_file)
  call_args = c(
    fun_args,
    where = "SharePoint"
  )
  tempFile = do.call(
    download365_file,
    call_args[formal_args]
  )
  if (any(names(dots_args) == "sheet")) {
    read_args = c(
      path = tempFile,
      within(dots_args, rm("sheet"))
    )
    mapply(
      readxl::read_excel,
      sheet = dots_args[["sheet"]],
      MoreArgs = read_args,
      SIMPLIFY = FALSE
    ) -> data
  } else {
    readxl::read_excel(tempFile, ...) -> data
  }
  if (length(data) == 1) data[[1]]
  else(data)
}

#' @title Read '.csv' files stored in a SharePoint
#' @description `r lifecycle::badge("experimental")`
#'
#' This function reads '.csv' files stored in a SharePoint
#' @param SharePoint url
#' @param drive SharePoint drive
#' @param file '.csv' file path
#' @param ... [read.csv()] arguments
#' @examples
#' \dontrun{
#' SharePoint_csv(
#'   SharePoint = "https://page.sharepoint.com",
#'   drive = "Documents",
#'   file = "Book.csv"
#'   )
#' }
#' @export
SharePoint_csv <- function(SharePoint, drive, file, ...) {
  fun_args = as.list(match.call())
  call_args = c(
    fun_args,
    where = "SharePoint"
  )
  formal_args = formalArgs(download365_file)
  tempFile = do.call(
    download365_file,
    call_args[formal_args]
  )
  read.csv(tempFile, ...)
}

#' @title Read files stored in a SharePoint
#' @description `r lifecycle::badge('experimental')`
#'
#' This function reads files stored in a SharePoint
#' @param SharePoint url
#' @param drive SharePoint drive
#' @param file File path
#' @param .function Function that will be used to reading the file
#' @param ... \code{.function} args
#' @examples
#' \dontrun{
#' SharePoint_file(
#'   SharePoint = "https://page.sharepoint.com",
#'   drive = "Documents",
#'   file = "Book.txt",
#'   read.delim,
#'   sep = ""
#'   )
#' }
#' @export
SharePoint_file <- \(SharePoint, drive, file, .function, ...) {
  fun_args = as.list(match.call())
  call_args = c(
    fun_args,
    where = "SharePoint"
  )
  formal_args = formalArgs(download365_file)
  tempFile = do.call(
    download365_file,
    call_args[formal_args]
  )
  dots_args <- c(tempFile, list(...))
do.call(.function, dots_args)
}
