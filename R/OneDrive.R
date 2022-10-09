#' @title Read '.xlsx' files stored in an OneDrive
#' @description `r lifecycle::badge("experimental")`
#'
#' This function reads '.xlsx' files stored in an OneDrive
#' @param file '.xlsx' file path
#' @param ... [readxl::read_excel()] arguments
#' @note You may read several 'xlsx' sheets using \code{sheet = c('sheet1', 'sheet2')}
#' @examples
#' \dontrun{OneDrive_xlsx("path/file.xlsx")}
#' @export
OneDrive_xlsx <- function(file, ...) {
  dots_args = list(...)
  call_args = c(
    file = file,
    dots_args,
    where = "OneDrive"
  )
  formalArgs = formalArgs(download365_file)
  x = call_args[formalArgs]
  tempFile = do.call(
    download365_file,
    x[!sapply(x,is.null)]
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

#' @title Read '.csv' files stored in an OneDrive
#' @description `r lifecycle::badge("experimental")`
#'
#' This function reads '.csv' files stored in an OneDrive
#' @param file '.csv' file path
#' @param ... [read.csv()] arguments
#' @examples
#' \dontrun{OneDrive_csv("path/file.csv")}
#' @export
OneDrive_csv <- function(file, ...) {
  dots_args = list(...)
  call_args = c(
    file = file,
    dots_args,
    where = "OneDrive"
  )
  formalArgs = formalArgs(download365_file)
  x = call_args[formalArgs]

  tempFile = do.call(
    download365_file,
    x[!sapply(x,is.null)]
  )
  read.csv(tempFile, ...)
}

#' @title Read files stored in an OneDrive
#' @description `r lifecycle::badge("experimental")`
#'
#' This function reads files stored in an OneDrive
#' @param file File path
#' @param .function Function that will be used to reading the file
#' @param ... \code{.function} args.
#' @examples
#'  \dontrun{OneDrive_file("path/file.csv", read.delim, sep = ",")}
#' @export
OneDrive_file <- function(file, .function, ...) {
  dots_args = list(...)
  call_args = c(
    file = file,
    dots_args,
    where = "OneDrive"
  )
  formalArgs = formalArgs(download365_file)
  x = call_args[formalArgs]
  tempFile = do.call(
    download365_file,
    x[!sapply(x,is.null)]
  )
  dots_args <- c(tempFile, list(...))
  do.call(.function, dots_args)
}
