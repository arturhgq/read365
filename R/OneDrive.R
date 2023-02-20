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
#'  \dontrun{OneDrive_file("path/file.txt", read.delim, sep = "")}
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

#' @title Download and read an OneDrive file from a shared url
#' @description
#' This function downloads and reads an OneDrive file from a shared url.
#' @param url A shared url from OneDrive.
#' @param .function Function that will be used to reading the file.
#' @param ... .function args.
#' @examples
#'  url = 'https://arturhgq-my.sharepoint.com/:x:/p/contact/EZ_KJ3cqtIVEh4PdXhTGy7IBpWT5_Zlp2VjYwlgVCPK4oQ?e=UZGQZt'
#'
#'  OneDrive_shared_file(
#'     url,
#'     .function = readxl::read_excel
#'   )
#' @references Based on https://github.com/PaulMelloy/Download_from_OneDrive
#' @export

OneDrive_shared_file <- function(url, .function, ...){
  if (length(url) > 1) {
    cli::cli_abort(
      "Use `lapply(urls, function(x) Read_shared_file(...))` to read more than one file at once"
    )
  }
  url_split = unlist(strsplit(url,"[?]"))[1]
  url = paste0(url_split,"?download=1")

  tempfile = tempfile()
  httr::GET(url, httr::write_disk(path = tempfile, overwrite = TRUE))

  dots_args = c(tempfile, list(...))
  do.call(.function, dots_args)
}

