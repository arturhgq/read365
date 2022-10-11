#' @title Download files stored in OneDrives or SharePoints
#' @description `r lifecycle::badge("experimental")`
#'
#' This function downloads files stored in OneDrives or SharePoints
#' @inheritParams SharePoint_file
#' @param where "OneDrive" or "SharePoint"
#' @export
download365_file <- function(where, SharePoint, drive, file) {
  ext = gsub(".*\\.", ".", file)
  tempFile = tempfile(fileext = ext)
  if (where == "SharePoint") {
    site = Microsoft365R::get_sharepoint_site(site_url = SharePoint)

    drives_list = site$list_drives()
    drives_name = get_drives(site)
    drive_index = which(drives_name == drive)

    if (length(drive_index) == 0) {
      cli::cli_abort(
        "The drive '{drive}' was not found.
        Use `get_drives()` to get a list of available drives."
      )
    }

    drive = drives_list[[drive_index]]

  } else if (where == "OneDrive"){
    drive = Microsoft365R::get_business_onedrive()
  } else{
    cli::cli_abort("``where` must be set to 'SharePoint' or 'OneDrive'.`")
  }
  drive$download_file(
    file,
    dest = tempFile,
    overwrite = T
  )
  return(tempFile)
}





