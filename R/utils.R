#' @title Download files stored in OneDrives or SharePoints
#' @description `r lifecycle::badge("experimental")`
#'
#' This function downloads files stored in OneDrives or SharePoints
#' @inheritParams SharePoint_file
#' @param where "OneDrive" or "SharePoint"
#' @export
download365_file <- function(where, SharePoint, drive, file) {
  ext = gsub(".*\\.", "", file)
  tempFile = tempfile(fileext = ext)
  if (where == "SharePoint") {
    site = Microsoft365R::get_sharepoint_site(site_url = SharePoint)
    drives = site$list_drives()
    drive_index = get_index(site, drive)
    drive = drives[[drive_index]]
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





