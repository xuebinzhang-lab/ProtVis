#' Cross-Platform Guide: Checking and Repairing Disk Volumes
#'
#' @param exclude A vector of names to exclude from the returned list.
#'
#' @return A named vector of available volumes with optional exclusions.
#' @importFrom stringr str_remove
#' @importFrom fs dir_ls dir_exists
#' @description
#' This function retrieves available volume information based on the operating system
#' the R session is currently running on. It supports MacOS, Linux, and Windows.
#' Specifically for Windows, it addresses a bug in `shinyFiles::getVolumes` related
#' to handling paths with Chinese characters, ensuring correct handling of such paths.
#' @export

getVolumes_win <- function (exclude = NULL) {
  osSystem <- base::Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- fs::dir_ls("/Volumes")
    base::names(volumes) <- base::basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(fs::dir_exists("/media"))) {
      media <- fs::dir_ls("/media")
      base::names(media) <- base::basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    wmic <- base::paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    if (!file.exists(wmic)) {
      volumes_info <- base::system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                              stdout = TRUE)
      num = base::as.integer(volumes_info[1])
      if (num == 0)
        return(NULL)
      mat <- base::matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- base::gsub(":\\\\$", ":/", mat[, 1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- base::paste0(volNames, " (", base::gsub(":/$", ":", volumes), ")")
    }
    else {
      volumes <- base::system(base::paste(wmic, "logicaldisk get Caption"),
                        intern = TRUE, ignore.stderr = TRUE)
      volumes <- base::sub(" *\\r$", "", volumes)
      keep <- !base::tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- base::system(base::paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                         intern = TRUE, ignore.stderr = TRUE)
      volNames <- stringr::str_remove(volNames," *\\r$") #> fix bugs for Chinese character.
      volNames <- volNames[keep]
      volNames <- base::paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- base::paste0(volNames, "(", volumes, ")")
    }
    names(volumes) <- volNames
    volumes <- base::gsub(":$", ":/", volumes)
  }
  else {
    stop("unsupported OS")
  }
  if (!base::is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  volumes
}
