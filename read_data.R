library(data.table)

read_expression_file <- function(path) {
  lpath <- tolower(path)
  if(endsWith(lpath, ".zip")) {
    files <- unzip(path, list=TRUE)
    fpath <- files$Name[which.max(files$Length)]
    exdir <- tempfile()
    ext.files <- unzip(path, files=fpath, overwrite=TRUE, exdir=exdir)
    read_expression_file(file.path(exdir, fpath))
  } else if(endsWith(lpath, ".rds")) {
    out <- readRDS(path)
    if(!(class(out)[1] %in% c("data.frame","data.table"))) {
      stop("Expression RDS file does not contain a `data.frame` or `data.table` object.")
    }
    if(class(out)[1] == "data.frame") {
        out <- data.table(out)
    }
    return(out)
  } else {
    fread(path, header=TRUE)
  }
}

read_network_file <- function(path) {
  lpath <- tolower(path)
  if(endsWith(lpath, ".zip")) {
    files <- unzip(path, list=TRUE)
    fpath <- files$Name[which.max(files$Length)]
    exdir <- tempfile()
    ext.files <- unzip(path, files=fpath, overwrite=TRUE, exdir=exdir)
    out <- read_network_file(file.path(exdir, fpath))
  } else if(endsWith(lpath, ".rds")) {
    out <- readRDS(path)
    if(!(class(out)[1] %in% c("data.frame","data.table"))) {
      stop("Network RDS file does not contain a `data.frame` or `data.table` object.")
    }
  } else {
    out <- fread(path, header=TRUE, colClasses="character")
  }
  if(ncol(out) < 2) {
    stop("Network file does not contain at least two columns.")
  }
  out <- out[,1:2]
  colnames(out) <- c("from","to")
  return(out)
}
