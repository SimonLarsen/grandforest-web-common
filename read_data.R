library(data.table)

read_expression_file <- function(path) {
  lpath <- tolower(path)
  if(endsWith(lpath, ".zip")) {
    fread(paste0("unzip -cq ", path), header=TRUE)
  } else if(endsWith(lpath, ".rds")) {
    out <- readRDS(path)
    if(!(class(out)[1] %in% c("data.frame","data.table"))) {
      stop("RDS file does not contain a `data.frame` or `data.table` object.")
    }
    return(out)
  } else {
    fread(path, header=TRUE)
  }
}