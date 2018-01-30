get_gene_targets <- function(genes, type) {
  if(type == "ttd") {
    D <- readRDS("grandforest-web-common/data/ttd.rds")
    out <- subset(D, gene %in% genes)
  } else if(type == "mirtarbase") {
    D <- readRDS("grandforest-web-common/data/mirtarbase.human.rds")
    out <- subset(D, gene %in% genes)
  }
  return(out)
}

gene_target_sources <- function() {
  list(
    "Therapeutic Target Database (drugs)" = "ttd",
    "miRTarBase 7.0 (miRNA)" = "mirtarbase"
  )
}

get_pubmed_links <- function(x) {
  if(is.na(x)) return(NA)
  ids <- unlist(strsplit(x, ","))
  links <- sapply(ids, function(x) sprintf("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/?term=%s\" target=\"_blank\">%s</a>", x, x))
  paste(links, collapse=", ")
}

get_gene_target_links <- function(D, type) {
  make_links <- function(ids, type) {
    link <- switch(type,
      ttd = "https://db.idrblab.org/ttd/drug/%s",
      pubchem = "https://pubchem.ncbi.nlm.nih.gov/compound/%s",
      mirtarbase = "http://mirtarbase.mbc.nctu.edu.tw/php/detail.php?mirtid=%s"
    )
    fmt <- sprintf("<a href=\"%s\" target=_blank>%%s</a>", link)
    sapply(ids, function(x) if(is.na(x)) NA else sprintf(fmt, x, x))
  }

  if(type == "ttd") {
    D[[1]] <- make_links(D[[1]], "ttd")
    D[[4]] <- make_links(D[[4]], "pubchem")
  } else if(type == "mirtarbase") {
    D[[1]] <- make_links(D[[1]], "mirtarbase")
    D[[4]] <- sapply(D[[4]], get_pubmed_links)
  }
  return(D)
}
