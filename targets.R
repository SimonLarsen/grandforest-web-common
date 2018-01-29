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
  ids <- unlist(strsplit(x, ","))
  links <- sapply(ids, function(x) sprintf("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/?term=%s\" target=\"_blank\">%s</a>", x, x))
  paste(links, collapse=", ")
}

get_gene_target_links <- function(D, type) {
  if(type == "ttd") {
    D[[1]] <- sapply(D[[1]], function(x) sprintf("<a href=\"https://db.idrblab.org/ttd/drug/%s\" target=\"_blank\">%s</a>", x, x))
    D[[4]] <- sapply(D[[4]], function(x) sprintf("<a href=\"https://pubchem.ncbi.nlm.nih.gov/compound/%s\" target=\"_blank\">%s</a>", x, x))
  } else if(type == "mirtarbase") {
    D[[1]] <- sapply(D[[1]], function(x) sprintf("<a href=\"http://mirtarbase.mbc.nctu.edu.tw/php/detail.php?mirtid=%s\" target=\"_blank\">%s</a>", x, x))
    D[[4]] <- sapply(D[[4]], get_pubmed_links)
  }
  return(D)
}
