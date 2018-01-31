library(visNetwork)
source("grandforest-web-common/mapping.R")

get_gene_targets <- function(genes, type) {
  if(type == "drugbank") {
    subset(readRDS("grandforest-web-common/data/drugbank.rds"), gene %in% genes)
  } else if(type == "ttd") {
    subset(readRDS("grandforest-web-common/data/ttd.rds"), gene %in% genes)
  } else if(type == "mirtarbase") {
    subset(readRDS("grandforest-web-common/data/mirtarbase.human.rds"), gene %in% genes)
  }
}

gene_target_sources <- function() {
  list(
    "DrugBank 5.0.11 (drugs)" = "drugbank",
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
      drugbank = "https://www.drugbank.ca/drugs/%s",
      ttd = "https://db.idrblab.org/ttd/drug/%s",
      pubchem = "https://pubchem.ncbi.nlm.nih.gov/compound/%s",
      mirtarbase = "http://mirtarbase.mbc.nctu.edu.tw/php/detail.php?mirtid=%s"
    )
    fmt <- sprintf("<a href=\"%s\" target=_blank>%%s</a>", link)
    sapply(ids, function(x) if(is.na(x)) NA else sprintf(fmt, x, x))
  }

  if(type == "drugbank") {
    D[[1]] <- make_links(D[[1]], "drugbank")
  } else if(type == "ttd") {
    D[[1]] <- make_links(D[[1]], "ttd")
    D[[4]] <- make_links(D[[4]], "pubchem")
  } else if(type == "mirtarbase") {
    D[[1]] <- make_links(D[[1]], "mirtarbase")
    D[[4]] <- sapply(D[[4]], get_pubmed_links)
  }
  return(D)
}

get_targets_network <- function(targets, show_symbols) {
  gene_col <- which(colnames(targets) == "gene")
  edges <- targets[,c(1,gene_col)]
  colnames(edges) <- c("from","to")

  from_nodes <- unique(edges$from)
  to_nodes <- unique(edges$to)
  to_labels <- if(show_symbols) map_ids_fallback(to_nodes, "SYMBOL", "ENTREZID") else to_nodes

  nodes <- data.frame(
    id = c(from_nodes, to_nodes),
    label = c(from_nodes, to_labels),
    color.background = c(rep("lightblue", length(from_nodes)), rep("#f18484", length(to_nodes))),
    stringsAsFactors = FALSE
  )

  validate(need(
    nrow(nodes) <= MAX_TARGET_NETWORK_NODES,
    sprintf("Gene target network not supported for > %d nodes.", MAX_TARGET_NETWORK_NODES)
  ))

  visNetwork(nodes, edges) %>%
    visNodes(shape = "ellipse") %>%
    visEdges(smooth = FALSE) %>%
    visIgraphLayout()
}
