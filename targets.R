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

get_gene_target_links <- function(D, type) {
  make_links <- function(ids, type) {
    link <- switch(type,
      drugbank = "https://www.drugbank.ca/drugs/%s",
      ttd = "https://db.idrblab.org/ttd/drug/%s",
      pubchem_cid = "https://pubchem.ncbi.nlm.nih.gov/compound/%s",
      pubchem_sid = "https://pubchem.ncbi.nlm.nih.gov/substance/%s",
      mirtarbase = "http://mirtarbase.mbc.nctu.edu.tw/php/detail.php?mirtid=%s",
      pubmed = "https://www.ncbi.nlm.nih.gov/pubmed/?term=%s",
      kegg_drug = "http://www.genome.jp/dbget-bin/www_bget?%s"
    )
    fmt <- sprintf("<a href=\"%s\" target=_blank>%%s</a>", link)
    sapply(ids, function(x) if(is.na(x)) NA else sprintf(fmt, x, x))
  }
  
  make_links_list <- function(ids, type) {
    sapply(ids, function(x) {
      if(is.na(x)) NA
      else {
        y <- unlist(strsplit(x, ","))
        paste(make_links(y, type), collapse=", ")
      }
    })
  }

  if(type == "drugbank") {
    D[[2]] <- make_links(D[[2]], "drugbank")
    D[[4]] <- make_links(D[[4]], "pubchem_cid")
    D[[5]] <- make_links(D[[5]], "pubchem_sid")
    D[[6]] <- make_links(D[[6]], "kegg_drug")
  } else if(type == "ttd") {
    D[[2]] <- make_links(D[[2]], "ttd")
    D[[4]] <- make_links(D[[4]], "pubchem_cid")
    D[[5]] <- make_links_list(D[[5]], "pubchem_sid")
  } else if(type == "mirtarbase") {
    D[[2]] <- make_links(D[[2]], "mirtarbase")
    D[[4]] <- make_links_list(D[[4]], "pubmed")
  }
  return(D)
}

get_targets_network <- function(targets, show_symbols) {
  edges <- targets[,1:2]
  colnames(edges) <- c("from","to")

  from_nodes <- unique(edges$from)
  from_labels <- if(show_symbols) map_ids_fallback(from_nodes, "SYMBOL", "ENTREZID") else from_nodes
  to_nodes <- unique(edges$to)

  nodes <- data.frame(
    id = c(from_nodes, to_nodes),
    label = c(from_labels, to_nodes),
    color.background = c(rep("#f18484", length(from_nodes)), rep("lightblue", length(to_nodes))),
    stringsAsFactors = FALSE
  )

  validate(need(
    nrow(nodes) <= MAX_TARGET_NETWORK_NODES,
    sprintf("Gene target network not supported for > %d nodes.", MAX_TARGET_NETWORK_NODES)
  ))

  visNetwork(nodes, edges) %>%
    visNodes(shape = "ellipse") %>%
    visEdges(smooth = FALSE, arrows="from") %>%
    visIgraphLayout()
}
