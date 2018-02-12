library(visNetwork)
source("grandforest-web-common/mapping.R")
source("grandforest-web-common/make_links.R")

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
  D[[1]] <- make_links(D[[1]], "ncbi_gene")
  if(type == "drugbank") {
    D[[3]] <- make_links(D[[3]], "drugbank")
    D[[5]] <- make_links(D[[5]], "pubchem_cid")
    D[[6]] <- make_links(D[[6]], "pubchem_sid")
    D[[7]] <- make_links(D[[7]], "kegg_drug")
  } else if(type == "ttd") {
    D[[3]] <- make_links(D[[3]], "ttd")
    D[[5]] <- make_links(D[[5]], "pubchem_cid")
    D[[6]] <- make_links_list(D[[6]], "pubchem_sid")
  } else if(type == "mirtarbase") {
    D[[3]] <- make_links(D[[3]], "mirtarbase")
    D[[5]] <- make_links_list(D[[5]], "pubmed")
  }
  return(D)
}

get_targets_network <- function(targets, type, show_symbols) {
  # make from node table
  from_nodes <- setNames(targets[,c(3,3,4)], c("id","label","title"))
  from_nodes <- from_nodes[!duplicated(from_nodes$id),]
  from_nodes$title <- paste0(make_links(from_nodes$id, type), "<br>", from_nodes$title)
  from_nodes$color.background <- "lightblue"

  # make to node table (genes)
  to_nodes <- setNames(targets[,c(1,2)], c("id","title"))
  to_nodes <- to_nodes[!duplicated(to_nodes$id),]
  to_nodes$label <- if(show_symbols) to_nodes$title else to_nodes$id
  to_nodes$title <- paste0(make_links(to_nodes$id, "ncbi_gene"), "<br>", to_nodes$title)
  to_nodes$color.background <- "#f18484"

  nodes <- rbind(from_nodes, to_nodes)

  validate(need(
    nrow(nodes) <= MAX_TARGET_NETWORK_NODES,
    sprintf("Gene target network not supported for > %d nodes.", MAX_TARGET_NETWORK_NODES)
  ))

  # get edges
  edges <- setNames(targets[,c(1,3)], c("from","to"))

  visNetwork(nodes, edges) %>%
    visNodes(shape = "ellipse") %>%
    visEdges(smooth = FALSE, arrows="from") %>%
    visIgraphLayout()
}
