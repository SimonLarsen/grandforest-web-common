library(visNetwork)
source("grandforest-web-common/mapping.R")
source("grandforest-web-common/make_links.R")

gene_target_sources <- function() {
  list(
    "DrugBank 5.0.11 (drugs)" = "drugbank",
    "Therapeutic Target Database (drugs)" = "ttd",
    "miRTarBase 7.0 (miRNA)" = "mirtarbase"
  )
}

get_gene_targets <- function(genes, type) {
  subset(readRDS(sprintf("grandforest-web-common/data/%s.rds", type)), gene %in% genes)
}

get_gene_target_counts <- function(type) {
  readRDS(sprintf("grandforest-web-common/data/%s.count.rds", type))
}

get_gene_target_links <- function(D, type) {
  D[["targets"]] <- make_links_list(D[["targets"]], "ncbi_gene", "/", "/")
  if(type == "drugbank") {
    D[[1]] <- make_links(D[[1]], "drugbank")
  } else if(type == "ttd") {
    D[[1]] <- make_links(D[[1]], "ttd")
  } else if(type == "mirtarbase") {
    D[[1]] <- make_links(D[[1]], "mirtarbase")
  }
  return(D)
}

get_gene_targets_table <- function(targets, type) {
  targets <- split(targets, targets[,3])
  counts <- get_gene_target_counts(type)
  
  out <- do.call(rbind, lapply(targets, function(x) {
    data.frame(
      x[1,3:ncol(x)],
      targets = paste0(x[,1], collapse="/"),
      count = nrow(x),
      ratio = paste0(nrow(x), "/", counts[x[1,3]]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  
  out <- get_gene_target_links(out, type)
  out[order(out$count, decreasing=TRUE),] # sort by target count
}

get_gene_targets_network <- function(targets, type, show_symbols) {
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
