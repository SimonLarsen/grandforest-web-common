source("grandforest-web-common/mapping.R")
source("grandforest-web-common/make_links.R")

gene_target_sources <- function() {
  list(
    "DrugBank 5.0.11 (drugs)" = "drugbank",
    "Therapeutic Target Database (drugs)" = "ttd",
    "miRTarBase 7.0 (miRNA)" = "mirtarbase"
  )
}

get_gene_target_source_type <- function(type) {
  switch(type,
    "drugbank" = "drugbank",
    "ttd" = "ttd",
    "mirtarbase" = "mirbase"
  )
}

get_gene_targets <- function(genes, type) {
  subset(readRDS(sprintf("grandforest-web-common/data/%s.rds", type)), gene %in% genes)
}

get_gene_target_counts <- function(type) {
  D <- readRDS(sprintf("grandforest-web-common/data/%s.rds", type))
  table(D[[3]])
}

get_gene_target_links <- function(D, type) {
  D[["targets"]] <- make_links_list(D[["targets"]], "ncbi_gene", "/", "/")
  D[[1]] <- make_links(D[[1]], get_gene_target_source_type(type))
  return(D)
}

get_gene_targets_table <- function(targets, type) {
  if(nrow(targets) == 0) {
    return(data.frame(
      targets[,3:ncol(targets),drop=FALSE],
      targets = character(),
      count = numeric(),
      ratio = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  targets <- split(targets, targets[,3])
  counts <- get_gene_target_counts(type)

  out <- do.call(rbind, lapply(targets, function(x) {
    data.frame(
      x[1,3:ncol(x),drop=FALSE],
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
  library(visNetwork)

  validate(need(
    nrow(targets) <= MAX_TARGET_NETWORK_EDGES,
    sprintf("Gene target network only supports up to %d edges.", MAX_TARGET_NETWORK_EDGES)
  ))

  # make from node table
  from_nodes <- setNames(targets[,c(3,3)], c("id","label"))
  if(ncol(targets) >= 4) {
    from_nodes$title <- targets[,4]
  }
  from_nodes <- from_nodes[!duplicated(from_nodes$id),]
  from_nodes$title <- paste0(make_links(from_nodes$id, get_gene_target_source_type(type)), "<br>", from_nodes$title)
  from_nodes$color.background <- "lightblue"

  # make to node table (genes)
  to_nodes <- setNames(targets[,c(1,2)], c("id","title"))
  to_nodes <- to_nodes[!duplicated(to_nodes$id),]
  to_nodes$label <- if(show_symbols) to_nodes$title else to_nodes$id
  to_nodes$title <- paste0(make_links(to_nodes$id, "ncbi_gene"), "<br>", to_nodes$title)
  to_nodes$color.background <- "#f18484"

  nodes <- rbind(from_nodes, to_nodes)

  # get edges
  edges <- setNames(targets[,c(1,3)], c("from","to"))

  visNetwork(nodes, edges) %>%
    visNodes(shape = "ellipse") %>%
    visEdges(smooth = FALSE, arrows="from") %>%
    visIgraphLayout()
}
