source("grandforest-web-common/mapping.R")
source("grandforest-web-common/make_links.R")

gene_target_sources <- function() {
  list(
    "DrugBank 5.0.11 (drugs)" = "drugbank",
    "Therapeutic Target Database (drugs)" = "ttd",
    "miRTarBase 7.0 (miRNA)" = "mirtarbase"
  )
}

#get_gene_targets <- function(genes, type) {
#  subset(readRDS(sprintf("grandforest-web-common/data/%s.rds", type)), gene %in% genes)
#}

get_gene_targets <- function(genes, type, filter_results, pvalueCutoff=1, qvalueCutoff=1) {
  D <- readRDS(sprintf("grandforest-web-common/data/%s.rds", type))
  sets <- split(D[,1], D[,3])
  usize <- length(unique(D[,1]))
  
  overlap <- lapply(sets, intersect, genes)
  hit <- lengths(overlap)
  size <- lengths(sets)
  gsize <- length(genes)
  
  pvalue <- mapply(function(hit, size, usize) {
    phyper(hit-1, size, usize-size, gsize, lower.tail=FALSE)
  }, hit, size, usize)
  
  padj <- p.adjust(pvalue, method="BH")
  
  q <- tryCatch(qvalue(pvalue)$qvalues, error=function(e) NA)
  
  keep <- hit > 0
  if(filter_results) {
    keep <- keep & pvalue <= pvalueCutoff
    if(!any(is.na(q))) {
      keep <- keep & q <= qvalueCutoff
    }
  }
  
  sourcemap <- unique(D[,3:4])
  sourcemap <- setNames(sourcemap[,2], sourcemap[,1])
  
  out <- data.table(
    names(overlap),
    sourcemap[names(overlap)],
    targets = overlap,
    pvalue = pvalue,
    p.adjust = padj,
    qvalue = q,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(out)[1:2] <- colnames(D)[3:4]
  
  out[keep,]
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

get_gene_targets_table <- function(targets) {
  out <- targets
  out$targets <- sapply(out$targets, paste0, collapse="/")
  out <- get_gene_target_links(out, type)
  out[order(out$pvalue, decreasing=FALSE),] # sort by target count
}

get_gene_targets_network <- function(targets, type, show_symbols, species) {
  library(visNetwork)
  saveRDS(targets, file="~/targets.rds")
  # make from node table
  targets <- tidyr::unnest(targets[,1:3], targets)
  
  from_nodes <- setNames(targets[,c(1,1,2)], c("id","label","title"))
  from_nodes <- from_nodes[!duplicated(from_nodes$id),]
  from_nodes$title <- paste0(make_links(from_nodes$id, type), "<br>", from_nodes$title)
  from_nodes$color.background <- "lightblue"

  # make to node table (genes)
  to_nodes <- data.frame(id=targets$targets, title=map_ids_fallback(targets$targets, "SYMBOL", "ENTREZID", species))
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
    visEdges(smooth = FALSE, arrows="to") %>%
    visIgraphLayout()
}
