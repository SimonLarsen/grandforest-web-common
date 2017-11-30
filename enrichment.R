library(DOSE)
library(clusterProfiler)

gene_set_enrichment <- function(genes, universe, type, pvalueCutoff, qvalueCutoff) {
  if(type == "gobp") {
    out <- enrichGO(genes, universe=universe, OrgDb="org.Hs.eg.db", ont="BP", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "gomf") {
    out <- enrichGO(genes, universe=universe, OrgDb="org.Hs.eg.db", ont="MF", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "gocc") {
    out <- enrichGO(genes, universe=universe, OrgDb="org.Hs.eg.db", ont="CC", pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "do") {
    out <- enrichDO(genes, universe=universe, pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  } else if(type == "disgenet") {
    out <- enrichDGN(genes, universe=universe, pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  }
  return(out)
}

gene_set_enrichment_types <- function() {
  list(
    "GO Biological process" = "gobp",
    "GO Molecular function" = "gomf",
    "GO Cellular component" = "gocc",
    "Disease Ontology" = "do",
    "DisGeNET" = "disgenet"
  )
}

gene_set_enrichment_get_links <- function(ids, type) {
  if(type == "gobp" || type == "gomf" || type == "gocc") {
    sapply(ids, function(x) sprintf("<a href=\"http://amigo.geneontology.org/amigo/term/%s\" target=\"_blank\">%s</a>", x, x))
  } else if(type == "do") {
    sapply(ids, function(x) sprintf("<a href=\"http://disease-ontology.org/term/%s\" target=\"_blank\">%s</a>", x, x))
  }
}