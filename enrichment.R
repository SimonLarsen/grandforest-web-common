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
  } else if(type == "ncg") {
    out <- enrichNCG(genes, universe=universe, pvalueCutoff=pvalueCutoff, qvalueCutoff=qvalueCutoff)
  }
  return(out)
}

gene_set_enrichment_types <- function() {
  list(
    "GO Biological process" = "gobp",
    "GO Molecular function" = "gomf",
    "GO Cellular component" = "gocc",
    "Disease Ontology" = "do",
    "DisGeNET" = "disgenet",
    "Network of Cancer Genes" = "ncg"
  )
}
