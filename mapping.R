map_ids_fallback <- function(x, to, from, species) {
  if(species == "human") {
    sym <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, x, to, from)
  }
  else if(species == "mouse") {
    sym <- AnnotationDbi::mapIds(org.Mm.eg.db::org.Mm.eg.db, x, to, from)
  }
  else {
    stop("Unknown species: ", species)
  }
  missing <- is.na(sym)
  sym[missing] <- x[missing]
  return(sym)
}
