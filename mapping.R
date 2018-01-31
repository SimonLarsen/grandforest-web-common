library(org.Hs.eg.db)

map_ids_fallback <- function(x, to, from) {
  sym <- mapIds(org.Hs.eg.db, x, to, from)
  missing <- is.na(sym)
  sym[missing] <- x[missing]
  return(sym)
}
