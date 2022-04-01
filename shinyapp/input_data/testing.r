

priority_block_geojson <- readLines("/blocks_priority.geojson")

#setup block geojson layer
block_geojson <- priority_block_geojson
block_geojson$features <- lapply(block_geojson$features, function(feat){
  feat$properties <- list(
    name = feat[["properties"]][["ID_NCBA_BLOCK"]]
  )
  feat
})