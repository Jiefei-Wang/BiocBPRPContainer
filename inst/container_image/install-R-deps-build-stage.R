BiocManager::install("Bioconductor/AnVIL", update=FALSE, upgrade = "never")
pkgs <- c("RedisParam")
AnVIL::install(pkgs, ask = FALSE)
if (!requireNamespace("doRedis", quietly = TRUE)){
    BiocManager::install("Jiefei-Wang/RedisParam", update=FALSE, upgrade = "never")
}