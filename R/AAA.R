###########################
## container provider
###########################
#' The Bioconductor BiocParallel Redis container
#'
#' The Bioconductor BiocParallel Redis container. See `?BiocBPRPServerContainer` and
#' `?BiocBPRPWorkerContainer`
.BiocBPRPContainer <- setRefClass(
    "BiocBPRPContainer",
    fields = list(
        sysPackages = "CharOrNULL",
        RPackages = "CharOrNULL"
    ),
    contains = "DockerContainer"
)
