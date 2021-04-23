BiocBPRPContainer <- function(image = "", name = NULL,  environment = list(),
                             maxWorkerNum = 4L,
                             RPackages = NULL,
                             sysPackages = NULL){
  .BiocBPRPContainer$new(
    name=name, image = image,
    environment = environment,
    maxWorkerNum = as.integer(maxWorkerNum),
    RPackages=RPackages,
    sysPackages=sysPackages)
}

#' Common BiocBPRPContainer parameter
#'
#' Common BiocBPRPContainer parameter
#'
#' @param image Character, the container image
#' @param name Character, the optional name of the container
#' @param environment List, the environment variables in the container
#' @rdname BiocBPRPContainer-commom-parameters
#' @name BiocBPRPContainer-commom-parameters
NULL



#' Get the Bioconductor BiocParallel RedisParam container
#'
#' Get the Bioconductor BiocParallel RedisParam container.
#'
#' @inheritParams BiocBPRPContainer-commom-parameters
#' @examples BiocBPRPServerContainer()
#' @return a `BiocBPRPContainer` object
#' @export
BiocBPRPServerContainer <- function(environment = list(),tag = "latest"){
  name <- "redisRServerContainer"
  image <- paste0("dockerparallel/redis-r-server:",tag)
  BiocBPRPContainer(image = image, name=name,
                   environment=environment,
                   maxWorkerNum=1L)
}
#' Get the Bioconductor BiocParallel Redis worker container
#'
#' Get the Bioconductor BiocParallel Redis worker container.
#'
#' @inheritParams BiocBPRPContainer-commom-parameters
#' @param RPackages Character, a vector of R packages that will be installed
#' by `AnVIL::install` before connecting with the server
#' @param sysPackages Character, a vector of system packages that will be installed
#' by `apt-get install` before running the R worker
#' @param maxWorkerNum Integer, the maximum worker number in a container
#'
#' @examples BiocBPRPWorkerContainer()
#' @return a `BiocBPRPContainer` object
#' @export
BiocBPRPWorkerContainer <- function(RPackages = NULL,
                                   sysPackages = NULL,
                                   environment = list(),
                                   maxWorkerNum = 4L,
                                   tag = "latest"){
  name <- "redisRWorkerContainer"
  image <- paste0("dockerparallel/bioc-biocparallel-redisparam-worker:",tag)
  BiocBPRPContainer(image = image, name=name, RPackages=RPackages, sysPackages=sysPackages,
                   environment=environment,
                   maxWorkerNum=maxWorkerNum)
}

.BiocBPRPContainer$methods(
  show = function(){
    cat("Bioconductor foreach redis container reference object\n")
    cat("  Image:     ", .self$image, "\n")
    cat("  maxWorkers:", .self$maxWorkerNum, "\n")
    if(!is.null(.self$RPackages)){
      cat("  R packages:", paste0(.self$RPackages, collapse = ", "), "\n")
    }
    if(!is.null(.self$sysPackages)){
      cat("  system packages:", paste0(.self$sysPackages, collapse = ", "), "\n")
    }
    cat("  Environment variables:\n")
    for(i in names(.self$environment)){
      cat("    ",i,": ", .self$environment[[i]], "\n",sep="")
    }
    invisible(NULL)
  }
)


