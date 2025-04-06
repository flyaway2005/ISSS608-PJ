# Set CRAN mirror and skip problematic package
.First <- function() {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  # This tells R to continue even if bayestestR can't be installed
  assign(".packageStartupMessage", 
         function(msg) {
           if (grepl("bayestestR", msg)) return(invisible(NULL))
           base::.packageStartupMessage(msg)
         }, 
         envir = baseenv())
}