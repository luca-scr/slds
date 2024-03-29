core <- c("caret", "caretEnsemble", "data.table", "ggplot2", "scales", 
          "patchwork", "mlbench", "mclust", "ModelMetrics", "mgcv", 
          "glmnet", "randomForest", "gbm", "klaR", "leaps", "rpart.plot",
          "visreg", "GGally", "jpeg")
					
# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) 
{
  loc <- if (pkg %in% loadedNamespaces()) 
            dirname(getNamespaceInfo(pkg, "path"))
  do.call(
    "library",
    list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
}

slds_attach <- function(to_load) 
{

  pkgs <- utils::packageDescription("slds")$Imports
  pkgs <- strsplit(gsub("\n", "", gsub(" ", "", pkgs)), ",")[[1]]
  available <- utils::installed.packages()[,"Package"]
  pkgsAvail <- sapply(pkgs, function(pkg) pkg %in% available)
  pkgsNotAvail <- names(pkgsAvail)[!pkgsAvail]
  if(length(pkgsNotAvail) > 0)
  {
    msg(cli::rule(left = crayon::bold("Installing packages")),
        startup = TRUE)
    install.packages(pkgsNotAvail)
  }
    
  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("slds ", package_version("slds"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible()
}

package_version <- function(x) 
{
  version <- as.character(unclass(utils::packageVersion(x))[[1]])
  if (length(version) > 3) 
	{
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

msg <- function(x, startup = FALSE) 
{
  if (startup) 
    rlang::inform(x, class = "packageStartupMessage")
	else 
    rlang::inform(x)
}
