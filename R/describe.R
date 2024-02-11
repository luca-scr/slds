#' @name describe
#' @aliases describe
#' 
#' @title Descriptive statistics
#' 
#' @description Descriptive statistics of a data frame.
#' 
#' @param data a matrix or a data.frame.
#' @param \dots additional arguments to be passed to the low level functions.
#'
#' @examples
#' 
#' describe(iris)
#' describe(dplyr::group_by(iris, Species))
#'
#' @importFrom skimr skim_with
#' 
#' @export

describe <- function(data, charts = TRUE, ...)
{
  requireNamespace("skimr")
  skimr_describe <- skimr::skim_with(
    "base" = skimr::sfl(n_complete = skimr::n_complete, 
                        n_missing = skimr::n_missing),
    "character" = skimr::sfl(n_unique = skimr::n_unique, 
                             top_chars = ~ top_chars(.)),
    "factor" = { l <- skimr::sfl(ordered = ~ is.ordered(.), 
                                 top_freqs = ~ top_freqs(.))
                 if(charts) 
                   l[["funs"]]$barchart = ~ inline_barchart(.)
                 l 
               },    
    "numeric" = { l <- skimr::sfl(mean, sd, min, 
                                  p25 = ~ quantile(., 0.25, na.rm = TRUE),
                                  p50 = ~ quantile(., 0.50, na.rm = TRUE),
                                  p75 = ~ quantile(., 0.75, na.rm = TRUE),
                                  max)
                  if(charts) 
                    l[["funs"]]$hist = ~ skimr::inline_hist(.)
                  l 
                },    
    append = FALSE)
  
  d <- skimr_describe(data)
  class(d) <- append("describe", class(d))
  return(d)
}

#' @rdname describe
#' @exportS3Method print describe
#' @export print.describe

print.describe <- function(x, ...)
{
  # requireNamespace("skimr")
  skimr:::print.skim_df(x, include_summary = FALSE, ...)
}
  
#-----------------------------------------------------------------------

top_freqs <- function(x, min_char = 5, max_levels = 5) 
{
  freqs <- round(prop.table(table(x, useNA = "no")), 4)
  top <- freqs[sort(order(freqs, decreasing = TRUE)[1:max_levels])]
  top_names <- abbreviate(names(top), minlength = min_char)
  out <- paste0(top_names, ": ", top, collapse = " |")
  if(length(freqs) > length(top)) 
    out <- paste0(out, " |", intToUtf8(8230))
  out
}

top_chars <- function(x, min_char = 5, max_levels = 5) 
{
  counts <- skimr::sorted_count(x)
  top <- if(length(counts) > max_levels)
           counts[seq_len(max_levels)] else counts
  top_names <- abbreviate(names(top), minlength = min_char)
  paste0(top_names, collapse = " | ")
}

inline_barchart <- function(x, max_levels = 8) 
{
  if(any(is.infinite(x))) 
  {
    x[is.infinite(x)] <- NA
    warning("Variable contains Inf or -Inf value(s) that were converted to NA.")
  }
  if(length(x) < 1 || all(is.na(x))) return(" ")
  freqs <- prop.table(table(x, useNA = "no"))
  tab <- freqs/max(freqs)
  tab <- tab[sort(order(tab, decreasing = TRUE)[1:max_levels])]
  out <- skimr:::spark_bar(tab)
  if(length(freqs) > length(tab)) 
    out <- paste0(out, intToUtf8(8230))
  out
}
