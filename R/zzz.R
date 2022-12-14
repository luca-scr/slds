.onAttach <- function(...) 
{
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()
  slds_attach(needed)
}

is_attached <- function(x) 
{
  paste0("package:", x) %in% search()
}
