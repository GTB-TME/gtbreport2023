# Simple functions to create call-outs in html text to the figures/tables and references.
# Display names have non-alphanumeric characters changed to hyphens in the html anchor name
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


lnk <- function(display_name){

  # Create a link from text to a named anchor
  return(paste0('<span class="red">[',
                display_name,
                '](#',
                gsub("[^a-zA-Z0-9]", "-", tolower(display_name)),
                ')</span>'))
}


anch <- function(display_name){

  # Create a named anchor (can be inserted above a figure or table or references section)
  return(paste0('<a name="',
                gsub("[^a-zA-Z0-9]", "-", tolower(display_name)),
                '"></a>'))
}


ref_lnk <- function(display_name){

  # Create a link from text to the references in the WHO style (black, italic)
  return(paste0('<span class="refs">[',
                display_name,
                '](#refs)</span>'))
}
