#' @importFrom stringr str_split_fixed str_split
#' @importFrom yaml yaml.load
parse_chapter <- function(chapter_file) {
  splitup <- str_split_fixed(paste(readLines(chapter_file, warn = FALSE), collapse = '\n'), "\n---", 2)
  chapter_meta <- yaml.load(gsub("^---\n+", '', splitup[1]))
  raw_exercises <- str_split(splitup[2], pattern = '\n\n---')[[1]]
  
  exercises = list()
  message("Rendering all exercises...")
  for(i in 1:length(raw_exercises)) {
    
    rendered_exercise <- parse_and_render_exercise(raw_exercises[[i]], i)
    message(sprintf("  - %s", rendered_exercise$title))
    
    # check exercise for consistency
    check_exercise(rendered_exercise)
    
    exercises[[i]] <- rendered_exercise
  }
  
  check_chapter(exercises)
  message("Rendering all exercises done.")

  payload <- list(meta = chapter_meta, exercises = exercises)
}

parse_and_render_exercise <- function(raw_ex, index) {
  raw_parts <- str_split(raw_ex, "\n\\*{3}")[[1]]
  parts <- lapply(raw_parts, parse_elements)
  if (length(parts) > 1){
    main  <- parts[[1]]
    named <- Filter(function(z) !is.null(z$name), parts[-1])
    names(named) <- lapply(named, '[[', "name")
    exercise <- c(main, named)
  } else {
    exercise <- slide[[1]]
  }
  
  class(exercise) <- exercise$type
  return(render_exercise(exercise, index))
}

parse_elements <- function(raw_part) {
  # element_details <- parse_meta(raw_part)
  splitup <- split_meta(raw_part)
  element <- parse_meta(splitup[1])
  element$content <- splitup[2]
  element
}

#' @importFrom stringr str_split_fixed
split_meta <- function(blocks){
  split_block <- function(block){
    if (grepl("^\\s*\\{", block)){
      block <- str_split_fixed(block, "}\n", 2)
      block[1] <- paste(block[1], "}")
    } else {
      block <- str_split_fixed(block, "\n", 2)
    }
    return(block)
  }
  t(sapply(blocks, split_block, USE.NAMES = F))
}

#' @importFrom stringr str_split_fixed
parse_meta <- function(meta){
  x <- strsplit(meta, ' ')[[1]]
  x <- sub('^=', 'name:', x)
  y <- str_split_fixed(x[grep(":", x)], ":", 2)
  y1 = y[,1]; y2 = y[,2]
  meta  = as.list(y2[y1 != 'class'])
  names(meta) = y1[y1 != 'class']
  meta
#  filter_blank(meta)
}

