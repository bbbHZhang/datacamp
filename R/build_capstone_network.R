#' Build graph structure of Capstone chapter
#' 
#' @inheritParams parse_chapter
#' 
#' @export
show_capstone_graph <- function(chapter_file) {
  chap_list <- parse_chapter(chapter_file)
  if(!require("networkD3")) {
    sure <- readline("To show the graph, you have to install the igraph package. Do this now? (Y/N) ")
    if (!(sure %in% c("y", "Y", "yes", "Yes"))) { return(message("Aborted.")) }
    install.packages("networkD3")
    require("networkD3")
  }
  
  get_edges <- function(ex) {
    if(ex$type == "CapstoneVideoExercise" || ex$type == "CapstoneNormalExercise") {
      return(c(ex$number, ex$next_exercise_number))
    } else if (ex$type == "CapstoneMultipleChoiceExercise") {
      nexts <- sapply(ex$instructions, `[[`, "next_exercise_number")
      matrix(c(rep(ex$number, length(nexts)), nexts), ncol = 2)
    } else {
      stop(sprintf("%s is not supported in a capstone chapter", exercises[[i]]$type))
    }
  } 
  
  edgesets <- sapply(chap_list$exercises, get_edges, USE.NAMES = FALSE)
  
  edgelist <- do.call(rbind, edgesets)
  edgelist <- edgelist[edgelist[,2] != 0,]
  
  lut <- sapply(chap_list$exercises, `[[`, "title")
  names(lut) <- as.numeric(sapply(chap_list$exercises, `[[`, "number"))
  
  # nodes <- data.frame(name = lut, group = ifelse(sapply(chap_list$exercises, `[[`, "optimal"), 1, 2))
  links <- data.frame(src = lut[as.character(edgelist[,1])], target = lut[as.character(edgelist[,2])])
  
  simpleNetwork(links)
}