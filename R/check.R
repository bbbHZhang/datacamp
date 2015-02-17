#' Check the course yaml object
check_course_object = function(course) {
  ## STEP 1: Check the course info
  # All basic info present in yaml?
  min_names = c("title", "author_field", "description")
  present_names = min_names %in% names(course)
  if (!all(present_names)) {
    stop(sprintf("Looks like your course.yml file is missing the information:\n%s\nHave a look at the documentation on %s.", 
               paste0(min_names[!present_names], collapse="\n"), doc_url()))
  }
  
  # Any empty elements
  min_course = course[min_names[present_names]]
  empty = sapply(min_course, function(x){
    is.null(x) || (x == "") || (x == " ") || is.na(x)
  })
  if (any(empty)) {
    stop(paste0("Looks like your course.yml file is missing information for the field(s):\n", 
               paste0(names(min_course[empty]), collapse="\n"), 
               ".\nThese cannot be empty. Please add that in your course.yml file.\nHave a look at the documentation on ", doc_url(),"."))
  }
  
  ## STEP 2: Check the chapters
  check_chapters(course)
  
  invisible()
}

#' Check the filenames of the different chapters
check_chapters = function(course) {
  chapters = course[["chapters"]]
  if (!is.null(chapters)) {
    chapters = unlist(chapters)
    
    pre = "Something is wrong with the chapters section in your course.yml file."
    check = sprintf("Please check the documentation at %s.", doc_url())
    
    if (!is.vector(chapters)) {
      stop(sprintf("%s\n%s", pre, check))
    }
    
    # Are the chapter ids unique and do they exist?
    empty_chapters = sapply(chapters, function(x){ is.null(x) || (x == "") || (x == " ") || is.na(x) })
    
    if (any(empty_chapters) || (length(course$chapters)!=length(chapters))) {
      stop(sprintf("%s\nYour chapter id can't be empty.\n%s.", pre, check))
    }
    
    if (length(chapters) != length(unique(chapters))) {
      stop(sprintf("%s\nYour chapter ids should be unique.\n%s", pre, check))
    }
    # Are the Rmd files unique and do they exist?
    if (length(names(chapters)) != length(unique(names(chapters)))) {
      stop(sprintf("%s\nYour chapter file names should be unique.\n%s", pre, check))
    }
    
    existing_files = file.exists(names(chapters))
    if (!all(all(existing_files))) {
      stop(paste0(pre, "\nThe following files are specified in the course.yml but do not exist in your working directory: ",
                 paste(names(chapters)[!existing_files], collapse = "\n"),"."))
    }    
  }
}
