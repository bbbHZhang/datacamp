#' Automatic add function links to RDocumentation to course (beta)
#' 
#' The \code{add_all_links} function will use the course yaml file to add all links to quoted functions. It will
#' automatically search for the relevant RDocumentation link. 
#' 
#' The function will first look within relevant libraries (the ones manually loaded in the chapter file). Afterwards
#' it will look in autoloaded libraries and finally it will look in all installed libraries for the relevant 
#' help file link.
#' 
#' IMPORTANT: This function will create backup files, as it overwrites the original chapter file. The automatic link generation
#' can be undone using the \code{undo_all_links} function. But only use this function right after generating the links,
#' as it uses the generated backup files. Changes done to the chapter file between using \code{add_all_links} and 
#' \code{undo_all_links} will be undone.
#' 
#' @param backup_ext The extension to the name of the backup files. Default is "_back_no_links".
#' @param autoloads The libraries that are autoloaded into R. 
#'                 Default is c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats").
#' @param course_yml The name of the course yaml file. Default is "course.yml".
#' 
#' @export
add_all_links <- function(backup_ext = "_back_no_links", 
                          autoloads = c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats"),
                          course_yml = "course.yml") {
  if (file.exists(course_yml)) {
    course <- yaml.load_file(course_yml)
    chapters <- course$chapters
    if (!is.null(chapters)) {
      files <- names(chapters)
      invisible(lapply(files, replace_back, backup_ext = backup_ext))
      invisible(lapply(files, add_links, backup_ext = backup_ext, autoloads = autoloads))
      cat(
        paste0(
        " =================================================================================== \n",
        "| Adding links completed (BETA VERSION)                                             |\n",
        " =================================================================================== \n"
        )
      )
      message(
        paste0(
        " =================================================================================== \n",
        "| Note: check your chapter files and make sure they are not messed up               |\n",
        "| if they are messed up use undo_all_links immediately after running add_all_links  |\n",
        "| WARNING: THIS WILL REPLACE THE CHAPTER FILES WITH THEIR BACKUPS SO ALL CHANGES    |\n",
        "| DONE BETWEEN undo_all_links AND add_all_links WILL BE MADE UNDONE                 |\n",
        " =================================================================================== \n"
        )
      )
    }
  }
}

#' Undo all link creation using the backup files you created (beta)
#' 
#' The \code{undo_all_links} function should only be used right after using the \code{add_all_links} function.
#' If you inspect the code and the link generation seem to have messed up the course format.
#' This should not happen, but everything can happen in a beta version. Needs more testing.
#' 
#' @param backup_ext The extension to the name of the backup files. Default is "_back_no_links".
#' @param course_yml The name of the course yaml file. Default is "course.yml".
#' 
#' @export
undo_all_links <- function(backup_ext = "_back_no_links",
                           course_yml = "course.yml") {
  if (file.exists(course_yml)) {
    course <- yaml.load_file(course_yml)
    chapters <- course$chapters 
    if (!is.null(chapters)) {
      files <- names(chapters)
      invisible(lapply(files, replace_back, backup_ext = backup_ext))
    }
  }
}

#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
add_links <- function(file_rmd,
                      backup_ext = "_back_no_links",
                      autoloads = c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")) {
  # Configuration
  file        <- sub("\\.Rmd$", "", file_rmd)
  rdoc        <- "http://www.rdocumentation.org/packages/\\1/functions/\\2"
  libs_pat    <- paste0("^",file.path(".*?","library","(.*?)","help","(.*?)"),"$")
  
  # Backup the file
  file.copy(file_rmd,paste0(file, backup_ext, ".Rmd"))
  
  # Add all links
  text <- readLines(file_rmd)
  pattern <- "[^\\[]`([^`]*?)\\(\\)`"
  extract_libs_pat <- "library\\(\"?(.*?)\"?\\)"
  libraries <- unique(gsub(extract_libs_pat,"\\1", unique(unlist(str_extract_all(text,extract_libs_pat)))))
  libraries <- libraries[libraries != ""]
  relevant_libs <- paste0("(",paste0(libraries,collapse="|"),")")
  matches <- unique(unlist(str_extract_all(text,pattern)))
  matches <- gsub(pattern, "\\1", matches)
  links <- unlist(vapply(matches, function(x) {
    # Will first check for help in relevant packages (of which libraries are manually imported)
    # Second will search in autoloaded packages
    # Finally will search in all installed packages
    help_files <- index_search(x, find.package(dir(.libPaths())))
    relevant_files <- help_files[grepl(paste0("library/",relevant_libs),help_files)]
    if (length(relevant_files) > 0) {
      return(relevant_files[1])
    } else {
      autoload_libs <- paste0("(",paste0(autoloads,collapse="|"),")")
      
      autoload_files <- help_files[grepl(paste0("library/",autoload_libs),help_files)]
      if (length(autoload_files) > 0) {
        return(autoload_files[1])
      } else {
        return(help_files[1])
      }
    }
  },character(1)))
  links <- links[!is.na(links)]
  links <- unlist(vapply(links, function(x) 
    sub(libs_pat,rdoc,x),character(1)))
  for (match in names(links)) {
    link <- links[match]
    cat(sprintf("Adding %-15s -> %s\n",match,link))
    pattern <- paste0("([^\\[])`\\Q",match,"\\E\\(\\)`")
    new <- paste0("\\1[`",match,"()`](",link,")")
    text <- gsub(pattern,new,text)
  }
  
  # Write the file
  write(text,file_rmd)
}

replace_back <- function(file_rmd, 
                         backup_ext = "_back_no_links") {
  file        <- sub("\\.Rmd$", "", file_rmd)
  backup      <- paste0(file, backup_ext, ".Rmd")
  if (file.exists(backup)) {
    file.remove(file_rmd)
    file.rename(backup,file_rmd)
  }
}

index_search <- function (topic, paths, firstOnly = FALSE) {
  res <- character()
  for (p in paths) {
    if (file.exists(f <- file.path(p, "help", "aliases.rds"))) 
      al <- readRDS(f)
    else if (file.exists(f <- file.path(p, "help", "AnIndex"))) {
      foo <- scan(f, what = list(a = "", b = ""), sep = "\t", 
                  quote = "", na.strings = "", quiet = TRUE)
      al <- structure(foo$b, names = foo$a)
    }
    else next
    f <- al[topic]
    if (is.na(f)) 
      next
    res <- c(res, file.path(p, "help", f))
    if (firstOnly) 
      break
  }
  res
}