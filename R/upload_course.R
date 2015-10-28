#' Create or update a course
#' 
#' Uploads the \code{course.yml} file to datacamp.com. 
#' Use this function to change the course title, description, etc. and to update the chapters' ordering.
#' 
#' If you're not yet logged in when calling this function, you'll be prompted to log in.
#' 
#' @param course_folder path to folder containing the course yml file.
#' @param open boolean, TRUE by default, determines whether a browser window should open, showing the course creation web interface
#' @param force boolean, FALSE by default, that allows to remove chapters from the live course that are not in the course.yml
#' @param upload_chapters, boolean, FALSE by default, that specifies whether to also upload the chapters
#' @examples 
#' \dontrun{
#' upload_course()
#' upload_course(upload_chapters = TRUE) # also upload chapters listed in course.yml
#' }
#' @export
upload_course = function(course_folder = ".", open = TRUE, force = FALSE, upload_chapters = FALSE) { 
  
  if (!datacamp_logged_in()) { datacamp_login() }
  
  # Move to folder containing course and chapter files
  old_wd <- getwd()
  setwd(course_folder)
  
  # Build course object from yml
  message("Parsing course file...")
  course <- load_course_file()
  
  if (is.null(course$id)) {
    sure <- readline("No id found in course.yml. This will create a new course, are you sure you want to continue? (Y/N) ")
    if (!(sure %in% c("y", "Y", "yes", "Yes"))) { return(message("Aborted.")) }
  }
  
  if(isTRUE(force)) {
    sure <- readline("Using 'force' will delete chapters online that are not specified in your course.yml. Are you sure you want to continue? (Y/N) ")
    if (!(sure %in% c("y", "Y", "yes", "Yes"))) { return(message("Aborted.")) }
    course$force = TRUE
  }
  
  message("Converting course info to json...")
  course_json <- RJSONIO::toJSON(course)
  
  message("Uploading chapter to datacamp.com ...")
  upload_course_json(course_json, open)
  
  if(upload_chapters) {
    chapter_files_in_yaml <- names(course$chapters)
    if(!is.null(chapter_files_in_yaml)) {
      lapply(chapter_files_in_yaml, upload_chapter, open = FALSE, force = force, ask = FALSE)  
    }
    
    # Check if chapter files in working directory that are not in course.yml
    chapter_files_in_dir <- dir(pattern = chapter_search_pattern)
    if(length(setdiff(chapter_files_in_dir, chapter_files_in_yaml)) > 0) {
      sure <- readline("Some chapters in your working directory are not yet uploaded to DataCamp. Do this now? (Y/N) ")
      if (!(sure %in% c("y", "Y", "yes", "Yes"))) { return(message("Aborted.")) }
      lapply(setdiff(chapter_files_in_dir, chapter_files_in_yaml), upload_chapter, open = FALSE, force = force, ask = FALSE)
      # Make sure order is correct by re-uploading
      upload_course(open = FALSE, force = FALSE, upload_chapters = FALSE)
    }
  }
  
  # reset working directory
  setwd(old_wd)
  invisible(NULL)
}


#' Upload the course json
#' @param course_json the JSON string to be posted
#' @param open whether or not to open the teach website after upload.
#' 
#' @importFrom httr POST content add_headers
upload_course_json = function(course_json, open) { 
  base_url = paste0(datacamp$get("base_url"), "/courses/create_from_r.json")
  auth_token = datacamp$get("auth_token")
  url = paste0(base_url,"?auth_token=", auth_token)
  x = try(POST(url = url, body = course_json, add_headers(c(`Content-Type` = "application/json", `Expect` = ""))))
  if ( class(x) != "response" ) {
    stop(no_response)
  } else {
    if (is.list(content(x)) ) {
      if ("course" %in% names(content(x))) {
        course = content(x)$course
        new = content(x)$created
        if (new == TRUE) {
          message(sprintf("Created course \"%s\" with id %i.", course$title, course$id))
        } else {
          message(sprintf("Updated course \"%s\" (id: %i)", course$title, course$id))
        }
        add_id_to_course_file(course$id) # write id to course.yml file if it's not already there
        if (open) { 
          browseURL(paste0(datacamp$get("redirect_base_url"), "/", course$id))
        }
        if ("message" %in% names(content(x))) {
          message(content(x)$message)
        }
      } else if ( "error" %in% names(content(x)) ) {
        message(paste0("Something went wrong:\n", content(x)$error ))
      }
    } else {
      message(paste0("Something went wrong. Please check whether your course was correctly uploaded to DataCamp."))
    }
  }
}