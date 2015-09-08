#' Create or update a chapter
#' 
#' @param chapter_file path to the ".Rmd" file to be uploaded
#' @param force boolean, FALSE by default, specifies whether exercises should be removed. If set, will prompt for confirmation.
#' @param open boolean, TRUE by default, determines whether a browser window should open, showing the course creation web interface
#' @param ask boolean, TRUE by default, determines whether you are asked for confirmation if you set force to TRUE.
#' @return No return values.
#' @examples
#' \dontrun{
#' # Upload without possibly deleting existing exercises
#' upload_chapter("chapter1.Rmd")
#' 
#' # Completely sync online chapter with your local markdown file
#' upload_chapter("chapter1.Rmd", force = TRUE)
#' }
#' 
#' @export
upload_chapter = function(chapter_file, force = FALSE, open = TRUE, ask = TRUE) {

  if(!hasArg(chapter_file)) {
    return(message("Error: You need to specify a chapter Rmd file.")) 
  }
  
  old_wd <- getwd()
  setwd(dirname(chapter_file))
  chapter_file <- basename(chapter_file)
  
  if(!datacamp_logged_in()) { 
    datacamp_login() 
  }
  
  if (isTRUE(force) && isTRUE(ask)) {
    sure <- readline("Using 'force' deletes exercises. Are you sure you want to continue? (Y/N) ")
    if (!(sure %in% c("y", "Y", "yes", "Yes"))) { return(message("Aborted.")) }
  }
  
  chapter_index <- get_chapter_index(chapter_file)
  if (length(chapter_index) == 0 && isTRUE(ask)) {
    sure <- readline("Chapter not found in course file. This will create a new chapter, are you sure you want to continue? (Y/N) ")
    if (!(sure %in% c("y", "Y", "yes", "Yes"))) { return(message("Aborted.")) }
  }
  
  #   if (skip_validation == TRUE) {
  #     sure = readline("Using 'skip_validation' implies that the exercises will not be checked for correctness. Are you sure you want to continue? (Y/N) ")
  #     if (!(sure == "y" || sure == "Y" || sure == "yes" || sure == "Yes")) { return(message("Aborted.")) }
  #   }
  
  message("Parsing course file...")
  course <- load_course_file()
  if (is.null(course$id)) {
    stop("Error: course file does not contain a course id. Please upload your course before uploading chapters.")
  }
  
  message("Parsing R Markdown file...")
  chapter <- parse_chapter(chapter_file)
  
  message("Converting chapter content to json...")
  output_list <-  list(force = force,
                       skip_validation = TRUE,
                       course = course$id,
                       email = datacamp$get("email"),
                       chapter = chapter)
  
  # Extract chapter id and index from course.yml. If found, add to output_list
  if (length(chapter_index) != 0) { # existing chapter, add info to output_list
    output_list$chapter$id <- as.integer(course$chapters[[chapter_index]])
    output_list$chapter$number <- chapter_index
  }
  
  # Convert entire list to JSON
  chapter_json <- RJSONIO::toJSON(output_list)
  
  message("Uploading chapter to datacamp.com ...")
  upload_chapter_json(chapter_json, chapter_file, open = open) # Upload everything
  
  # reset working directory
  setwd(old_wd)
  
  invisible(NULL)
}

#' Upload the chapter json
#' @param chapter_json the JSON string to be posted
#' @param chapter_file chapter file name that is being uploaded
#' @param open whether or not to open the teach website after upload.
#' 
#' @importFrom httr POST content add_headers
upload_chapter_json = function(chapter_json, chapter_file, open = TRUE) {
  base_url = paste0(datacamp$get("base_url"), "/chapters/create_from_r.json")
  auth_token = datacamp$get("auth_token")
  url = paste0(base_url,"?auth_token=", auth_token)
  x = try(POST(url = url, body = chapter_json, add_headers(c(`Content-Type` = "application/json", `Expect` = ""))))
  
  if ( class(x) != "response" ) {
    stop("Something went wrong. We didn't get a valid response from the datacamp server. Try again or contact info@datacamp.com in case you keep experiencing this problem.")
  } else { 
    if (is.list(content(x)) ) { 
      if ("course" %in% names(content(x)) ) {  
        course = content(x)$course
        chapter = content(x)$chapter
        new = content(x)$created
        message(sprintf("Updated course \"%s\" (id: %i)", course$title, course$id))
        if (new == TRUE) {
          message(sprintf("Created chapter \"%s\" (id: %i)", chapter$title, chapter$id)) 
        } else {
          message(sprintf("Updated chapter \"%s\" (id: %i)", chapter$title, chapter$id))
        }
        add_chapter_to_course_file(chapter_file, as.integer(chapter$id))
        if (open) {
          browseURL(paste0(datacamp$get("redirect_base_url"), "/", course$id))
        } 
      } 
      if ("message" %in% names(content(x))) {
        message(content(x)$message)
      }
      if ("result" %in% names(content(x))) {
        if ("testresults" %in% names(content(x)$result)) {
          invisible(lapply(content(x)$result$testresults, function(x) message(x[[2]])))
        }
      }
      if ( "error" %in% names(content(x)) ) {
        message(paste0("Something went wrong:\n", content(x)$error ))
      } 
    } else {
      message(paste0("Something went wrong. Please check whether your course was correctly uploaded to datacamp.com."))
    } 
  } 
}