#' Create or update a course
#' 
#' Uploads the \code{course.yml} file to datacamp.com. Use this function to change the course title, description, etc. and to update the chapters' ordering.
#' 
#' If you're not yet logged in when calling this function, you'll be prompted to log in.
#' 
#' @usage upload_course(open = TRUE, force = FALSE)
#' @param open boolean, TRUE by default, determines whether a browser window should open, showing the course creation web interface
#' @param force boolean, FALSE by default, that allows to remove chapters from the live course that are not in the course.yml
#' @examples 
#' \dontrun{
#' upload_course()
#' }
#' @export
upload_course = function(open = TRUE, force = FALSE) { 
  if (!datacamp_logged_in()) { datacamp_login() }
  course = load_course_yml()
  
  # TODO?
  if (is.null(course$id)) {
    sure = readline("No id found in course.yml. This will create a new course, are you sure you want to continue? (Y/N) ")
    if (!(sure == "y" || sure == "Y" || sure == "yes" || sure == "Yes")) { return(message("Aborted.")) }
  }
  
  if (force == TRUE) {
    sure = readline("Using 'force' will delete chapters online that are not specified in your course.yml. Are you sure you want to continue? (Y/N) ")
    if (!(sure == "y" || sure == "Y" || sure == "yes" || sure == "Yes")) { return(message("Aborted.")) }
    course$force = TRUE
  }
  
  course$chapters = lapply(course$chapters, function(x) { as.integer(x) }) # put ids in array
  the_course_json = RJSONIO::toJSON(course)
  upload_course_json(the_course_json)
}


#' Upload the course json
#' @param theJSON the JSON string to be posted
#' @param open whether or not to open the teach website after upload.
#' 
#' @importFrom httr POST content add_headers
upload_course_json = function(theJSON, open = TRUE) { 
  base_url = paste0(.DATACAMP_ENV$base_url, "/courses/create_from_r.json")
  auth_token = .DATACAMP_ENV$auth_token
  url = paste0(base_url,"?auth_token=", auth_token)
  x = try(POST(url = url, body = theJSON, add_headers(c(`Content-Type` = "application/json", `Expect` = ""))))
  if ( class(x) != "response" ) {
    stop("Something went wrong. We didn't get a valid response from the datacamp server. Try again or contact info@datacamp.com in case you keep experiencing this problem.")
  } else {
    if (is.list(content(x)) ) {
      if ("course" %in% names(content(x))) {
        course = content(x)$course
        new = content(x)$created
        if (new == TRUE) {
          message(paste0("A new course was created with id ", course$id," and title \"", course$title,"\".")) 
        } else {
          message(paste0("Existing course (id:", course$id,"): \"", course$title,"\" was updated."))
        }
        add_id_to_course_yml(course$id) # write id to course.yml file if it's not already there
        if (open) { 
          browseURL(paste0(.DATACAMP_ENV$redirect_base_url, "/", course$id))
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