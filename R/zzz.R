.onAttach <- function(...) {
  packageStartupMessage("To author a course, use author_course().\n",
                        "To upload or update your course file on DataCamp, use upload_course().\n",
                        "To upload or update your chapter files on DataCamp, use upload_chapter().\n",
                        "For more information on course creation, visit https://www.teach.datacamp.com.\n")
}