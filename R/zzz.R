.onAttach <- function(...) {
  packageStartupMessage("author_course(): Create a course template to get you started.\n",
                        "datacamp_login(): log in to DataCamp from within R.",
                        "upload_course(): upload or update your course file on DataCamp.\n",
                        "upload_chapter(): upload or update your chapter files on DataCamp.\n",
                        "For more information on course creation, visit https://www.teach.datacamp.com.\n")
}