.onAttach <- function(...) {
  packageStartupMessage("Everybody can teach on DataCamp! Use these functions to build your own course:\n",
                        " - datacamp_login(): Log in to DataCamp from within R.\n",
                        " - author_course(): Create a course and chapter template to get you started.\n",
                        " - author_chapter(): Create new chapter templates to grow your course.\n",
                        " - add_exercise(): Add exercise templates to your chapter files.\n",
                        " - upload_course(): Upload or update your course file on DataCamp.\n",
                        " - upload_chapter(): Upload or update your chapter files on DataCamp.\n",
                        "For an easier way to create DataCamp courses, visit www.datacamp.com/teach\n")
}