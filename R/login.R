#' Log in to DataCamp.com via R
#' 
#' To be able to upload your course to the DataCamp platform, you need to log in to set-up the connection. The function will prompt for your
#' DataCamp username and password, and will then log you into the DataCamp server. Optionally, a subdomain can be specified (the default is 
#' www.DataCamp.com). Note, in addition to the log in via R, it is also necessary to log into DataCamp.com via your browser.
#' 
#' 
#' @usage datacamp_login()
#' @return No return values.
#' @export
datacamp_login = function(email = NULL, pw = NULL, subdomain = NULL) {
  if(is.null(email))
    email = readline("Email: ")
  if(is.null(pw)) {
    if (exists(".rs.askForPassword")) {
      pw <- .rs.askForPassword("Password: ")
    } else {
      pw = readline("Password: ")
    }  
  }
  if(is.null(subdomain))
    subdomain = readline("Subdomain (leave empty for default): ")
  
  if (subdomain == "" || subdomain == " ") {
    base_url = paste0("https://api.datacamp.com")
    redirect_base_url = paste0("https://teach.datacamp.com/courses")
  } else if (subdomain == "localhost") {
    base_url = "127.0.0.1:3000"
    redirect_base_url = "http://localhost:9000/courses"
  } else {
    base_url = paste0("http://api-", subdomain, ".datacamp.com")
    redirect_base_url = paste0("http://teach-", subdomain, ".datacamp.com/courses")
  }
  
  url = paste0(base_url, "/users/details.json?email=", curlEscape(email), "&password=", curlEscape(pw)) 
  message("Logging in...")
  if (url.exists(url, ssl.verifypeer=FALSE)) {
    getURL(url, ssl.verifypeer=FALSE)
    content = getURLContent(url, ssl.verifypeer=FALSE)
    auth_token = fromJSON(content)$authentication_token
    .DATACAMP_ENV <<- new.env()
    .DATACAMP_ENV$auth_token = auth_token
    .DATACAMP_ENV$email = email
    .DATACAMP_ENV$base_url = base_url
    .DATACAMP_ENV$redirect_base_url = redirect_base_url
    message(paste0("Logged in successfully to datacamp.com with R! Make sure to log in with your browser to datacamp.com as well using the same account."))
  } else {
    stop("Wrong user name  or password for datacamp.com.")
  } 
}

datacamp_logged_in = function() {
  if (exists(".DATACAMP_ENV")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}