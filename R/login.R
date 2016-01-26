#' Log in to DataCamp.com via R
#' 
#' To be able to upload your course to the DataCamp platform, you need to log in to set-up the connection. The function will prompt for your
#' DataCamp username and password, and will then log you into the DataCamp server. Optionally, a subdomain can be specified (the default is 
#' www.DataCamp.com). Note, in addition to the log in via R, it is also necessary to log into DataCamp.com via your browser.
#' 
#' @usage datacamp_login(email = NULL, pw = NULL, subdomain = NULL)
#' @param email Email of your DataCamp account
#' @param pw Password of your DataCamp account
#' @param subdomain The subdomain to login to.
#' 
#' @return No return values.
#' 
#' @importFrom RCurl curlEscape getURL getURLContent url.exists
#' @importFrom RJSONIO fromJSON
#' @export
datacamp_login = function(email = NULL, pw = NULL, subdomain = NULL) {
  if(is.null(email))
    email <- readline("Email: ")
  if(is.null(pw)) {
    if (exists(".rs.askForPassword")) {
      pw <- do.call(".rs.askForPassword", list(prompt = "Password: "))
    } else {
      pw <- readline("Password: ")
    }  
  }
  if(is.null(subdomain))
    subdomain = readline("Subdomain (leave empty for default): ")
  
  if (subdomain == "" || subdomain == " ") {
    base_url <- "https://www.datacamp.com/api"
    redirect_base_url = "https://www.datacamp.com/legacy/teach/courses"
  } else if (subdomain == "localhost") {
    base_url <- "127.0.0.1:3000/api"
    redirect_base_url = "http://localhost:9000/courses"
  } else {
    base_url <- sprintf("https://%s.datacamp.com/api", subdomain)
    redirect_base_url <- sprintf("https://%s.datacamp.com/legacy/teach/courses", subdomain)
  }
  
  url <- sprintf("%s/users/details.json?email=%s&password=%s", base_url, curlEscape(email), curlEscape(pw))
  message("Logging in...")
  if (url.exists(url, ssl.verifypeer=FALSE)) {
    getURL(url, ssl.verifypeer = FALSE)
    content <- getURLContent(url, ssl.verifypeer=FALSE)
    auth_token <- fromJSON(content)$authentication_token
    datacamp$set(auth_token = auth_token)
    datacamp$set(email = email)
    datacamp$set(base_url = base_url)
    datacamp$set(redirect_base_url = redirect_base_url)
    message(paste("Logged in successfully to datacamp.com through R!", 
                  "Make sure to log in with your browser to datacamp.com as well using the same account."))
  } else {
    datacamp$clear()
    stop("Wrong user name or password. Make sure to create an account on www.datacamp.com first!")
  } 
}

#' Is the user logged in?
datacamp_logged_in = function() {
  !is.null(datacamp$get("email"))
}