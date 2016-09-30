#' Building a function to auth the user
#'
#' This function auth the user
#'
#'@param clientID char spotify clientID
#'@param secret char spotify secret
#'@return a response
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function authenticate the user
#'@references
#'\url{https://developer.spotify.com/web-api/authorization-guide/}
#'@seealso \code{\link{auth}}
#'@import httr
#'@export
auth <- function(clientID, secret) {
  response = POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body = list(grant_type = 'client_credentials'),
    encode = 'form'
    #verbose(data_out = FALSE) Not necessary to print all information every time
  )
  if((status_code(response) %% 400) %in% c(1:99) ){
    stop("Bad request")
  } else if((status_code(response) %% 500) %in% c(1:99)){
    stop("Server failed")
  } else if((status_code(response) %% 300) %in% c(1:99)){
    stop("Redirections")
  } else if((status_code(response) %% 100) %in% c(1:99)){
    stop("Information from server")
  }
  return(response)
}
