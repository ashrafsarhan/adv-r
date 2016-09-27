#' Building a function to auth the user
#'
#' This function auth the user
#'
#'@param clientID char spotify clientID
#'@param secret char spotify secret
#'@return a response
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function auth the user
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
    encode = 'form',
    verbose()
  )
  return(response)
}
