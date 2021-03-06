# You can test this method using the following Frank Sinatra and shawn Mendes spotify artist IDs
# artistID = '1HBjj22wzbscIZ9sEb5dyf,7n2wHs1TKAczGzO7Dd2rGr'
#' Building a function to access to the specific artist
#'
#' This function is building the access to the specific artist
#'
#'@param artistID a character of the ID of spotify artist
#'@param access_token a charater of the access token
#'@return a response
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function is to get the acesss to the artists
#'@references
#'\url{https://developer.spotify.com/web-api/authorization-guide/}
#'@seealso \code{\link{auth}}
#'@import httr
#'@export

getArtists <- function(artistIDs, access_token) {
  stopifnot(is.character(artistIDs),is.character(access_token))
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/artists?ids=', artistIDs)
  response = GET(url = URI, add_headers(Authorization = HeaderValue))
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
