# You can test this method using the following Frank Sinatra and shawn Mendes spotify artist IDs
# artistID = '1HBjj22wzbscIZ9sEb5dyf,7n2wHs1TKAczGzO7Dd2rGr'
#' Building a function to auth the user
#'
#' This function auth the user
#'
#'@param artistIDs...
#'@param access_token
#'@return a response
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details Returns information about a specific artists...
#'@references
#'\url{https://developer.spotify.com/web-api/authorization-guide/}
#'@seealso \code{\link{auth}}
#'@import httr
#'@export
getArtists <- function(artistIDs, access_token) {
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/artists?ids=', artistIDs)
  response = GET(url = URI, add_headers(Authorization = HeaderValue))
  stopifnot(response$status_code == 200)
  return(response)
}
