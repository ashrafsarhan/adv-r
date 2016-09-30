# You can test the following two methods using the following tracks IDs
# trackIDs = '1CUVN2kn7mW5FjkqXTR2W1,387r02a1k6RZ4cwFraHkee'

#' Building a function to access to the specific tracks
#'
#' This function is building the access to the specific tracks
#'
#'@param artistID a character of the ID of spotify tracks
#'@param access_token a charater of the access token
#'@param market a charater of the market country
#'@example trackIDs = '1CUVN2kn7mW5FjkqXTR2W1,387r02a1k6RZ4cwFraHkee'
#'@return a response
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function is to get the acesss to the specific tracks
#'@references
#'\url{https://developer.spotify.com/web-api/authorization-guide/}
#'@seealso \code{\link{auth}}
#'@export

getTracks <- function(trackIDs, market, access_token) {
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/tracks?ids=', trackIDs, '&market=', market)
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

getAudioFeatures <- function(trackIDs, access_token) {
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/audio-features?ids=', trackIDs)
  response = GET(url = URI, add_headers(Authorization = HeaderValue))
  if(status_code(response) == 400 ){
    stop("Bad request")
  } else if(status_code(response) == 500){
    stop("Server failed")
  } else if(status_code(response) == 300){
    stop("Redirections")
  } else if(status_code(response) == 100){
    stop("Information from server")
  }
  return(response)
}
