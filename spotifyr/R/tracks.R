# You can test the following two methods using the following tracks IDs
# trackIDs = '1CUVN2kn7mW5FjkqXTR2W1,387r02a1k6RZ4cwFraHkee'

getTracks <- function(trackIDs, market, access_token) {
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/tracks?ids=', trackIDs, '&market=', market)
  response = GET(url = URI, add_headers(Authorization = HeaderValue))
  return(response)
}

getAudioFeatures <- function(trackIDs, access_token) {
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/audio-features?ids=', trackIDs)
  response = GET(url = URI, add_headers(Authorization = HeaderValue))
  return(response)
}
