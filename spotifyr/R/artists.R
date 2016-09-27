# You can test this method using the following Frank Sinatra and shawn Mendes spotify artist IDs
# artistID = '1HBjj22wzbscIZ9sEb5dyf,7n2wHs1TKAczGzO7Dd2rGr'

getArtists <- function(artistIDs, access_token) {
  HeaderValue = paste0('Bearer ', access_token)
  URI = paste0('https://api.spotify.com/v1/artists?ids=', artistIDs)
  response = GET(url = URI, add_headers(Authorization = HeaderValue))
  return(response)
}
