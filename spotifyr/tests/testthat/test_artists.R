context('artists tests')

clientID = 'a223e7590d9043f1849bffa7ef86b102'
secret = 'aa351a255a8c49e7a4d69a8c2f893175'
artistID = '1HBjj22wzbscIZ9sEb5dyf'
respons <- auth(clientID,secret) # Extract access_token later on...

test_that("Test valid ID/secret", {
  #Happy scenario
  expect_equivalent(getArtists(artistID,NULL)$status_code, 200)
})

artistID = 'aa351a255a8c49e7a4d69a8c2f893175###'

test_that("Test with invalid ID/secret", {
  #Sad scenario
  expect_error(getArtists(artistID,NULL)$status_code != 200)
})