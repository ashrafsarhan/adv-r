context('tracks tests')

clientID = 'a223e7590d9043f1849bffa7ef86b102'
secret = 'aa351a255a8c49e7a4d69a8c2f893175'
artistID = '1HBjj22wzbscIZ9sEb5dyf'
trackIDs = '1CUVN2kn7mW5FjkqXTR2W1'
token <- content(auth(clientID,secret))$access_token

test_that("Test valid ID/secret", {
  #Happy scenario
  expect_equivalent(getTracks(artistID,"se",token)$status_code, 200)
})

test_that("Test with invalid ID/secret", {
  expect_error(getTracks(artistID,TRUE))
})