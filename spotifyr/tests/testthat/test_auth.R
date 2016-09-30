context('auth tests')

clientID = 'a223e7590d9043f1849bffa7ef86b102'
secret = 'aa351a255a8c49e7a4d69a8c2f893175'

test_that("Test Client Credentials Flow", {
  #Happy scenario
  expect_equivalent(auth(clientID, secret)$status_code, 200)
})

test_that("Test with invalid ID/secret", {
  expect_error(auth(artistID,TRUE))
})