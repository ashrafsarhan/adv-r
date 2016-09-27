context('auth tests')

clientID = 'a223e7590d9043f1849bffa7ef86b102'
secret = 'aa351a255a8c49e7a4d69a8c2f893175'

test_that("Test Client Credentials Flow", {
  #Happy scenario
  expect_equivalent(status_code(auth(clientID, secret)), 200)
  #Sad scenario
  expect_equivalent(status_code(auth('clientID', 'secret')), 400)
})
