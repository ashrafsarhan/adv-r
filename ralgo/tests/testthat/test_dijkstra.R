context('dijkstra tests')

test_that("dijkstra", {
  expect_error(dijkstra(iris[,c(1,2)],20))
  expect_error(dijkstra(wiki_graph,99))
})

