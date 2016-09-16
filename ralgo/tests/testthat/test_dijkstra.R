context('dijkstra tests')

test_that("dijkstra", {
  expect_error(dijkstra(iris[,c(1,2)],20))
  expect_error(dijkstra(wiki_graph,99))
})

test_that("dijkstra", {
  expect_equivalent(dijkstra(wiki_graph, 1), c(0,7,9,20,20,11))
  expect_equivalent(dijkstra(wiki_graph, 3), c(9,10,0,11,11,2))
})