
test_that("dijkstra", {
  expect_that(dijkstra(wiki_graph,20))
  expect_that(dijkstra(wiki_graph,3))
})

