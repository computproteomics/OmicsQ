test_that("log_operations contains the app version (no browser)", {
  source(file.path("..", "..", "ui.R"), chdir = TRUE)
  source(file.path("..", "..", "server.R"), chdir = TRUE)

  shiny::testServer(server, {
    lo <- log_operations()
    expect_equal(lo$omicsQ_version, readLines(file.path("..", "..", "VERSION")))
  })
})
