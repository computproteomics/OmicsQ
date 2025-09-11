test_that("App constructs server without browser (smoke)", {
  # Source UI (loads modules) and server
  source(file.path("..", "..", "ui.R"), chdir = TRUE)
  source(file.path("..", "..", "server.R"), chdir = TRUE)

  expect_true(is.function(ui))
  expect_true(is.function(server))

  # Initialize server in test mode to ensure it binds without errors
  shiny::testServer(server, {
    # log_operations reactiveVal should be available and contain version
    lo <- log_operations()
    expect_type(lo$omicsQ_version, "character")
  })
})
