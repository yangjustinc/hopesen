# Load pure, data-free helpers only. These tests deliberately avoid sourcing the
# SRS setup or touching any restricted data/database connections.
project_root <- if (file.exists(file.path("R", "functions_send.R"))) {
  "."
} else {
  file.path("..", "..")
}

source(file.path(project_root, "R", "functions_send.R"))
source(file.path(project_root, "R", "functions_models.R"))
rm(project_root)
