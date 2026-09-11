# Load pure, data-free helpers only. These tests deliberately avoid sourcing the
# SRS setup or touching any restricted data/database connections.
source(file.path("R", "functions_send.R"))
source(file.path("R", "functions_models.R"))
