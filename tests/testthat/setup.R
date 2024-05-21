withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = testthat::teardown_env(),
  EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
)

tryCatch({
  if (Sys.getenv("skip_eunomia_download_test") != "TRUE") CDMConnector::downloadEunomiaData(overwrite = TRUE)
}, error = function(e) NA)
