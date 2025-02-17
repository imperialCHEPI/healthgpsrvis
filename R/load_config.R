#' Load configuration file
#'
#' This function loads the configurations from the config.yml file located
#' in the inst/config directory using the `config` package.
#' @param config_name The name of the configuration environment to load (e.g.,
#' "default", "development", "production", "testing").
#' @return A list containing the configuration data.
#' @export
load_config <- function(config_name = "default") {
  config_file_path <- system.file("config",
                                  "config.yml",
                                  package = "healthgpsrvis")
  config::get(file = config_file_path, config = config_name)
}
