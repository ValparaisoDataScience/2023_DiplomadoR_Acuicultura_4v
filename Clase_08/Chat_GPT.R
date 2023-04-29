library(devtools)
library(openai)
library(jsonlite)
library(pak)
library(gptstudio)
pak::pak("MichelNivard/gptstudio")
require(usethis)
edit_r_environ(scope = "project")

devtools::install_github("MichelNivard/gptstudio")
Sys.setenv(OPENAI_API_KEY = "")

Sys.getenv("OPENAI_API_KEY")









