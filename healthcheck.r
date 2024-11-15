#!/usr/bin/env Rscript
library(shiny)


log_message <- function(message) {
    cat(paste(Sys.time(), message, "\n"))
}


connect_to_app <- function() {
    tryCatch(
        {
            response <- httr::GET("http://0.0.0.0:3838")
            if (response$status_code == 200) {
                log_message("Shiny app is running")
                return(TRUE)
            } else {
                log_message(paste("Shiny app returned status code:", response$status_code))
                return(FALSE)
            }
        },
        error = function(e) {
            log_message(paste("Error checking Shiny app:", e$message))
            return(FALSE)
        }
    )
}

if (connect_to_app()) {
    quit(status = 0)
} else {
    quit(status = 1)
}
