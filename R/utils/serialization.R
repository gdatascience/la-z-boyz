#' Serialization utility functions for data persistence
#'
#' Helper functions for saving/loading RDS files with metadata attributes,
#' exporting to CSV with UTF-8 encoding, and handling file system errors
#' gracefully.

#' Log an error message with timestamp
#' @param msg Character message to log
#' @keywords internal
ser_log_error <- function(msg) {
  message(sprintf("[%s] ERROR: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

#' Log a warning message with timestamp
#' @param msg Character message to log
#' @keywords internal
ser_log_warn <- function(msg) {
  message(sprintf("[%s] WARN: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

#' Log an info message with timestamp
#' @param msg Character message to log
#' @keywords internal
ser_log_info <- function(msg) {
  message(sprintf("[%s] INFO: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

#' Save an R object to RDS with metadata attributes
#'
#' Attaches source, timestamp, and league_id metadata as attributes before
#' saving. Handles file system errors gracefully: logs the error and returns
#' the data unchanged so in-memory results are preserved.
#'
#' @param data R object to save (data frame, list, or any serializable object)
#' @param file_path Character: path to the output .rds file
#' @param source Character: description of the data source (e.g., URL, file path)
#' @param league_id Character: league identifier (default "l-z-bs")
#' @return Invisibly returns the data with metadata attributes attached.
#'   On file system error, returns the data unchanged (not saved to disk).
#' @export
save_rds_with_metadata <- function(data, file_path, source, league_id = "l-z-bs") {
  # Attach metadata attributes

  attr(data, "source_file") <- source
  attr(data, "parsed_at") <- Sys.time()
  attr(data, "league_id") <- league_id

  # Attempt to save, handling file system errors gracefully

  tryCatch(
    {
      # Ensure directory exists
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }

      saveRDS(data, file_path)
      ser_log_info(sprintf("Saved RDS: %s (source=%s, league_id=%s)",
                           file_path, source, league_id))
    },
    error = function(e) {
      ser_log_error(sprintf(
        "Failed to save RDS to '%s': %s. In-memory results retained.",
        file_path, conditionMessage(e)
      ))
      warning(
        "File system error saving to '", file_path, "': ", conditionMessage(e),
        ". In-memory results are still available.",
        call. = FALSE
      )
    }
  )

  invisible(data)
}

#' Load an RDS file with corruption handling
#'
#' Safely reads an RDS file. If the file is missing or corrupted (unreadable),
#' warns the user and suggests re-running the ingest pipeline. Returns NULL
#' on failure rather than crashing.
#'
#' @param file_path Character: path to the .rds file to load
#' @return The deserialized R object on success, or NULL if the file is
#'   missing or corrupted.
#' @export
load_rds_safe <- function(file_path) {
  # Check file existence

  if (!file.exists(file_path)) {
    ser_log_warn(sprintf("RDS file not found: '%s'", file_path))
    warning(
      "File not found: '", file_path, "'. ",
      "Run the appropriate ingest script to generate this data.",
      call. = FALSE
    )
    return(NULL)
  }

  # Attempt to read, handling corruption

  tryCatch(
    {
      data <- readRDS(file_path)
      ser_log_info(sprintf("Loaded RDS: %s", file_path))
      data
    },
    error = function(e) {
      ser_log_error(sprintf(
        "Corrupted RDS file '%s': %s",
        file_path, conditionMessage(e)
      ))
      warning(
        "RDS file appears corrupted: '", file_path, "'. ",
        "Please re-run the ingest pipeline to regenerate this file. ",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

#' Export a data frame to CSV with UTF-8 encoding
#'
#' Writes data to CSV using UTF-8 encoding and comma delimiters.
#' Handles file system errors gracefully: logs the error and returns
#' the data unchanged so in-memory results are preserved.
#'
#' @param data Data frame to export
#' @param file_path Character: path to the output .csv file
#' @param encoding Character: file encoding (default "UTF-8")
#' @return Invisibly returns the data. On file system error, returns
#'   the data unchanged (not saved to disk).
#' @export
export_csv <- function(data, file_path, encoding = "UTF-8") {
  if (!is.data.frame(data)) {
    stop("export_csv expects a data frame. Got: ", class(data)[1], call. = FALSE)
  }

  tryCatch(
    {
      # Ensure directory exists
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }

      # Write CSV with specified encoding
      con <- file(file_path, open = "w", encoding = encoding)
      on.exit(close(con), add = TRUE)
      write.csv(data, con, row.names = FALSE, fileEncoding = encoding)

      ser_log_info(sprintf("Exported CSV: %s (encoding=%s, rows=%d, cols=%d)",
                           file_path, encoding, nrow(data), ncol(data)))
    },
    error = function(e) {
      ser_log_error(sprintf(
        "Failed to export CSV to '%s': %s. In-memory results retained.",
        file_path, conditionMessage(e)
      ))
      warning(
        "File system error exporting CSV to '", file_path, "': ", conditionMessage(e),
        ". In-memory results are still available.",
        call. = FALSE
      )
    }
  )

  invisible(data)
}
