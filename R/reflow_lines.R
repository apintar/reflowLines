my_wrap <- function (string, width = 80, indent = 0, exdent = 0) {

  if (width <= 0) {
    width <- 1
  }

  out <- stringi::stri_wrap(string, width = width,
                            indent = indent, exdent = exdent,
                            whitespace_only = TRUE, simplify = FALSE)
  vapply(out, stringr::str_c, collapse = "\n", character(1))
}
#' @title reflowLines
#'
#' @description reflowLines
#'
#' @export
#'
reflowLines <- function () {

  tmp <- rstudioapi::getActiveDocumentContext()
  start_pos <- tmp$selection[[1]]$range$start
  end_pos <- tmp$selection[[1]]$range$end

  # count leading white space
  lws <- nchar(tmp$contents[start_pos[1]]) -
    nchar(stringr::str_trim(tmp$contents[start_pos[1]]))
  if (start_pos[1] == end_pos[1] && start_pos[2] == end_pos[2]){

    replacement_text <- my_wrap(tmp$contents[start_pos[1]], width = 70,
                                indent = lws, exdent = lws)
    current_length <- nchar(tmp$contents[start_pos[1]])
    replacement_range <- rstudioapi::document_range(start = c(start_pos[1], 1),
                                                    end = c(start_pos[1], current_length + 1))
  } else {

    if (nchar(tmp$contents[end_pos[1]]) == 0) {
      # a blank line is part of the selection (no spaces even)

      replacement_text <- paste0(tmp$contents[start_pos[1]:(end_pos[1] - 1)],
                                 collapse = ' ')
      replacement_text <- my_wrap(replacement_text, width = 70,
                                  indent = lws, exdent = lws)
      last_length <- nchar(tmp$contents[end_pos[1] - 1])
      replacement_range <- rstudioapi::document_range(start = c(start_pos[1], 1),
                                                      end = c(end_pos[1] - 1, last_length + 1))
    } else {

      replacement_text <- paste0(tmp$contents[start_pos[1]:end_pos[1]],
                                 collapse = ' ')
      replacement_text <- my_wrap(replacement_text, width = 70,
                                  indent = lws, exdent = lws)
      last_length <- nchar(tmp$contents[end_pos[1]])
      replacement_range <- rstudioapi::document_range(start = c(start_pos[1], 1),
                                                      end = c(end_pos[1], last_length + 1))
    }
  }

  rstudioapi::modifyRange(location = replacement_range,
                          text = replacement_text,
                          id = tmp$id)
}
