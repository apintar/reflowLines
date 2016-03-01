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
  if (start_pos[1] == end_pos[1] && start_pos[2] == end_pos[2]){

    replacement_text <- stringr::str_wrap(tmp$contents[start_pos[1]], width = 70)
    current_length <- nchar(tmp$contents[start_pos[1]])
    replacement_range <- rstudioapi::document_range(start = c(start_pos[1], 1),
                                                    end = c(start_pos[1], current_length + 1))
  } else {

    if (nchar(tmp$contents[end_pos[1]]) == 0) {

      replacement_text <- paste0(tmp$contents[start_pos[1]:(end_pos[1] - 1)],
                                 collapse = ' ')
      replacement_text <- stringr::str_wrap(replacement_text, width = 70)
      last_length <- nchar(tmp$contents[end_pos[1] - 1])
      replacement_range <- rstudioapi::document_range(start = c(start_pos[1], 1),
                                                      end = c(end_pos[1] - 1, last_length + 1))
    } else {

      replacement_text <- paste0(tmp$contents[start_pos[1]:end_pos[1]],
                                 collapse = ' ')
      replacement_text <- stringr::str_wrap(replacement_text, width = 70)
      last_length <- nchar(tmp$contents[end_pos[1]])
      replacement_range <- rstudioapi::document_range(start = c(start_pos[1], 1),
                                                      end = c(end_pos[1], last_length + 1))
    }
  }

  rstudioapi::modifyRange(location = replacement_range,
                          text = replacement_text,
                          id = tmp$id)
}