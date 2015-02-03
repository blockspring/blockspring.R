#'
#'
#' This function allows you to run code remotely.
#' @param block A string.
#' @param data A list.
#' @param api_key A string.
#' @return This function returns the result of running a blockspring block.


blockspringParse <- function( input_params, json_parsed = TRUE) {
  request = list(
    params = list(),
    `_errors` = list(),
    `_headers` = list(),
    getErrors = function(){
      return(request$`_errors`)
    },
    addError = function(error){
      request$`_errors`[[length(request$`_errors`) + 1]] <<- error
    },
    addHeaders = function(headers){
      request$`_headers` <<- headers
    },
    getHeaders = function(){
      return(request$`_headers`)
    }
  )

  if (json_parsed == TRUE) {
    params = input_params
  } else {
    tryCatch({
      params = fromJSON(readLines(input_params, warn = FALSE))
    }, error = function(err){
      stop("You didn't pass valid json inputs.")
    })
  }

  if (!(is.list(params)) || is.null(names(params))) {
    stop("Can't parse keys/values from your json inputs.")
  }

  if (!((exists("_blockspring_spec",params)) && isTRUE(params$`_blockspring_spec`))){
    request$params = params
  } else {
    for (var_name in names(params)){
      if (var_name =="_blockspring_spec") {
        #pass
      }
      else if ((var_name == "_errors") && (is.list(params[[var_name]]))) {
        for (error in params[[var_name]]){
          if((is.list(error)) && exists("title", error)){
            request$addError(error)
          }
        }
      }
      else if ((var_name == "_headers") && (is.list(params[[var_name]]))) {
        request$addHeaders(params[[var_name]])
      }
      else if ((is.list(params[[var_name]]) && !(is.null(names(params[[var_name]])))) &&
                (exists("filename", params[[var_name]])) &&
                (!(is.null(params[[var_name]]$filename))) &&
                 (((exists("data", params[[var_name]])) && !(is.null(params[[var_name]]$data))) ||
                 ((exists("url", params[[var_name]])) && !(is.null(params[[var_name]]$url))))
               ){
        tmp_file_path = tempfile(pattern = "", tmpdir = tempdir(), fileext = paste("-",params[[var_name]][["filename"]], sep=""))

        if(exists("data", params[[var_name]])){
          tryCatch({
            tmp_file = file(tmp_file_path, "wb")
            writeBin(base64Decode(params[[var_name]][["data"]], mode="raw"), tmp_file)
            close(tmp_file)
            request$params[[var_name]] = tmp_file_path
          },
          error = function(err){
            request$params[[var_name]] = params[[var_name]]
          })
        } else {
          download.file(params[[var_name]][["url"]],tmp_file_path, method="curl", quiet=TRUE)
          request$params[[var_name]] = tmp_file_path
        }
      }
      else {
        request$params[[var_name]] = params[[var_name]]
      }
    }
  }

  return(request)
}