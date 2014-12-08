#'
#'
#' This function defines a new blockspring function.
#' @param block A function.
#' @return This function doesn't return anything, it strictly defines a new blockspring function.

 blockspringDefine <- function(block) {
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
      request$`_headers` = headers
    },
    getHeaders = function(){
      return(request$`_headers`)
    }
  )

  response = list(
    result = list(
      `_blockspring_spec` = TRUE,
      `_errors` = list()
    ),
    addOutput = function(name, value){
      response$result[[name]] <<- value
      return(response)
    },
    addFileOutput = function(name, filepath){
      newFile = file(filepath)
      data = readBin(filepath, raw(), file.info(filepath)$size)
      filename = basename(filepath)

      response$result[[name]] <<- list(
        "filename" = filename,
        "content-type" = guess_type(filename),
        "data" = base64(data)[[1]]
      )
      close(newFile)
      return(response)
    },
    addErrorOutput = function(title, message = NULL){
      response$result$`_errors`[[length(response$result$`_errors`) + 1]] <<- list("title" = title, "message" = message)
      return(response)
    },
    end = function(){
      cat(paste(toJSON(response$result),"",sep="\n"))
    }
  )

  processStdin = function(request){
    if(!(isatty(stdin()))){
      stdin = file('stdin')
      request = blockspringParse(stdin, json_parsed = FALSE)
      close(stdin)

      return(request)
    } else {
      return(request)
    }
  }

  processArgs = function(request){
    argv <- commandArgs(asValues=TRUE, trailingOnly=TRUE)
    for (key in names(argv)){
      request$params[[key]] <<- argv[[key]]
    }
    return(request)
  }

  request = processArgs(processStdin(request))

  block(request, response)
}
