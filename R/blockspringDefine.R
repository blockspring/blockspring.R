#'
#'
#' This function defines a new blockspring function.
#' @param block A function.
#' @return This function doesn't return anything, it strictly defines a new blockspring function.  

 blockspringDefine <- function(block) {
  if(missing(block)){
    stop("your forgot to pass in your function!")
  }

  result = list(
    data = list(),
    files = list(),
    errors = NA
  )

  request = list(
    params = list(),
    stdin = ""
  )

  response = list(
    addOutput = function(name, value){
      result$data[[name]] <<- value
      return(response)
    },
    addFileOutput = function(name, filepath){
      filename = basename(filepath)
      newFile = file(filename)
      data = readLines(newFile, warn = FALSE )

      result$files[[name]] <<- list(
        filename = filename,
        mimeType = guess_type(filepath),
        data = base64(data)[[1]]
      )
      close(newFile)
      return(response)
    },
    end = function(){
      output = toJSON(result)
      cat(output)
    }
  )

  processStdin = function(){
    if(!(isatty(stdin()))){
      stdin = file('stdin')
      data = fromJSON(readLines(stdin))
      request$params <<- data
      close(stdin)
    }
  }

  processArgs = function(){
    argv <- commandArgs(asValues=TRUE, trailingOnly=TRUE)
    for (key in names(argv)){
      if (is.na(match(key, c("_", "$0")))) {
        request$params[[key]] <<- argv[[key]]
      }
    }
  }

  processStdin()
  processArgs()

  block(request, response)
}
