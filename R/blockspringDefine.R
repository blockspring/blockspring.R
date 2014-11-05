#'
#'
#' This function defines a new blockspring function.
#' @param block A function.
#' @return This function doesn't return anything, it strictly defines a new blockspring function.  

 blockspringDefine <- function(block) {
  request = list(
    params = list(),
    `_errors` = list(),
    getErrors = function(){
      return(request$`_errors`)
    },
    addError = function(error){
      request$`_errors`[[length(request$`_errors`) + 1]] <<- error
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

  processStdin = function(){
    if(!(isatty(stdin()))){
      stdin = file('stdin')
      
      tryCatch({
        params = fromJSON(readLines(stdin, warn = FALSE))
      }, error = function(err){
        stop("You didn't pass valid json inputs.")
      })
      
      if(!(is.list(params))){
        stop("Can't parse keys/values from your json inputs.") 
      }
      
      if (!((exists("_blockspring_spec",params)) && isTRUE(params$`_blockspring_spec`))){
        request$params <<- params  
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
          else if ((is.list(params[[var_name]])) &&
                    (exists("filename", params[[var_name]])) &&
                    (!(is.null(params[[var_name]]$filename))) &&
                     (((exists("data", params[[var_name]])) && !(is.null(params[[var_name]]$data))) ||
                     ((exists("url", params[[var_name]])) && !(is.null(params[[var_name]]$url))))
                   ){
            tmp_file_path = tempfile(pattern = "", tmpdir = tempdir(), fileext = paste("-",params[[var_name]][["filename"]], sep=""))
            
            if(exists("data", params[[var_name]])){
              tryCatch({
                cat(base64Decode(params[[var_name]][["data"]]), file = tmp_file_path)
                request$params[[var_name]] <<- tmp_file_path
              },
              error = function(err){
                request$params[[var_name]] <<- params[[var_name]]
              })
            } else {
              download.file(params[[var_name]][["url"]],tmp_file_path, method="curl", quiet=TRUE)
              request$params[[var_name]] <<- tmp_file_path
            }
          }
          else {
            request$params[[var_name]] <<- params[[var_name]]
          }
        }
      }
      
      close(stdin)
    }
  }

  processArgs = function(){
    argv <- commandArgs(asValues=TRUE, trailingOnly=TRUE)
    for (key in names(argv)){
      request$params[[key]] <<- argv[[key]]
    }
  }

  processStdin()
  processArgs()

  block(request, response)
}
