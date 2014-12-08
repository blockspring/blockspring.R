#'
#'
#' This function allows you to run code remotely.
#' @param block A string.
#' @param data A list.
#' @param api_key A string.
#' @return This function returns the result of running a blockspring block.


blockspringRunParsed <- function( block, data = setNames(fromJSON('{}'), character(0)), api_key = NULL ) {
  if(!(is.list(data))){
    stop("your data needs to be a list!")
  }

  data = toJSON(data)

  if(is.null(api_key)){
    api_key = Sys.getenv("BLOCKSPRING_API_KEY")
  }

  blockspring_url = Sys.getenv("BLOCKSPRING_URL")
  if (blockspring_url == ""){
    blockspring_url = "https://sender.blockspring.com"
  }

  block = tail(strsplit(block,"/")[[1]], n=1)

  response = POST(paste( blockspring_url, "/api_v2/blocks/", block, "?api_key=", api_key, sep = ""), accept_json(), add_headers("Content-Type" = "application/json"), body = data)

  results = content(response, as="text")

  tryCatch({
    parsed_results = fromJSON(results)
    if ((is.list(parsed_results)) && !is.null(names(parsed_results))) {
      parsed_results$`_headers` = headers(response)
    }
  }, error = function(e) {})
  if (exists("parsed_results")){
    return(blockspring::blockspringParse(parsed_results, TRUE))
  } else {
    return(results)
  }
}