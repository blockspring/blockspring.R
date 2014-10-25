#'
#'
#' This function allows you to run code remotely.
#' @param block A string.
#' @param data A list.
#' @param api_key A string.
#' @return This function returns the result of running a blockspring block.


blockspringRun <- function( block = NULL, data = NULL, api_key = NULL ) {
  if(is.null(api_key)){
    api_key = Sys.getenv("BLOCKSPRING_API_KEY") 
  }
  
  if(api_key == "") {
    write("Pass in your blockspring api key to increase your limit.", stderr())
  }

  blockspring_url = Sys.getenv("BLOCKSPRING_URL")
  if (blockspring_url == ""){
    blockspring_url = "https://sender.blockspring.com"
  }

  block_parts = strsplit(block, "/")
  block = block_parts[[1]][length(block_parts[[1]])]

  response = POST(paste( blockspring_url, "/api_v2/blocks/", block, "?api_key=", api_key, sep = ""), body = data)

  results = content(response, as="text")

  output = tryCatch({
    return(fromJSON(results))
  }, error = function(e) {
    return(results)
  })

  return(output)
}