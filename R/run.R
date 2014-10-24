#'
#'
#' This function allows you to run code remotely.
#' @param block A string.
#' @param data A list.
#' @return This function doesn't return anything, it strictly defines a new blockspring function.
#' @examples
#' run("/blockspring_username/block_id", list())


run <- function(block, data) {
  api_key = Sys.getenv("BLOCKSPRING_API_KEY")

  if(api_key == "") {
    stop("BLOCKSPRING_API_KEY environment variable not set")
  }

  blockspring_url = Sys.getenv("BLOCKSPRING_URL") || "https://sender.blockspring.com"

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
