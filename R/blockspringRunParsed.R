#'
#'
#' This function allows you to run code remotely.
#' @param block A string.
#' @param data A list.
#' @param options A list.
#' @return This function returns the result of running a blockspring block.


blockspringRunParsed <- function( block, data = setNames(fromJSON('{}'), character(0)), options = list()) {
  if (typeof(options) == "character"){
    options = list(
      api_key = options,
      cache = FALSE,
      expiry = NULL
    )
  }

  if (!("api_key" %in% names(options))){
    options$api_key = NULL
  }

  if(!(is.list(data))){
    stop("your data needs to be a list!")
  }

  data = toJSON(data)

  if(is.null(options$api_key)){
    api_key = Sys.getenv("BLOCKSPRING_API_KEY")
  } else {
    api_key = options$api_key
  }

  if("cache" %in% names(options)){
    cache = options$cache
  } else {
    cache = FALSE
  }

  if("expiry" %in% names(options)){
    expiry = options$expiry
  } else {
    expiry = NULL
  }

  blockspring_url = Sys.getenv("BLOCKSPRING_URL")
  if (blockspring_url == ""){
    blockspring_url = "https://sender.blockspring.com"
  }

  block = tail(strsplit(block,"/")[[1]], n=1)

  response = POST(
    paste(
      blockspring_url,
      "/api_v2/blocks/",
      block,
      "?api_key=",
      api_key,
      "&cache=",
      cache,
      "&expiry=",
      expiry,
      sep = ""
    ),
    accept_json(),
    add_headers("Content-Type" = "application/json"),
    body = data
  )

  results = content(response, as="text")

  tryCatch({
    blockspring_parsed_results = fromJSON(results)
    if (is.list(blockspring_parsed_results)) {
      if (!is.null(names(blockspring_parsed_results))) {
        blockspring_parsed_results$`_headers` = headers(response)
      } else {
        blockspring_non_list_json = TRUE
      }
    }
  }, error = function(e) {})
  if (exists("blockspring_parsed_results")){
    if (exists("blockspring_non_list_json")) {
      return(blockspring_parsed_results)
    } else {
      return(blockspring::blockspringParse(blockspring_parsed_results, TRUE))
    }
  } else {
    return(results)
  }
}