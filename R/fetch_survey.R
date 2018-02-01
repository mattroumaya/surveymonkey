# Right now this function is dumb, there's not much to return except response_count

fetch_survey_details <- function(id){
  if(missing(id)){
    stop("specify an id")
  }

  if(!is.null(oauth_token)){
    u <- 'https://api.surveymonkey.com/'
    token <- paste('bearer', oauth_token)
  }
  else{ stop("Must specify 'oauth_token'") }


  h <- httr::add_headers(Authorization=token,
                         'Content-Type'='application/json')
  p <- list("v3", survey = "surveys", id = id, details = "details")

  out <- httr::GET(u, config = h, path = p)
  message(paste0("you have ", out$headers$`x-ratelimit-app-global-day-remaining`, " requests left today before you hit the limit"))
  httr::stop_for_status(out)
  parsed_content <- httr::content(out, as = 'parsed')
  tibble::tibble(
    title = parsed_content$title,
    id = parsed_content$id,
    nickname = parsed_content$nickname,
    response_count = parsed_content$response_count
  )

}
