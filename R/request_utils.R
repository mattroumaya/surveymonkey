standard_request_header = function(token) {
  token = get_bearer_token(token)
  httr::add_headers(
    Authorization = token,
    "Content-Type" = "application/json"
  )
}

#' Get request for survey monkey API
#'
#' Will always set the mattroumaya/surveymonkey repo as the user agent
#'
#' @param url character, url to send request to
#' @param query character, components of the url to change for request arguments,
#'  see also \code{?httr::modify_url}, \link[httr]{modify_url}
#' @param config see \code{?httr::config}, \link[httr]{config}, for full details.
#' Additional configuration settings such as http additional headers.
#' @param ... additional argument passed to \code{httr::GET} \link[httr]{GET}.
#' @export
sm_get = function(url, query, config, ...) {
  out = httr::GET(
    url,
    config = config,
    query = query,
    httr::user_agent("http://github.com/mattroumaya/surveymonkey"),
    ...
  )

  httr::stop_for_status(out)

    remaining_request_message(out)
    reset_time_message(out)

  httr::content(out, as = "parsed")
}

remaining_request_message = function(response) {
  if(length(response$headers$`x-ratelimit-app-global-day-remaining`>0)) {
  message(paste0(
    "You have ",
    response$headers$`x-ratelimit-app-global-day-remaining`,
    " requests left today before you hit the limit"
  ))
  } else {
    warning("Could not determine API request limit")
  }
}

reset_time_message = function(response, frequency = 20) {
  if(length(response$headers$`x-ratelimit-app-global-day-remaining`>0)) {
  if (as.numeric(response$headers$`x-ratelimit-app-global-day-remaining`) %% frequency == 0) {
    message(paste0(
      "Your daily request limit will reset in ",
      response$headers$`X-Ratelimit-App-Global-Day-Reset`,
      " seconds"
    ))
  }
  } else {
    warning("Could not determine API request limit")
  }
}
