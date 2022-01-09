standard_request_header = function(token) {
  token = get_bearer_token(token)
  httr::add_headers(
    Authorization = token,
    "Content-Type" = "application/json"
  )
}