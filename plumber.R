# Packages ----
# For API
library(plumber)
library(rapidoc)
library(logger)
library(glue)
# For model predictions
library(parsnip)
library(ranger)

# Load model ----
model <- readr::read_rds("models/model.rds")

# Path for log files
log_path <- "logs"

# Create log file directory if it doesn't exist
if(!fs::dir_exists(log_path)) fs::dir_create(log_path)

# Send logs both to stdout and the log directory
log_appender(appender_tee(tempfile("plumber_", log_path,".log")))

#* @apiTitle Penguin Predictions

#* Predict penguin species based on input data
#* @parser json
#* @serializer json
#* @post /predict
function(req, res) {
  data <- tryCatch(jsonlite::parse_json(req$postBody, simplifyVector = TRUE), error = function(e) NULL)
  if(is.null(data)) {
    res$status <- 400
    list(error = "No data submitted")
  }
  pred = predict(model, new_data = as.data.frame(req$body), type = "prob")
  
  df_logs = data.frame(
       IP_address_client = req$REMOTE_ADDR, #1. The IP address of the client making the request
       HTTP_header = req$HTTP_USER_AGENT, #2. Entries for all of the HTTP headers sent with this request
       host = req$HTTP_HOST, #3. IP address where the service is hosted
       method = req$REQUEST_METHOD, #4. The method used for this HTTP request
       endpoint = req$PATH_INFO, #5. The path of the incoming HTTP request
       status = res$status) #6. The status of request
  saveRDS(object = df_logs, file = "logs/df_logs.RDS")
  return(list(predicion = pred, df = df_logs))
         
}

#* @plumber
function(pr) {
  pr %>%
    pr_hooks(
      list(
        preroute = function(){tictoc::tic()},
        postroute = function(req, res) {
          end <- tictoc::toc(quiet = TRUE)
          log_info('{req$REMOTE_ADDR} "{req$HTTP_USER_AGENT}" {req$HTTP_HOST} {req$REQUEST_METHOD} {req$PATH_INFO} {res$status} {round(end$toc - end$tic, digits=5)}')
        })
    )
}

# Update UI
#* @plumber
function(pr) {
  pr %>% 
    pr_set_api_spec(yaml::read_yaml("openapi.yaml")) %>%
    pr_set_docs(docs = "rapidoc")
}
