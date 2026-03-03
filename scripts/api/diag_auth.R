library(httr)
library(jsonlite)

# Diagnostic Script
api_key <- Sys.getenv("NIVUS_API_KEY")
message("--- DIAGNOSTIC START ---")
if (api_key == "") {
    message("ERROR: NIVUS_API_KEY environment variable is COMPLETELY EMPTY.")
    message("This means the GitHub Secret is not being passed correctly or is missing.")
} else {
    message("NIVUS_API_KEY is present.")
    message(paste("Length of key:", nchar(api_key)))
    # Check for common issues like quotes or whitespace
    if (grepl("^\"", api_key) || grepl("\"$", api_key)) message("WARNING: Key contains surrounding quotes!")
    if (grepl("^\\s+", api_key) || grepl("\\s+$", api_key)) message("WARNING: Key contains leading/trailing whitespace!")
}

base_url <- "https://datakiosk-api.nivusweb.com"
endpoint <- "/api/v2/stations"

message(paste("Testing GET", paste0(base_url, endpoint)))

# Try with a common User-Agent just in case
resp <- GET(
    url = paste0(base_url, endpoint),
    add_headers(
        `X-API-Key` = api_key,
        `User-Agent` = "R-httr-SCIU-Project"
    )
)

message(paste("Status Code:", status_code(resp)))
message("Headers received:")
print(headers(resp))

if (status_code(resp) == 401) {
    message("ERROR: 401 Unauthorized. The API key is rejected.")
    message("Response content:")
    print(content(resp, "text"))
} else if (status_code(resp) == 403) {
    message("ERROR: 403 Forbidden. The API key is valid but has no permissions for this endpoint.")
} else if (status_code(resp) == 200) {
    message("SUCCESS! API Key is valid and working.")
} else {
    message(paste("Unknown error. Status:", status_code(resp)))
    print(content(resp, "text"))
}
message("--- DIAGNOSTIC END ---")
