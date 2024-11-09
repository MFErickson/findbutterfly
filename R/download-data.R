# Download data from the Firestore realtime database 
# Creates two CSV files named session.csv and score.csv
#
# A session is a single play of the game, i.e. 30 images
# scores$sessionId joins sessions$sessionId


# Functions to download data from the Firebase cloud database.
# We use the realtime Database, NOT Cloud Firestore.

library(lubridate)
#devtools::install_github("JimMcL/JUtils")
library(JUtils)

# ==== Firebase fetcher ====

API_KEY <- Sys.getenv("API_KEY")
DB_URL <- Sys.getenv("DATABASE_URL")
PROJECT_ID <- Sys.getenv("PROJECT_ID")
PROJECT_DOMAIN <- Sys.getenv("AUTH_DOMAIN")

# Details are stored in the environment so they are not in GitHub
# E.g. I define them in .Renviron, which is in .gitignore
userEmail <- Sys.getenv("USER_EMAIL")
userPassword <- Sys.getenv("USER_PASSWORD")
DB_DOCUMENT <- "butt-scores"

.getFBUrl <- function() {
  url <- paste0(DB_URL, "/", DB_DOCUMENT, ".json")
  
  # Firebase is set up so that only this user is allowed to read the data
  user <- auth(API_KEY, email = userEmail, password = userPassword)
  url <- paste0(url, "?", "auth=", user$idToken)
}

# Copied from the fireData package which no longer installs on my system (3 Aug 2019).
# https://github.com/Kohze/fireData
# 
#' @title The user authentication function:
#' @description fireData::auth checks the validity of a login and returns the temporary JWT user token. FireData_auth can be used to store individual user data in specified directories that are only accessible to that specific user.
#' @param projectAPI The Firebase Project API {string}
#' @param email The user email {string}
#' @param password The user password {string}
#' @return Returns the content of the firebase API request, such as the state of registration, idToken, and validity of the user password.
#' @export
#' @examples
#' \dontrun{
#' auth(projectAPI = "XXX", email = "xxx@example.com", password = "12341234")
#' }
auth <- function(projectAPI, email="prompt", password="prompt"){
  if (password == "prompt" && email == "prompt") {
    email <- readline(prompt = "Email: ")
    password <- readline(prompt = "Password: ")
    print(paste0("Connecting to",  project_api, ":"))
  }
  AuthUrl = paste0("https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key=", projectAPI)
  userData = httr::POST(url = AuthUrl, body = list("email" = email, "password" = password, "returnSecureToken" = "True"), encode = "json")
  return(httr::content(userData))
}

# Queries the Firebase database for any trial date as old as or newer than the specified start time.
# @return List of lists, each inner list is a "row" of data
QueryFirebase <- function(startTime = NULL) {
  
  url <- .getFBUrl()
  
  if (!is.null(startTime)) {
    url <- paste0(url, '&orderBy="created_at"&startAt="', startTime, '"')
  }
  
  # message(url)
  data = httr::GET(url)
  
  content <- jsonlite::fromJSON(httr::content(data,"text"))
  if (length(content) == 0 || is.null(content)) 
    stop(sprintf("No data found in Firebase location %s", DB_DOCUMENT))
  if (!is.null(content$error))
    stop(paste("Unable to query Firebase", content$error))
  
  # Give each element an ID value
  for(id in names(content)) {
    content[[id]]$id <- id
  }
  content
}


#--------------------------------------------------------------#
# Get the data
fbd <- QueryFirebase()
# Get types of data
types <- unique(sapply(fbd, function(row) row$type))
badTypes <- types[!types %in% c("score", "session")]
if (length(badTypes) > 0) {
  stop(sprintf("Unexpected type %s", paste(badTypes, collapse = ", ")))
}

# Load table of scores
scores <- do.call(rbind, Map(as.data.frame, Filter(function(row) row$type == "score", fbd)))
sessions <- do.call(rbind, Map(as.data.frame, Filter(function(row) row$type == "session", fbd)))

#--------------------------------------------------------------#
# Synthesise some columns

# Create a localTime column by converting from UTC to local time
sessions$localTime <- as_datetime(sessions$created_at, tz = Sys.timezone())
scores$localTime <- as_datetime(scores$created_at, tz = Sys.timezone())

# Group backgrounds by type
scores$bgType <- as.factor(sub("^([[:lower:]]+).*", "\\1", scores$backgroundUrl))

# Conspicuousness score is 1 for hit, 0 for escape
scores$conspicuous <- ifelse(scores$score == "hit", 1, 0)
# Record butterfly/background combo
scores$combo <- paste(scores$bgType, scores$butterflyUrl, sep = "-")

# Work out each butterfly/background combination in each session
i <- seq_along(scores$combo)[-1]
scores$comboId <- c(0, cumsum(scores$combo[i] != scores$combo[i - 1]))

#--------------------------------------------------------------#
# Filter out bad data for various reasons

# # Filter out sessions which didn't use the finalised images
sessions <- sessions[sessions$localTime > ymd("2024-10-29"), ]
scores <- scores[scores$sessionId %in% sessions$sessionId, ]

# Check that we excluded all the old background images. Old ones were named "DSC..."
badSessIds <- unique(scores$sessionId[!grepl("^[[:lower:]]", scores$backgroundUrl)])
if (length(badSessIds) > 0) {
  badSess <- sessions[sessions$sessionId %in% badSessIds, ]
  warning(sprintf(" Old sessions included from dates: %s\n", JToSentence(unique(badSess$localTime))))
}

# Limit analysis to butterflies that have been tested at least 12 times because
# we removed some early on. Remove entire bad sessions
bt <- table(scores$butterflyUrl)
badSess <- scores$sessionId[scores$butterflyUrl %in%  names(bt)[bt < 15]]

# Also remove some backgrounds
badSess <- c(badSess, scores$sessionId[scores$bgType == "mix"])

# There are some weird times - presumably people paused then continued
badSess <- c(badSess, scores$sessionId[scores$time > 10500],
             scores$sessionId[scores$time <= 0])

scores <- scores[!scores$sessionId %in% badSess, ]
sessions <- sessions[!sessions$sessionId %in% badSess, ]

#--------------------------------------------------------------#
# Write out the clean data

# Output a CSV for each type of data
write.csv(scores, "score.csv", row.names = FALSE)
write.csv(sessions, "session.csv", row.names = FALSE)

cat(sprintf("Downloaded %d sessions from %d users with %d scores: %s escapes, %d misses and %d hits\n",
            nrow(sessions), length(unique(sessions$userId)), nrow(scores),
            sum(scores$score == "escape"), sum(scores$score == "miss"), sum(scores$score == "hit")))
