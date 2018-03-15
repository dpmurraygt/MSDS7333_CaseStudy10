# R make file to induct email raw data and cleanse for analysis

spamPath <- "./Data"

dirNames <- list.files(path = spamPath, full.names = TRUE)

fileNames <- list.files(dirNames[1], full.names = TRUE)

# FUNC to split into header and body based on first blank line
splitMessage <- function(msg) {
  splitPoint = match("", msg) # find first blank line
  header = msg[1:(splitPoint-1)] # get header, will use this for attachments
  body = msg[ -(1:splitPoint) ] # get body
  return(list(header = header, body = body))
}


# process the header for feature engineering purposes
processHeader = function(header)
{
  # replace k:v for reading into read.dcf
  header[1] = sub("^From","Top-From:", header[1])
  
  # get k:v pairs, return in list
  headerMat = read.dcf(textConnection(header), all = TRUE)
  headerVec = unlist(headerMat)
  
  dupKeys = sapply(headerMat, function(x) length(unlist(x)))
  names(headerVec) = rep(colnames(headerMat), dupKeys)
  
  return(headerVec)
}

# get boundary id from email headers
getBoundary<- function(header) {
  boundaryIdx = grep("boundary=", header) # index boundary
  boundary = gsub('"', "", header[boundaryIdx]) # replace quotes
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary) # regexrepl with first group match
}

# pass in body and associated content type list
processAttach = function(body, contentType){
  # get length of body
  n = length(body)
  # get boundary given contentType string already extracted
  boundary = getBoundary(contentType)
  
  # where are starting boundary strings?
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  # where is ending boundary string?
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  # if no ending string use length of body as ending boundary loc
  if (length(eStringLoc) == 0) eStringLoc = n
  
  # if beginning boundaries <= 1 then no attachments
  # take last line of message as last line of body
  if (length(bStringLocs) <= 1) {
    attachLocs = NULL
    msgLastLine = n
    # tiny little conditional
    if (length(bStringLocs) == 0) bStringLocs = 0
  } else {
    # else we have attachments, so establish locations
    # for all attachments - this just uses beginning and end
    # will get all attachment text
    attachLocs = c(bStringLocs[ -1 ],  eStringLoc)
    # identify message body last line
    msgLastLine = bStringLocs[2] - 1
  }
  
  # parse message body
  msg = body[ (bStringLocs[1] + 1) : msgLastLine] 
  if ( eStringLoc < n )
    # concatenate body and last lines
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  # if we have attachments, parse them and get their types
  if ( !is.null(attachLocs) ) {
    # get length of each attachment
    attachLens = diff(attachLocs, lag = 1) 
    # pass beginning and ending positions to multivariate apply
    attachTypes = mapply(function(begL, endL) {
      # select the sections of body identified by attachLens
      # and find content type
      CTloc = grep("^[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      # if no content type, no MIMEType
      if ( length(CTloc) == 0 ) {
        MIMEType = NA
      } else {
        CTval = body[ begL + CTloc[1] ]
        CTval = gsub('"', "", CTval )
        # get MIMEType
        MIMEType = sub(" *[Cc]ontent-[Tt]ype: *([^;]*);?.*", "\\1", CTval)   
      }
      return(MIMEType)
    }, 
    # pass in attachment locations to mapply
    attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  # else if no attachments retrurn a list of the message body and no dataframe
  if (is.null(attachLocs)) return(list(body = msg, attachDF = NULL) )
  # else return a list of the body, and a dataframe detailing the lengths and types of attachments
  return(list(body = msg, 
              attachDF = data.frame(aLen = attachLens, 
                                    aType = unlist(attachTypes),
                                    stringsAsFactors = FALSE)))                                
}                       


readEmail = function(dirName) {
  # retrieve the names of files in directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  # read all files in the directory
  lapply(fileNames, readLines, encoding = "latin1")
}

processAllEmail = function(dirName, isSpam = FALSE)
{
  # read all files in the directory
  messages = readEmail(dirName)
  fileNames = names(messages)
  n = length(messages)
  
  # split header from body
  eSplit = lapply(messages, splitMessage)
  rm(messages)
  
  # process header as named character vector
  headerList = lapply(eSplit, 
                      function(msg) processHeader(msg$header))
  
  # extract content-type key from each message
  contentTypes = sapply(headerList, 
                        function(header) header["Content-Type"])
  
  # extract the body from each message
  bodyList = lapply(eSplit, function(msg) msg$body)
  rm(eSplit)
  
  # determine which email have attachments, index
  hasAttach = grep("^ *multi", tolower(contentTypes))
  
  # get summary stats for attachments and the shorter body
  # index by those emails that have attachments
  attList = mapply(processAttach, # apply process Attach multivariate
                   bodyList[hasAttach],  # over bodies that have attachments
                   contentTypes[hasAttach],  # and their content types
                   SIMPLIFY = FALSE)
  
  # create a placeholder vector to store each df of attachments for
  # every email - if no attachment, index pos == NULL
  attachInfo = vector("list", length = n )
  attachInfo[ hasAttach ] = lapply(attList, 
                                   function(attEl) attEl$attachDF)
  
  # replace body listings with atachments with body from
  # process attach considering last lines
  bodyList[hasAttach] = lapply(attList, function(attEl) 
    attEl$body)
  
  # prepare return structure as a list of lists
  emailList = mapply(function(header, body, attach, isSpam) {
    # return a list of lists for each message we can index into
    list(isSpam = isSpam, header = header, 
         body = body, attach = attach)
  },
  headerList, bodyList, attachInfo, 
  rep(isSpam, n), SIMPLIFY = FALSE )
  names(emailList) = fileNames
  
  invisible(emailList)
}

# this results in a k:v pair for the header (processHeader)
# a list of lines of the body (splitMessage)
# a df of attachment types and attachment length (processAttach)
emailStruct <- mapply(processAllEmail, dirNames, isSpam = rep(c(FALSE,TRUE), 3:2))
emailStruct <- unlist(emailStruct, recursive = FALSE)