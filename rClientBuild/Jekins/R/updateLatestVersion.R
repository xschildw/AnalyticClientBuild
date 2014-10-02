###############################################################################
# updateLatestVersion.R
# Author: brucehoff
# reads the Version info (latest version and black list) from S3, updates the latestVersion, and writes it back
#
###############################################################################

library("RJSONIO")
library("RCurl") # includes base64()
library("digest") # includes hmac()

# get the version of this package (assumed to be the latest) and put it in the public 'versions' resource
# Note:  'versionsEndpoint' is simply the bucket name, e.g. "versions.synapse.sagebase.org"
updateLatestVersion<-function(versionsEndpoint, awsAccessKeyId, secretAccessKey) {
  targetFileName<-"synapseRClient"
  uri <- sprintf("%s/%s", versionsEndpoint, targetFileName)
  fileContent<-fromJSON(getURL(uri))
  fileContent$latestVersion<-packageDescription("synapseClient", field="Version")
  releaseNotes<-packageDescription("synapseClient", field="ReleaseNotes")
  if (is.null(releaseNotes) | is.na(releaseNotes)) {
    fileContent$releaseNotes<-""
  } else {
    fileContent$releaseNotes<-releaseNotes
  }
  # now upload to S3
  bucket <- versionsEndpoint
  uploadToS3File(toJSON(fileContent), "application/json", bucket, targetFileName, awsAccessKeyId, secretAccessKey)
}

# uploads file content to S3
uploadToS3File<-function(content, contentType, bucket, targetFileName, awsAccessKeyId, secretAccessKey) {
  #UTC timestamp "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'" a bit in the future (e.g. an hour from now)
  futureOffsetSeconds <- 100
  expirationTimestamp <- sprintf("%sZ", format(Sys.time()+futureOffsetSeconds,"%Y-%m-%dT%H:%M:%OS2", tz="UTC"))
  # this is a hack. drop leap seconds
  expirationTimestamp <- gsub(":6[01][\\.]", ":59.", expirationTimestamp)
  policyRaw <- sprintf("{\"expiration\": \"%s\",\"conditions\": [{\"bucket\": \"%s\" },{\"acl\": \"public-read\" },[\"eq\",\"$key\", \"%s\"],[\"content-length-range\", 0, \"10000\"],[\"starts-with\", \"$Content-Type\", \"%s\"],]}",
    expirationTimestamp, bucket, targetFileName, contentType)
  policyBase64<-base64(policyRaw, encode=TRUE)[1]
  signatureRaw<-hmac(secretAccessKey, policyBase64, algo="sha1", raw=TRUE)
  signatureBase64 <- base64(signatureRaw, encode=TRUE)
  
  uri <-sprintf("http://s3.amazonaws.com/%s", bucket)
  postfields <- list(
    key=targetFileName,
    acl="public-read",
    AWSAccessKeyId=awsAccessKeyId,
    Policy=policyBase64,
    Signature=signatureBase64,
    "Content-Type"=contentType,
    file=content)
  
  # if warnings are not suppressed prints "Found possible curl options in form parameters: file"
  oldWarn<-options("warn")
  options(warn=-1)
  postForm(uri, .params=postfields)
  options(oldWarn)
}

# recursively upload the contents of a folder
# in the initial call 'root' is the root directory of interest, 
# 'relativePath' is NULL, and mimTypeMap=createMimeTypeMap()
uploadFolderToS3<-function(root, relativePath, bucket, awsAccessKeyId, secretAccessKey, mimeTypeMap) {
  if (is.null(relativePath)) {
    path<-root
  } else {
    path<-file.path(root, relativePath)
  }
  if (!file.exists(path)) stop(sprintf("%s does not exist.", path))
  if (file.info(path)$isdir) {
    dirContent<-list.files(path)
    for (f in dirContent) {
      if (is.null(relativePath)) {
        subdir<-f
      } else {
        subdir<-file.path(relativePath, f)
      }
      uploadFolderToS3(root, subdir, bucket, awsAccessKeyId, secretAccessKey, mimeTypeMap)
    }
  } else {
    # it's a file, so upload the content
    connection<-file(path)
    fileContent<-paste(readLines(connection), collapse="\n")
    close(connection)
    contentType<-"text/plain"
    targetFileName<-relativePath
    uploadToS3File(content, contentType, bucket, awsAccessKeyId, secretAccessKey, mimeTypeMap) 
  }
}

#
# getMimeTypeForFile
# given a file name, determine the MIME type using the publicly available Apache mapping
#

# get the file's extension and then look up the MIME type, if there is no extension, apply a default
getMimeTypeForFile<-function(fileName, mimeTypeMap) {
  defaultMimeType<-"application/octet-stream"
  extension<-getExtension(fileName)
  if (nchar(extension)==0) return(defaultMimeType) # default mime type
  mimeType<-mimeTypeMap[[extension]]
  if (is.null(mimeType)) mimeType<-defaultMimeType
  return(mimeType)
}

# get the substring following the LAST "." in fname, or "" if there is no "."
# this code takes into account that there may be multiple "."s in fname
getExtension<-function(fname) {
  suffix<-fname
  dot<-0
  while (dot>=0) {
    lastDot<-dot
    suffix<-substr(suffix, dot+1, nchar(suffix))
    dot<-regexpr(".", suffix, fixed=T)[[1]]
    if (dot==0) stop("Illegal state, dot==0")
  }
  if (lastDot==0) {
    return("")
  } else {
    return(tolower(suffix))
  }
}


# read the Apache MIME type list from the 'net and build a map in memory
createMimeTypeMap<-function() {
  apacheMimeTypeURL<-"http://svn.apache.org/repos/asf/httpd/httpd/branches/2.0.x/docs/conf/mime.types";
  parsedResult<-strsplit(getURLContent(apacheMimeTypeURL), "\n", fixed=TRUE)[[1]]
  ans<-list()
  for (line in parsedResult) {
    # skip blank lines and comments
    if (nchar(line)>0 && substr(line,1,1)!="#") {
      tokens<-strsplit(line, "[ \t]")[[1]]
      if (length(tokens)>1) {
       mimeType<-tokens[1]
       for (token in tokens[2:length(tokens)]) if (nchar(token)>0) ans[[token]]<-mimeType
      }
    }
  }
  # The online list does not contain a mime type for .R files
  ans[['r']] <- 'text/x-r'
  return(ans)
}


