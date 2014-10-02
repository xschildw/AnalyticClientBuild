###############################################################################
# updateLatestVersion.R
# Author: brucehoff
# reads the Version info (latest version and black list) from S3, updates the latestVersion, and writes it back
#
###############################################################################

library("RJSONIO")
library("RCurl") # includes base64()
library("digest") # includes hmac()

getDepotRepo<-function() {
    repo<-contrib.url("http://depot.sagebase.org/CRAN/prod/3.1")
  # the above should return something like:
  # http://depot.sagebase.org/CRAN/prod/3.1/src/contrib
  repo <-gsub("2.15", "3.1", repo, fixed=T)
  repo <-gsub("3.0", "3.1", repo, fixed=T)
  repo
}

getLatestVersion<-function() {
  repo<-getDepotRepo()
  ap <- available.packages(repo)
  if (any(dimnames(ap)[[1]]=="synapseClient")) {
    scVersion <- ap['synapseClient','Version']
  } else {
    stop(sprintf("Unable to find current R client version in this repo %s", repo))
  } 
  scVersion
}

getArtifactURL<-function() {
  repo<-getDepotRepo()
  scVersion<-getLatestVersion()
  # the artifact's URL will look something like:
  # http://depot.sagebase.org/CRAN/prod/3.1/src/contrib/synapseClient_1.4-4.tar.gz
  fileName<-sprintf("synapseClient_%s.tar.gz", scVersion)
  fileURL<-sprintf("%s/%s", repo, fileName)
  fileURL
}

# get the version of this package (assumed to be the latest) and put it in the public 'versions' resource
# Note:  'versionsEndpoint' is simply the bucket name, e.g. "versions.synapse.sagebase.org"
updateLatestVersion<-function(versionsEndpoint, awsAccessKeyId, secretAccessKey) {
  targetFileName<-"synapseRClient"
  uri <- sprintf("%s/%s", versionsEndpoint, targetFileName)
  fileContent<-fromJSON(getURL(uri))
  fileContent$latestVersion<-getLatestVersion()
  # now upload to S3
  bucket <- versionsEndpoint
  uploadToS3File(toJSON(fileContent), "application/json", bucket, targetFileName, awsAccessKeyId, secretAccessKey)
  message("Successfully completed updating version info file.")
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

generateHtmlDocs<-function(awsAccessKeyId, secretAccessKey) {
  fileURL<-getArtifactURL()
  # now download
  localFilePath<-file.path(tempdir(), fileName)
  download.file(fileURL, localFilePath)
  # now unzip into tempdir
  result<-system(sprintf("cd %s; tar xzf %s", tempdir(), localFilePath))
  if (result!=0) stop(sprintf("Could not gunzip %s", localFilePath))
  cat("Successfully unzipped tar.gz file.\n")
  # make a subdir 'staticdocs'
  docsdir<-file.path(tempdir(), "synapseClient/staticdocs")
  if (!dir.create(docsdir)) stop(sprintf("could not create %s", docsdir))
  cat("Successfully created staticdocs sub-directory.\n")
  library(staticdocs)
  cat("Loaded staticdocs package.\n")
  build_site(pkg=file.path(tempdir(), "synapseClient"),examples=FALSE,launch=FALSE)
  cat("Created the html documentation.\n")
  # now upload tempdir()/synapseClient/inst/web to S3 (is the bucket called "http://r-docs.synapse.org"??)
  uploadFolderToS3(file.path(tempdir(), "synapseClient/inst/web"), NULL, "r-docs.synapse.org", "AKIAIYHLMFZOUMW2HXUA", "JmFISdOGJXIff6ggf6KtQEVoNLEcdEaolI5fHvO2", createMimeTypeMap())
}


