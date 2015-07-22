library (httpuv)
library (methods)
#----------------------------------------------------------------------------------------------------
browserVizDemoBrowserFile <- system.file(package="BrowserVizDemo2", "scripts", "browserVizDemo2.html")
#----------------------------------------------------------------------------------------------------
.BrowserVizDemo2 <- setClass ("BrowserVizDemo2Class", 
                            representation = representation (),
                            contains = "BrowserVizClass",
                            prototype = prototype (uri="http://localhost", 9000)
                            )

#----------------------------------------------------------------------------------------------------
setGeneric ('plot',  signature='obj', function (obj, x, y) standardGeneric ('plot'))
setGeneric ('getSelection',  signature='obj', function (obj) standardGeneric ('getSelection'))
setGeneric ('selectPoints',  signature='obj', function (obj, pointNames) standardGeneric ('selectPoints'))
#----------------------------------------------------------------------------------------------------
setupMessageHandlers <- function()
{
   addRMessageHandler("handleResponse", "handleResponse")

} # setupMessageHandlers
#----------------------------------------------------------------------------------------------------
# constructor
BrowserVizDemo2 = function(portRange, host="localhost", title="BrowserVizDemo2", quiet=TRUE)
{
  .BrowserVizDemo2(BrowserViz(portRange, host, title, quiet, browserFile=browserVizDemoBrowserFile))

} # BrowserVizDemo2: constructor
#----------------------------------------------------------------------------------------------------
setMethod('plot', 'BrowserVizDemo2Class',

  function (obj, x, y) {
     xMin <- min(x)
     xMax <- max(x)
     yMin <- min(y)
     yMax <- max(y)
     send(obj, list(cmd="plotxy", callback="handleResponse", status="request",
                    payload=list(x=x, y=y, xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("plot waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#----------------------------------------------------------------------------------------------------
setMethod('getSelection', 'BrowserVizDemo2Class',

  function (obj) {
     send(obj, list(cmd="getSelection", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("getSelection waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#----------------------------------------------------------------------------------------------------
setMethod('selectPoints', 'BrowserVizDemo2Class',

  function (obj, pointNames) {
     send(obj, list(cmd="selectPoints", callback="handleResponse", status="request", payload=pointNames))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("getSelection waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#----------------------------------------------------------------------------------------------------
