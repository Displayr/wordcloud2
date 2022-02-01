##' Create wordcloud by wordcloud2.js
##'
##' @description
##' Function for Creating wordcloud by wordcloud2.js
##'
##' @usage
##' wordcloud2(data, size = 1, minSize = 0, gridSize =  0,
##'     fontFamily = 'Segoe UI', fontWeight = 'bold',
##'     color = 'random-dark', backgroundColor = "white",
##'     minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
##'     rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
##'     widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
##'
##' @param data   A data frame including word and freq in each column
##' @param size   Font size, default is 1. The larger size means the bigger word.
##' @param minSize  minimum font size to draw on the canvas.
##' @param gridSize  Size of the grid in pixels for marking the availability of the canvas
##' the larger the grid size, the bigger the gap between words.
##' @param fontFamily Font to use.
##' @param fontWeight Font weight to use, e.g. normal, bold or 600
##' @param color  color of the text, keyword 'random-dark' and 'random-light' can be used.
##' color vector is also supported in this param
##' @param backgroundColor Color of the background.
##' @param minRotation If the word should rotate, the minimum rotation
##' (in rad) the text should rotate.
##' @param maxRotation If the word should rotate, the maximum rotation (in rad) the text should rotate.
##' Set the two value equal to keep all text in one angle.
##' @param shuffle  Shuffle the points to draw so the result will be different each time for the same list and settings.
##' @param rotateRatio Probability for the word to rotate. Set the number to 1 to always rotate.
##' @param shape The shape of the "cloud" to draw. Can be a keyword present. Available presents are 'circle'
##'  (default), 'cardioid' (apple or heart shape curve, the most known polar equation),
##'  'diamond' (alias of square), 'triangle-forward', 'triangle', 'pentagon', and 'star'.
##' @param ellipticity degree of "flatness" of the shape wordcloud2.js should draw.
##' @param figPath The path to a figure used as a mask.
##' @param widgetsize size of the widgets
##' @param hoverFunction Callback to call when the cursor enters or leaves a region occupied
##' by a word. A string of java script function.
##'
##' @examples
##'library(wordcloud2)
##'# Global variables can go here
##'
##'
##'
##' wordcloud2(demoFreq)
##' wordcloud2(demoFreq, size = 2)
##'
##' wordcloud2(demoFreq, size = 1,shape = 'pentagon')
##' wordcloud2(demoFreq, size = 1,shape = 'star')
##'
##' wordcloud2(demoFreq, size = 2,
##'            color = "random-light", backgroundColor = "grey")
##'
##' wordcloud2(demoFreq, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
##' wordcloud2(demoFreq, size = 2, minRotation = -pi/6, maxRotation = -pi/6,
##'   rotateRatio = 1)
##' wordcloud2(demoFreq, size = 2, minRotation = -pi/6, maxRotation = pi/6,
##'   rotateRatio = 0.9)
##'
##' wordcloud2(demoFreqC, size = 2,
##'            color = "random-light", backgroundColor = "grey")
##' wordcloud2(demoFreqC, size = 2, minRotation = -pi/6, maxRotation = -pi/6,
##'   rotateRatio = 1)
##'
##' # Color Vector
##'
##' colorVec = rep(c('red', 'skyblue'), length.out=nrow(demoFreq))
##' wordcloud2(demoFreq, color = colorVec, fontWeight = "bold")
##'
##' wordcloud2(demoFreq,
##'   color = ifelse(demoFreq[, 2] > 20, 'red', 'skyblue'))



#' @import htmlwidgets
#' @export
# data = data.frame(name=c("New","Old"),
#                   freq=c(100,30))
wordcloud2 <- function(data,
                       size = 1,
                       minSize =  0,
                       gridSize =  0,
                       fontFamily = 'Segoe UI',
                       fontWeight = 'bold',
                       color =  'random-dark',
                       backgroundColor = "white",
                       minRotation = -pi/4,
                       maxRotation = pi/4,
                       shuffle = TRUE,
                       rotateRatio = 0.4,
                       shape = 'circle',
                       ellipticity = 0.65,
                       widgetsize = NULL,
                       figPath = NULL,
                       hoverFunction = NULL
                       ) {
  if("table" %in% class(data)){
    dataOut = data.frame(name = names(data),
                         freq = as.vector(data))
  }else{
    data = as.data.frame(data)
    dataOut = data[,1:2]
    names(dataOut) = c("name", "freq")
  }



  # if(!is.null(figPath)){
  #   if(!file.exists(figPath)){
  #   stop("cannot find fig in the figPath")
  #   }
  #   spPath = strsplit(figPath, "\\.")[[1]]
  #   len = length(spPath)
  #   figClass = spPath[len]
  #
  #   if(!figClass %in% c("jpeg","jpg","png","bmp","gif")){
  #     stop("file should be a jpeg, jpg, png, bmp or gif file!")
  #   }
  #
  #   base64 = base64enc::base64encode(figPath)
  #   base64 = paste0("data:image/",figClass ,";base64,",base64)
  #
  # }else{
  #   base64 = NULL
  # }

  base64 = "data:image/png;base64,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"

  # create a list that contains the settings

  weightFactor = size * 180 / max(dataOut$freq)

  settings <- list(
    word = dataOut$name,
    freq = dataOut$freq,
    fontFamily = fontFamily,
    fontWeight = fontWeight,
    color =  color,
    minSize =  minSize,
    weightFactor = weightFactor,
    backgroundColor = backgroundColor,
    gridSize =  gridSize,
    minRotation = minRotation,
    maxRotation = maxRotation,
    shuffle = shuffle,
    rotateRatio = rotateRatio,
    shape = shape,
    ellipticity = ellipticity,
    figBase64 = base64,
    hover = htmlwidgets::JS(hoverFunction)
  )


  htmlwidgets::createWidget("wordcloud2", settings,
                            width = widgetsize[1],
                            height = widgetsize[2],
                            sizingPolicy = htmlwidgets::sizingPolicy(
                              viewer.padding = 0,
                              # viewer.suppress = T,
                              browser.padding = 0,
                              browser.fill = TRUE
                            ))
}




