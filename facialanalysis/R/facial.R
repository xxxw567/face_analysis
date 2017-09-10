#' Post image to face++ and return face landmark
#' @param img.path the path of image
#' @param apikey api key
#' @param seqkey secrit key
#' @return data.frame
#' @keywords basic
#' @author Xia Yiwei
#' @export
#' @examples
#'

faceplusplus<-function(img.path,apikey="i6aAp-TZNec1-HjPlzJS1oAaEcdRUHxi",seqkey= "dwobv2uNCA0MvUNTNwIiEmqQEn8jGGLR") {
  library("httr")
  library("imager")
  a<-POST("https://api-us.faceplusplus.com/facepp/v3/detect",
          body=list(
            "api_key" =apikey ,
            "api_secret" =seqkey,
            "image_file" =  upload_file(img.path),
            "return_landmark" = "1",
            "return_attributes" = "gender,age,smiling,headpose,facequality,blur,eyestatus,emotion,ethnicity,beauty,mouthstatus,eyegaze"
          ),
          verbose()
  )
  output<-httr::content(a)
  df=unlist(output$faces, use.names=TRUE)

  data=data.frame(
    type=strsplit(names(df), ".", fixed = TRUE) [[1]][1] ,
    pos=strsplit(names(df), ".", fixed = TRUE) [[1]][2] ,
    coord=strsplit(names(df), ".", fixed = TRUE) [[1]][3] ,
    value=df
  )

  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)

  for (i in 1:length(df)) {
    data[i,"type"]=strsplit(names(df), ".", fixed = TRUE)[[i]][1]
    data[i,"pos"]=strsplit(names(df), ".", fixed = TRUE)[[i]][2]
    data[i,"coord"]=strsplit(names(df), ".", fixed = TRUE)[[i]][3]
  }

  library(dplyr)
  fin<-data %>%
    filter(coord=="x"|coord=="y") %>%
    spread(coord,value)
  return(fin)
}

distance<-function(a,b,x,y,type="abs") {
  if (type=="abs") {
    xdif<-a-x
    ydif<-b-y
    return( sqrt(xdif*xdif+ydif*ydif) )
  }
}

#' find the distance between two points
#' @param a x of first point
#' @param b y of first point
#' @param x x of first point
#' @param y y of first point
#' @return scalar
#' @keywords basic
#' @author Xia Yiwei
#' @export
#' @examples
#'

distance<-function(a,b,x,y,type="abs") {
  if (type=="abs") {
    xdif<-a-x
    ydif<-b-y
    return( sqrt(xdif*xdif+ydif*ydif) )
  }
}

#' plot the dots the image
#' @param data dataset contine "x" as x, "y" as y
#' @param path path of image
#' @return plot
#' @keywords basic
#' @author Xia Yiwei
#' @export
#' @examples
#'

faceplot<-function(data,path,pch=19, col="black") {
  library("imager")
  plot(load.image("path"))
  points(data$x,data$y, pch=pch, col=col)
}

#' plot the dots the image
#' @param data dataset contine "x" as x, "y" as y
#' @param path path of image
#' @return plot
#' @keywords basic
#' @author Xia Yiwei
#' @export
#' @examples
#'

faceplot<-function(data,pch=19, col="black") {
  library("imager")
  plot(load.image("path"))
  points(data$x,data$y, pch=pch, col=col)
}





# library(devtools)
# library(roxygen2)
# library(testthat)
# site<-"c:/Users/xiayi_000/Documents/GitHub/face_analysis/facialanalysis"
# roxygenize(site)
# load_all(site)
# test(site)
# document(site)
# check(site)
