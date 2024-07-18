# Conversion Tool


#-----------------------------------------------------------------------

# Conversion can convert multiple measurements of length, input a numeric, the
# old and the new unit.
# Per default, values will be converted to meters.

#' Convert multiple measurements of length
#'
#' @description
#' Possible units: "twip", "thou", "inch", "hand", "foot", "yard", "chain", "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer" or "twip", "th", "in", "hh", "ft", "yd", "ch", "fur", "mi", "lea", "mm", "cm", "m", "km"
#'
#' @param value Value to convert
#' @param oldunit Unit of the value to convert
#' @param newunit Unit of return value, default is meters
#' @return Converted value
#' @export
#' @examples
#' conversion(5,"yd","mi")
#' conversion(10,"yard")
conversion <- function(value, oldunit, newunit="meter") {
  unitnames <- c("twip", "thou", "inch", "hand", "foot", "yard", "chain",
                 "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer")
  unitabb <- c("twip", "th", "in", "hh", "ft", "yd", "ch",
               "fur", "mi", "lea", "mm", "cm", "m", "km")
  conversion <- c(0.0000176389, 0.0000254, 0.0254, 0.1016, 0.3048, 0.9144, 20.1168,
                  201.168,1609.344,4828.032,0.001,0.01,1,1000)
  df <- data.frame(unitnames, unitabb, conversion)
  if (oldunit %in% df$unitabb) {
    oldunit <- df$unitnames[which(unitabb == oldunit)]
  }
  if (newunit %in% df$unitabb) {
    newunit <- df$unitnames[which(unitabb == newunit)]
  }
  if (oldunit %in% df$unitnames && newunit %in% df$unitnames) {
    meter = value * df$conversion[which(unitnames == oldunit)]
    new = meter / df$conversion[which(unitnames == newunit)]
    return(new)
  } else {
    return("Unknown Unit")
  }
}

#-----------------------------------------------------------------------

# Arrayconversion can convert multiple measurements of length, input an array or vector of numeric,
# the old and the new unit.
# Per default, values will be converted to meters.

#' Convert an array or vector of measurements of length
#'
#' @description
#' Possible units: "twip", "thou", "inch", "hand", "foot", "yard", "chain", "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer" or "twip", "th", "in", "hh", "ft", "yd", "ch", "fur", "mi", "lea", "mm", "cm", "m", "km"
#'
#' @param value Array/Vector of values to convert
#' @param oldunit Unit of the value to convert ("twip", "thou", "inch", "hand", "foot", "yard", "chain", "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer" or "twip", "th", "in", "hh", "ft", "yd", "ch", "fur", "mi", "lea", "mm", "cm", "m", "km")
#' @param newunit Unit of return value, default is meters
#' @return Converted array/vector of values
#' @export
#' @examples
#' arrayconversion(c(5,10,15),"yd","mi")
#' arrayconversion(c(5,10,15),"yard")
arrayconversion <- function(value, oldunit, newunit="meter"){
  new <- c()
  for (x in value){
    new <- c(new, conversion(x,oldunit, newunit))
  }
  if(is.null(dim(value))==TRUE) {
    return(new)
  } else {
    return(array(new,dim=dim(value)))
  }
}

#-----------------------------------------------------------------------

# Strconversion can convert multiple measurements of length, input a
# string with the Value and unit and choose if the output should be a numeric
# or a string with value and unit as well. Plural -s will be ignored.
# Per default, values will be converted to meters.

#' Convert a string with measurements of length
#'
#' @description
#' Possible units: "twip", "thou", "inch", "hand", "foot", "yard", "chain", "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer" or "twip", "th", "in", "hh", "ft", "yd", "ch", "fur", "mi", "lea", "mm", "cm", "m", "km".
#' Plural "-s" and spaces in the input will be ignored
#'
#' @param old String of values with unit at the end
#' @param newunit Unit of return value, default is meters
#' @param returnstring TRUE:return is string with unit name at the end, FALSE: return is numeric data
#' @return Converted value
#' @export
#' @examples
#' strconversion("5yd","mi")
#' strconversion("10miles","yard",TRUE)
strconversion <- function(old, newunit="meter", returnstring=FALSE) {
  unitnames <- c("twip", "thou", "inch", "hand", "foot", "yard", "chain",
                 "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer")
  unitabb <- c("twip", "th", "in", "hh", "ft", "yd", "ch",
               "fur", "mi", "lea", "mm", "cm", "m", "km")
  conversion <- c(0.0000176389, 0.0000254, 0.0254, 0.1016, 0.3048, 0.9144, 20.1168,
                  201.168,1609.344,4828.032,0.001,0.01,1,1000)
  df <- data.frame(unitnames, unitabb, conversion)

  valuestr <- stringr::str_extract(old,"\\d+\\.?\\d*")
  value <- as.numeric(stringr::str_extract(old,"\\d+\\.?\\d*"))
  oldunit <- gsub("\\s","",gsub("s$","",stringr::str_replace(old, valuestr, "")))

  if (oldunit %in% df$unitabb) {
    oldunit <- df$unitnames[which(unitabb == oldunit)]
  }
  if (newunit %in% df$unitabb) {
    newunit <- df$unitnames[which(unitabb == newunit)]
  }

  if (oldunit %in% df$unitnames && newunit %in% df$unitnames) {
    meter = value * df$conversion[which(unitnames == oldunit)]
    new = meter / df$conversion[which(unitnames == newunit)]
    if (returnstring==TRUE) {
      return(paste(new,newunit))
    } else {
      return(new)
    }
  } else {
    return("Unknown Unit")
  }
}

#-----------------------------------------------------------------------

# Arraystrconversion can convert multiple measurements of length, input an
# Array or vector of strings with the Value and unit and choose if the output should be a numeric
# or a string with value and unit as well. Plural -s will be ignored.
# Per default, values will be converted to meters.

#' Convert an array or vector of strings with measurements of length
#'
#' @description
#' Possible units: "twip", "thou", "inch", "hand", "foot", "yard", "chain", "furlong", "mile", "league", "millimeter", "centimeter", "meter", "kilometer" or "twip", "th", "in", "hh", "ft", "yd", "ch", "fur", "mi", "lea", "mm", "cm", "m", "km".
#' Plural "-s" and spaces in the input will be ignored
#'
#' @param old Array/Vector of strings with values with unit at the end
#' @param newunit Unit of return value, default is meters
#' @param returnstring TRUE:return is string with unit name at the end, FALSE: return is numeric data
#' @return Converted array/vector of values
#' @export
#' @examples
#' arraystrconversion(c("5yd","6ft"),"mi")
#' arraystrconversion(c("10miles","3 mile","2chain"),"yard",TRUE)
arraystrconversion <- function(old, newunit="meter", returnstring=FALSE){
new <- c()
  for (x in old){
    new <- c(new, strconversion(x,newunit,returnstring))
  }
  if(is.null(dim(old))==TRUE) {
    return(new)
  } else {
    return(array(new,dim=dim(old)))
  }
}

#-----------------------------------------------------------------------

