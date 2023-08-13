package org.bds.json.splitter.parser

import org.apache.commons.lang3.StringUtils
import org.slf4j.LoggerFactory

import java.text.{DateFormat, SimpleDateFormat}
import java.util.{Date, TimeZone}

/**
 * Created by Divyanshu Shekhar Singh.
 * This Class has methods to handle and convert different possible types Date formats.
 * Enhanced by Anamika Singh.
 */


object DateConverter {
  lazy val log = LoggerFactory.getLogger(this.getClass)

  val DEFAULT_DATE_FORMAT: String = "yyyy-MM-dd HH:mm:ss.SSS"
  val yyyyMMdd = "yyyy-MM-dd"
  val DATE_DEFAULT_DELIMITER: String = "-"
  val DEFAULT_VALUE: String = ""
  val SPACE_DELIMITER: String = " "
  private val UTC_TIME_ZONE: TimeZone = TimeZone.getTimeZone("UTC")
  private var count: Int = 0

  def getConvertedDate(inputDateText: String): String = {

    if (inputDateText.endsWith("Z")) {
      val handleZuluTimeVal = handleZuluTime(inputDateText)
      handleZuluTimeVal
    }
    else {
      val formattedTime = handleTimeStampTime(inputDateText)
      formattedTime
    }
  }

  def handleZuluTime(inputDateString: String) = {
    val standardDateTimeFormat = DEFAULT_DATE_FORMAT
    var zuluTime = ""
    try {
      val inputDate = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").parse(inputDateString)
      val dateFormat = new SimpleDateFormat(standardDateTimeFormat)
      zuluTime = dateFormat.format(inputDate)
    } catch {
      case ex: Exception =>
        log.info("Exception while converting time to Standard Date Format. Input is: " + inputDateString, ex.getMessage)
    }
    zuluTime
  }

  def handleTimeStampTime(inputDateString: String) = {
    val dateFormatText = DEFAULT_DATE_FORMAT
    val defaultValueText: String = ""
    var inputDateTexts = inputDateString
    if (inputDateString.contains("T")) {
      inputDateTexts = inputDateString.replace("T", " ")
    }
    if (inputDateString.contains(" -") || inputDateString.contains(" +")) {
      inputDateTexts
    }
    else {
      try {
        val first = inputDateTexts.split(" ")(0)
        val second = inputDateTexts.split(" ")(1).replace("-", " -").replace("+", " +")
        inputDateTexts = first + " " + second
      } catch {
        case ex: Exception =>
          log.info("Exception while converting time to Standard Date Format. Input is: " + inputDateTexts, ex.getMessage)
      }
    }
    val defaultValue: String = getStringValue2(defaultValueText, DEFAULT_VALUE)
    val strDate: String = getStringValue1(inputDateTexts)
    var formattedDate: String = defaultValue
    if (StringUtils.isNotBlank(strDate)) {
      val dateFormat: String =
        getStringValue2(dateFormatText, DEFAULT_DATE_FORMAT)
      formattedDate = getFormattedDate(strDate, dateFormat, defaultValue)
      if (formattedDate == null || !formattedDate.matches(".*\\d+.*")) {
        formattedDate = defaultValue
      }
    }
    formattedDate
  }

  def getStringValue1(text: String): String = getStringValue2(text, null)

  def getStringValue2(text: String, defaultValue: String): String =
    if (text == null) defaultValue else text.toString

  def getFormattedDate(strDate: String, dateFormat: String, defaultValue: String): String = {
    var str: String = strDate
      .replaceAll("m/", "")
      .replaceAll("a. m.|a.m.", "AM")
      .replaceAll("p. m.|p.m.", "PM")
      .replaceAll(":PM|:pm", " PM")
      .replaceAll(":AM|:am", " AM")
      .replaceAll("'", "")
      .trim()
    str = str.replaceAll(
      ",|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday",
      "")
    str = str.replaceAll("[^\\x00-\\x7F]", "").trim()
    while (str.contains("  ")) str =
      str.replaceAll("  ", SPACE_DELIMITER).trim()
    val count: Int = StringUtils.countMatches(str, ".")
    if (count > 1) {
      if (count == 2) {
        var replaceStr: String = "."
        if (str.contains(". ")) {
          replaceStr = ". "
        }
        str = str.replace(replaceStr, DATE_DEFAULT_DELIMITER)
      } else {
        val lastIndex: Int = str.lastIndexOf(".")
        val yearString: String = str
          .substring(0, lastIndex)
          .replace(SPACE_DELIMITER, "")
          .replace(".", "-")
        val timeString: String = str.substring(lastIndex + 1).trim()
        str = yearString + SPACE_DELIMITER + timeString
      }
    }
    str = str.replaceAll("/|\\\\", DATE_DEFAULT_DELIMITER)
    if (!str.contains("-") && str.matches(".*\\d+.*")) {
      val st: Array[String] = str.split(" ")
      var format: String = null
      if (st.length == 1) {
        format = "yyyyMMddHHmmssSSS"
      } else {
        val firstWord: String = st(0)
        if (firstWord.length > 2) {
          if (StringUtils.isAlpha(firstWord)) {
            format = "MMM dd yyyy HH:mm:ss"
            if (str.endsWith("AM") || str.endsWith("PM")) {
              format = "MMM dd yyyy HH:mm:ss a"
            } else if (str.endsWith(" +") || str.endsWith(" -")) {
              format = "MMM dd yyyy HH:mm:ss Z"
            }
          } else {
            format = "yyyyMMdd HH:mm:ss"
            if (str.endsWith("AM") || str.endsWith("PM")) {
              format = "yyyyMMdd HH:mm:ss a"
            } else if (str.contains(" +") || str.contains(" -")) {
              format = "yyyyMMdd HH:mm:ss Z"
            }
          }
        } else {
          format = "dd MMM yyyy HH:mm:ss"
        }
      }
      str = getStringDateByDefaultFormat(str, format, dateFormat)
    } else {
      var st2: Array[String] = str.split(" ")
      val countMatches: Int = StringUtils.countMatches(str, "-")
      if (st2.length == 1 && countMatches == 2) {
        str = getStringDateByDefaultFormat(str, yyyyMMdd, dateFormat)
      } else {
        val colonCount: Int = StringUtils.countMatches(str, ":")
        if (colonCount == 1) {
          val st: Array[String] = str.split(" ")
          if (st.length == 2) {
            str = str + ":00"
          }
        }
        if (StringUtils.endsWith(str, ":")) {
          str = str + "00"
        }
        if (countMatches > 3) {
          if (countMatches == 5) {
            str = str + "-000"
          }
          if (countMatches == 6 && str.length > 23) {
            str = str.substring(0, 23)
          }
          val st: Array[String] = str.split(" ")
          if (st.length > 1) {
            val dateBuilder: StringBuilder = new StringBuilder()
            val dateFormatterBuilder: StringBuilder = new StringBuilder()
            dateBuilder.append(st(0))
            dateFormatterBuilder.append(yyyyMMdd)
            if (st.length > 1) {
              if (st(1).contains("-")) {
                dateBuilder.append(" ").append(st(1))
                dateFormatterBuilder.append(" ").append("HH-mm-ss")
              } else {
                dateBuilder.append(" ").append(st(1))
                dateFormatterBuilder.append(" ").append("HH:mm:ss")
              }
              if (st.length > 2) {
                if (st(2).contains("AM") || st(2).contains("PM")) {
                  dateBuilder.append(" ").append(st(2))
                  dateFormatterBuilder.append(" ").append("a")
                } else if (st(2).contains("+") || st(2).contains("-")) {
                  dateBuilder.append(" ").append(st(2))
                  dateFormatterBuilder.append("Z")
                }
              }
            }
            str = getStringDateByDefaultFormat(dateBuilder.toString,
              dateFormatterBuilder.toString,
              dateFormat)
          } else {
            str = getStringDateByDefaultFormat(str,
              "yyyy-MM-dd-HH-mm-ss-SSS",
              dateFormat)
          }
        }
        if (str != null && !str.isEmpty) {
          val st: Array[String] = str.split(" ")
          val yearDate: String = st(0)
          val split: Array[String] = yearDate.split("-")
          val year: String = split(0)
          val sbDate: StringBuilder = new StringBuilder()
          if (year.length == 4) {
            val sb: StringBuilder = new StringBuilder(yyyyMMdd)
            sbDate.append(yearDate)
            if (st.length > 1) {
              var timeStampVal: String = st(1)
              timeStampVal =
                timeStampVal.replaceAll("H", ":").replaceAll("h", ":")
              sbDate.append(" ").append(timeStampVal)
              if (timeStampVal.contains(".")) {
                sb.append(" HH:mm:ss.SSS")
              } else {
                sb.append(" HH:mm:ss")
              }
              if (st.length > 2) {
                val timeStamp: String = st(2).trim()
                if (timeStamp.contains("AM") || timeStamp.contains("PM")) {
                  sb.append(" a")
                  sbDate.append(" ").append(timeStamp)
                } else if (timeStamp.contains("+") || timeStamp.contains("-")) {
                  sbDate.append(" ").append(timeStamp.replace(":", ""))
                  sb.append(" Z")
                }
              }
              str = getStringDateByDefaultFormat(sbDate.toString,
                sb.toString,
                dateFormat)
            } else {
              str = getStringDateByDefaultFormat(sbDate.toString,
                sb.toString,
                dateFormat)
            }
          } else {
            if (split.length == 3) {
              val s: String = split(2)
              if (s.length == 4) {
                val month: Int = getIntegerValue(split(1))
                val sb: StringBuilder = new StringBuilder()
                if (month > 12) {
                  sb.append("MM-dd-yyyy")
                } else {
                  sb.append("dd-MM-yyyy")
                }
                sbDate.append(yearDate)
                if (st.length > 1) {
                  val timeStampVal: String =
                    st(1).replaceAll("H", ":").replaceAll("h", ":")
                  sbDate.append(" ").append(timeStampVal)
                  if (timeStampVal.contains(".")) {
                    sb.append(" HH:mm:ss.SSS")
                  } else {
                    sb.append(" HH:mm:ss")
                  }
                  if (st.length > 2) {
                    val timeStamp: String = st(2).trim()
                    if (timeStamp.contains("AM") || timeStamp.contains("PM")) {
                      sb.append(" a")
                      sbDate.append(" ").append(timeStamp)
                    } else if (timeStamp.contains("+") || timeStamp.contains(
                      "-")) {
                      sbDate.append(" ").append(timeStamp.replace(":", ""))
                      sb.append(" Z")
                    }
                  }
                  str = getStringDateByDefaultFormat(sbDate.toString,
                    sb.toString,
                    dateFormat)
                }
              } else
                str =
                  if (split(1).length == 3)
                    getStringDateByDefaultFormat(str,
                      "dd-MMM-yy HH:mm:ss a",
                      dateFormat)
                  else
                    getStringDateByDefaultFormat(str,
                      "dd-MM-yy HH:mm:ss a",
                      dateFormat)
            } else {
              str = defaultValue
            }
          }
        }
      }
    }
    str
  }

  def getStringDateByDefaultFormat(dateString: String, stringFormat: String, outputFormat: String): String = {
    var dateStrings: String = null
    try {
      val format: DateFormat = new SimpleDateFormat(stringFormat)
      val index: Int =
        if (dateString.contains(" +")) dateString.indexOf(" +")
        else if (dateString.contains("+")) dateString.indexOf("+")
        else if (dateString.contains(" -")) dateString.indexOf(" -")
        else if (dateString.contains("-")) dateString.lastIndexOf("-")
        else dateString.indexOf("-")
      var isTimeZone: Boolean = false
      if (index != -1) {
        val dateStr: String = dateString.substring(0, index)
        val timeZoneStr: String = dateString.substring(index).replace(":", "")
        dateStrings = dateStr + SPACE_DELIMITER + timeZoneStr
        isTimeZone = true
      }
      if (dateStrings.startsWith("0001-01-01"))
        return dateStrings.substring(0, index)
      val date: Date = format.parse(dateStrings)
      if (date.getYear > 0) {
        return getStringDate(date, outputFormat, isTimeZone)
      }
    } catch {
      case ex: Exception => {
        log.error(s"Exception: ${ex.getMessage}")
      }
    }
    dateStrings
  }

  def getStringDate(date: Date, dateFormat: String, isTimeZone: Boolean): String = {
    val df: DateFormat = new SimpleDateFormat(dateFormat)
    if (isTimeZone) {
      df.setTimeZone(UTC_TIME_ZONE)
    }
    df.format(date)
  }

  def getIntegerValue(str: String): Int =
    try java.lang.Integer.parseInt(str.trim())
    catch {
      case nfe: NumberFormatException =>
        return 0
    }

  def getCollectionDate(strDate: String): String = {
    var standardFormatDate = ""
    var extractedDate = ""
    if (strDate.contains("_")) {
      extractedDate = strDate.substring(strDate.lastIndexOf("_") + 1, strDate.length - 5)
    } else {
      extractedDate = strDate
    }
    val standardDateTimeFormat = DEFAULT_DATE_FORMAT
    var simpleDateFormat: String = ""
    try {
      simpleDateFormat = getDateFormat(extractedDate)
      val inputDate = new SimpleDateFormat(simpleDateFormat).parse(extractedDate)
      val standardDateFormat = new SimpleDateFormat(standardDateTimeFormat)
      standardFormatDate = standardDateFormat.format(inputDate)
    } catch {
      case ex: Exception =>
        log.error(s"Exception while converting time to Standard Date Format. Input is:  ${extractedDate}  ${ex.getMessage}")
    }
    standardFormatDate
  }

  def getDateFormat(extractedDate: String): String = {
    val mmddyyyyFormat: String = "MMddyyyyHHmmss"
    val yyyymmddFormat: String = "yyyyMMddHHmmss"
    val formatyyyymmdd: String = "^[0-3]?[0-9][0-3]?[0-9](?:[0-9]{2})?[0-9]{2}.*$"
    val formatmmddyyyy: String = "^(1[0-2]|0[1-9])(3[01]|[12][0-9]|0[1-9])[0-9]{4}.*$"
    if (extractedDate.matches(formatmmddyyyy)) {
      return mmddyyyyFormat
    }
    if (extractedDate.matches(formatyyyymmdd)) {
      return yyyymmddFormat
    }
    null
  }

  def isUTCDate(strDate: String): Boolean = {
    val standardDateTimeFormat = "yyyy-MM-dd HH:mm:ss"
    var standardFormatDate = ""
    try {
      val inputDate = new SimpleDateFormat(standardDateTimeFormat).parse(strDate)
      val standardDateFormat = new SimpleDateFormat(standardDateTimeFormat)
      standardFormatDate = standardDateFormat.format(inputDate)
      if (standardFormatDate == strDate) {
        true
      } else {
        false
      }
    } catch {
      case ex: Exception =>
        false
    }
  }
}
