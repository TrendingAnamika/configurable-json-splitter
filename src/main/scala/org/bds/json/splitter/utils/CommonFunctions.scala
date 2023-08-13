package org.bds.json.splitter.utils

import java.text.SimpleDateFormat
import java.util.Calendar

/**
 * Created by Anamika Singh
 * This Class contains methods used for common functions.
 *
 */

object CommonFunctions {

  def fileCollectionDate(fileName: String): String = {
    val fileNameDate: String = ""
    if (fileName.contains("TEST"))
      fileName.substring(fileName.indexOf("_TEST_") + 6, fileName.indexOf(".json"))
    else fileNameDate
  }


  def fileLocation(fileName: String) = {
    if (fileName.contains("TEST")) "TEST"
    else if (fileName.contains("Archive")) "Archive"
    else "test"
  }

  def snappyPathCreation(snappyPath: String) = {
    val format = new SimpleDateFormat("yyyy/MM/dd")
    val formattedDate = format.format(Calendar.getInstance().getTime())
    snappyPath + "/" + formattedDate //+ "/"
  }

  def dateCreation(): String = {
    val cal = Calendar.getInstance()
    var date = cal.get(Calendar.DATE).toString
    val Year = cal.get(Calendar.YEAR)
    val dateTime = cal.getTime
    var dateFormat = new SimpleDateFormat("MM")
    val Month = dateFormat.format(dateTime)
    dateFormat = new SimpleDateFormat("dd")
    date = dateFormat.format(dateTime)
    var fileDate = (Year + "_" + Month + "_" + date).toString
    fileDate
  }
}
