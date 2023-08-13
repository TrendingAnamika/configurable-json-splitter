package org.bds.json.splitter.parser

import org.bds.json.splitter.parser.DateConverter._
import org.bds.json.splitter.utils.Constants._
import org.bds.json.splitter.utils.HashCodeGenerator
import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import java.lang.System.currentTimeMillis
import java.util.concurrent.TimeUnit
import scala.collection.breakOut

/**
 * Created by Anamika Singh.
 * This Class has methods to process the Split the Json in expected formats.
 * Enhanced by Divyanshu Shekhar Singh.
 */

class ParseUtil {
  val parser = new JsonParser
  val hashId = new HashCodeGenerator


  def jsonParser(json: String, fileName: String, filePath: String, fileNameDate: String, jobName: String, root: String, domainLevelKeys: String) = {

    var st = currentTimeMillis()
    var et = currentTimeMillis()
    val jsonData = parser.readJson(json, jobName)

    val getRelationTree = (str: String) => str.substring(0, str.lastIndexOf("~>"))
    val getAttribute = (str: String) => str.substring(str.lastIndexOf("~>") + 2)
    val getEntity = (str: String) => {
      val strErrLog = JSON_ROOT_1
      var entity = ""
      if (str != strErrLog) {
        entity = str.substring(str.lastIndexOf("~>") + 2)
        if (entity.indexOf("-") > 0)
          entity.substring(0, entity.indexOf("-"))
        else
          entity
      }
      else strErrLog

    }
    val getObjInstance = (str: String) => {
      val strErrLog = JSON_ROOT_1
      if (str != strErrLog)
        str.substring(str.lastIndexOf("~>") + 2)
      else
        strErrLog
    }

    def getCollectionDateInUTC(collectionFileDate: String, fileNameDate: String) = {
      var convertedDate: String = ""
      if (collectionFileDate.trim != "")
        if (isUTCDate(collectionFileDate) == true) convertedDate = collectionFileDate
        else convertedDate = getConvertedDate(collectionFileDate)
      if (convertedDate == "" || convertedDate == null) {
        if (fileNameDate.trim != "")
          convertedDate = getCollectionDate(fileNameDate)
        else convertedDate = "Date Time Not Found"
      }
      convertedDate
    }


    val fileCreationDate = parser.getFileCreationDateTime(jsonData, JSON_ROOT_1, JSON_ROOT_2, JSON_ROOT_3)
    val scanTime = getCollectionDateInUTC(fileCreationDate, fileNameDate)

    log.info("jsonFlattener Started")
    st = currentTimeMillis()
    val relationTreeData = parser.jsonFlattener(jsonData).toString()
    log.info("jsonFlattener Complete")
    et = currentTimeMillis()
    log.info(s"[${jobName}] jsonFlattener took : ${TimeUnit.MILLISECONDS.toSeconds(et - st)} sec")

    val mapData = parser.jsonStringToMap(relationTreeData)
    val filterBlankValues = mapData.filterKeys(
      key => Option(mapData(key)).map(x => x.toString.trim()).exists(_ != "")
    )

    //constructing domain keys to filter out to avoid creating leaf level splitting
    val getKeyFromPath: List[String] = domainLevelKeys.split(",").map(_.trim)(breakOut)
    val splitKeys = for {
      keys <- getKeyFromPath
      splitKeys = "~>" + keys.split("/").last + "~>"
    } yield splitKeys

    val splitDomainKeys = splitKeys.mkString(",")

    //Excluding domain keys from leaf level output
    val keys = splitDomainKeys.split(",")

    val filteredDomainLevelMap = filterBlankValues.filter(x => !keys.exists(k => x._1.contains(k)))
    val listData = filteredDomainLevelMap.toList

    implicit val formats = Serialization.formats(NoTypeHints)
    val resultData = listData
      .map(map => (getRelationTree(map._1), getAttribute(map._1), map._2))
      .groupBy(_._1)
      .map(tuple => {
        Map(
          "parent_id" -> hashId.parentIdHash(fileName, filePath),
          "item_id" -> hashId.itemIdHash(fileName, filePath, tuple._1),
          "scan_time" -> scanTime,
          "file_path" -> filePath,
          "object_class" -> getEntity(tuple._1),
          "object_class_instance" -> getObjInstance(tuple._1),
          "relation_tree" -> tuple._1,
          "split_level" -> "leaf",
          getEntity(tuple._1) -> (tuple._2.map(a => (a._2, a._3)).toMap)
        )

      })

    import org.json4s.native.Serialization.write
    val listOfJson = write(resultData)
    val leafLevelJson = listOfJson.substring(1).dropRight(1)
    var joinedJson: String = null

    joinedJson = "[" + leafLevelJson
    val path: List[String] = domainLevelKeys.split(",").map(_.trim)(breakOut)
    for (splittedKey <- path) {
      try {
        var getDomainLevelJson = parser.splitJsonUsingIndexLength(json, root, splittedKey, fileName, filePath, scanTime, jobName)
        if (joinedJson.equalsIgnoreCase("["))
          joinedJson = "[" + getDomainLevelJson
        else
          joinedJson = joinedJson + "," + getDomainLevelJson

      }
      catch {
        case exception: Exception => {
          log.warn("Invalid leaf Node " + exception.getMessage)
          }
      }
    }
    joinedJson = joinedJson + "]"
    joinedJson
  }


  def getPath(path: String, contRef: String): String = {
    path.substring(path.indexOf("/", path.indexOf(contRef)) + 1)
  }
}
