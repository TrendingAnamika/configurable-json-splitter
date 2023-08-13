package org.bds.json.splitter.parser

import org.bds.json.splitter.utils.HashCodeGenerator
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsValue, _}


/**
 * Created by Anamika Singh.
 * This Class has methods to parse the json String/File to split into the number of possible child nodes.
 * Enhanced by Divyanshu Shekhar Singh.
 */

class JsonParser {

  lazy val log = LoggerFactory.getLogger(this.getClass)

  //Method to parse and flatten the Json

  def jsonFlattener(js: JsValue, prefix: String = ""): JsObject = {
    def getRelationTree(oky: String, nky: String): String = {
      if (oky.nonEmpty) oky + "~>" + nky else nky
    }

    var finalJsObject: JsObject = null
    try {
      if (!js.isInstanceOf[JsObject])
        finalJsObject = Json.obj(prefix -> js)
      else
        finalJsObject = js.as[JsObject].fields.foldLeft(Json.obj()) {
          case (oky, (key, value)) => {
            oky.deepMerge(value match {
              case leaf: JsArray => leaf.as[Seq[JsValue]].zip(Stream from 1).foldLeft(oky) {
                case (oky, (nky, id: Int)) => oky.deepMerge(
                  jsonFlattener(nky.as[JsValue], getRelationTree(prefix, key) + "~>" + key + "-" + id))
              }
              case leaf: JsObject => jsonFlattener(leaf, getRelationTree(prefix, key))
              case leaf: JsBoolean => Json.obj(getRelationTree(prefix, key) -> leaf.toString())
              case leaf: JsNumber => Json.obj(getRelationTree(prefix, key) -> leaf)
              case leaf: JsString => Json.obj(getRelationTree(prefix, key) -> leaf)
              case leaf => Json.obj(getRelationTree(prefix, key) -> leaf.as[JsValue])
            })
          }
        }
    }
    catch {
      case (e: Exception) => log.error("Error occurred while tried to flatten the json using jsonFlattener ", e)
    }
    finalJsObject
  }

  //Method to convert String to Map
  def jsonStringToMap(jsonStr: String): Map[String, String] = {
    implicit val formats = org.json4s.DefaultFormats
    parse(jsonStr).extract[Map[String, String]]
  }

  //Get Created Tme from Json file for this use case.
  def getFileCreationDateTime(jsonData: JsValue, jsonRoot1: String, jsonRoot2: String, jsonRoot3: String) = {
    var fileCreationDateTime = ""
    try {
      val extractValueFunc = jsonData \ jsonRoot1 \ jsonRoot2 \ jsonRoot3
      fileCreationDateTime = extractValueFunc.as[JsString].value
      fileCreationDateTime
    }
    catch {
      case ex: Exception =>
        log.error("File_Creation_Datetime does not exist.")
        fileCreationDateTime
    }
  }

  //This Method is to Split the json for given splittedKey (json path) only in confile and not to flatten all the json nodes dinamically from parent node to child node.

  def splitJsonUsingIndexLength(json: String, root: String, splittedKey: String, fileName: String, filePath: String, scanTime: String, jobName: String) = {

    val hashId = new HashCodeGenerator
    //val jsonString = readJson(json, getErrorConfig, jobName)
    val jsonString = cleanJson(readJson(json, jobName).asInstanceOf[JsObject])
    var pruneJson: Reads[JsObject] = null
    var root1: String = null
    var root2: String = null
    var node: String = null
    var finalPrune: Any = null
    var indexLength: Long = 0
    var relationTree: String = null
    var cleanedPath: String = null
    if (!splittedKey.contains("*")) {
      node = splittedKey.split("/").last
      root1 = splittedKey.split("/")(0)
      indexLength = splittedKey.split("/").length
      if (indexLength == 2) {
        pruneJson = (__ \ node).json.copyFrom((__ \ root \ root1 \ node).json.pick)
        relationTree = root + "~>" + root1 + "~>" + node
      }
      else if (indexLength == 3) {
        root2 = splittedKey.split("/")(1)
        pruneJson = (__ \ node).json.copyFrom((__ \ root \ root1 \ root2 \ node).json.pick)
        relationTree = root + "~>" + root1 + "~>" + root2 + "~>" + node
      }
      finalPrune = Option(jsonString.transform(pruneJson)).get.get.as[JsObject] +
        ("parent_id" -> Json.toJson(hashId.parentIdHash(fileName, filePath))) +
        ("item_id" -> Json.toJson(hashId.itemIdHash(fileName, filePath, relationTree))) +
        ("scan_time" -> Json.toJson(scanTime)) +
        ("file_path" -> Json.toJson(filePath)) +
        ("object_class" -> Json.toJson(node)) +
        ("object_class_instance" -> Json.toJson(node)) +
        ("relation_tree" -> Json.toJson(relationTree)) +
        ("split_level" -> Json.toJson("domain"))
    }
    else if (splittedKey.contains("*")) {
      cleanedPath = splittedKey.replaceAll("\\**", "")
      root1 = cleanedPath.split("/")(0)
      root2 = cleanedPath.split("/")(1)
      node = cleanedPath.split("/").last
      finalPrune = Json.toJson(
        (jsonString \ root \ root1 \ root2)
          .as[Seq[JsValue]]
          .zipWithIndex
          .map { case (jsValue, index) => JsObject(Seq(
            node -> (jsValue \ node).get,
            "parent_id" -> Json.toJson(hashId.parentIdHash(fileName, filePath)),
            "item_id" -> Json.toJson(hashId.itemIdHash(fileName, filePath, root + "~>" + root1 + "~>" + root2 + "~>" + root2 + "-" + index.+(1) + "~>" + node)),
            "scan_time" -> Json.toJson(scanTime),
            "file_path" -> Json.toJson(filePath),
            "object_class" -> Json.toJson(node),
            "object_class_instance" -> Json.toJson(node),
            "relation_tree" -> Json.toJson(root + "~>" + root1 + "~>" + root2 + "~>" + root2 + "-" + index.+(1) + "~>" + node),
            "split_level" -> Json.toJson("domain")
          ))
          })
      finalPrune = finalPrune.toString.substring(1).dropRight(1)
    }
    finalPrune
  }

  //Read the Json
  def readJson(jsonString: String, jobName: String): JsValue = {
    try {
      val jsonData = Json.parse(jsonString)
      return jsonData.asInstanceOf[JsValue]
    }
    catch {
      case ex: Exception => {
        log.warn("Exception while reading the Json string: " + ex.getMessage)
       }.asInstanceOf[JsValue]
    }
  }


  //Clean the json having null or space value.
  def cleanJson(jsObject: JsObject): JsValue = {
    val filterList = Seq(JsNull, JsString(""))

    JsObject(jsObject.fields.collect {
      case (str, json: JsObject) =>
        (str, cleanJson(json))
      case (str, json: JsArray) =>
        val cleanArray: JsArray = JsArray(json.value.collect {
          case jObject: JsObject =>
            cleanJson(jObject)
          case filerArrayJsonValue if !filterList.contains(filerArrayJsonValue) =>
            filerArrayJsonValue
        })
        (str, cleanArray)
      case filerJsonValue if !filterList.contains(filerJsonValue._2) =>
        filerJsonValue
    })
  }

}
