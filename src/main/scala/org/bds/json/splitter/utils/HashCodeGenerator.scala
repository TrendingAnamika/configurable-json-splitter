package org.bds.json.splitter.utils

/**
 * Created by anamika_singh3 on 02/20/2020.
 * This Class has methods to generate Hash Code for Parent_Id and Item_Id.
 */


class HashCodeGenerator {

  import com.google.gson.Gson

  val gson = new Gson

  case class ParentId(payload: String, filePath: String)

  case class ItemId(payload: String, filePath: String, relationTreeValue: String)

  //Parent_Id HashCode Method
  def parentIdHash(fileName: String, filePath: String) = {
    val payload = fileName;
    val comParentId = ParentId(payload, filePath)
    val comParentIdHashCode = gson.toJson(comParentId).hashCode
    comParentIdHashCode
   }


  //Item_Id HashCode Method
  def itemIdHash(fileName: String, filePath: String, relationTree: String) = {
    val payload = fileName;
    val relationTreeValue = relationTree;
    val comItemId = ItemId(payload, filePath, relationTreeValue)
    val comItemIdHashCode = gson.toJson(comItemId).hashCode
    comItemIdHashCode
   }


}
