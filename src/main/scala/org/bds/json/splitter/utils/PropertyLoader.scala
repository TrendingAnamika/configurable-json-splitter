package org.bds.json.splitter.utils

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory._

import java.io.File

/**
 * Created by Anamika Singh.
 * This Class has methods to load the Config file.
 */


object PropertyLoader {

  
  def readConfig(fileName: String): Config = {
    load(fileName)
  }

  def readConfigFromPath(): Config = {
    load()
  }

  def readConfigFromPathForAKS(pth: String): Config = {
    var config: Config = null
    try {
      config = load(parseFile(new File(pth)))
    } catch {
      case ex: Exception => {
        throw ex
      }
    }
    config
  }
}
