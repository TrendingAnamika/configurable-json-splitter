package org.bds.json.splitter
import org.bds.json.splitter.utils.Constants._
import org.slf4j.LoggerFactory

object JsonSplitter {

  def main(args: Array[String]): Unit = {
      val log = LoggerFactory.getLogger(this.getClass)
      log.info(" ===============Logger Initiated. ===============")
      val sourceType = PROP_FILE

    var env = ""
    var conf_path = ""
    var appConfigName = ""
    var xsltFileName = ""

    if (args.length < 1){
      env =  System.getenv.get("argument_1").toLowerCase()
      conf_path =  System.getenv.get("argument_2").toString
      appConfigName =  System.getenv.get("argument_3").toString
      xsltFileName =  System.getenv.get("argument_4").toString

    }
    else{
      env =  args(0)
      conf_path =  args(1)
      appConfigName = args(2)
      xsltFileName =  args(3)
    }
    ENV = env


    log.info(" ===============Conf Received. Connecting to Keyvault ===============")
    //val scrtClient = KeyVaultIntegration.getKeyVaultConnection(cId, cScrt, tnntId, vlt)
    //val scrtNamesList = aiScrtkey + "," + blobAccScrtKey + "," + blobConnectionStringScrtKey + "," +  srcEhScrtKey + "," + errorEhScrtKey + "," + targetEhScrtKey + "," + filterEhScrtKey + "," + offsetBlobAccScrtKey + "," + offsetBlobClientIDTemp + "," + offsetBlobSecretTemp + "," + offsetBlobTenantIDTemp

    //scrtValuesList = KeyVaultIntegration.getScrtValues(scrtClient, scrtNamesList)
    //val aiInstrumentationKey = scrtValuesList.getOrElse(aiScrtkey, throw new Exception("Exception occurred while retrieving App Insight key vault password"))


    var checkpointPath = ""
    var checkpointPathQuery = ""

      log.info(" ===============Getting Offset Values from Valve===============")

    //updateCheckpointToAzureBlob:  to set up for Hadoop Configuration
   // val sparkSession = new SingletonSparkSession().getSparkSession("jobName")
    if (env != TEST && env != "unit") {
      checkpointPath = "tmp"//EventHubsIntegration.updateCheckpointToAzureBlob(sparkSession, defaultFs, offSetPath, offsetBlobClientID, offsetBlobTenantID, offsetBlobSecret)
      //checkpointPathQuery = checkpointPath
    }

    //Get config details
    if (env != TEST && env != "unit") {
     // val eventDataFrame = EventHubsIntegration.readEventHubs(sparkSession, cTopic, cBootstrapServers, cEventHubsSasl, kafkaRequestTimeoutMs, kafkaSessionTimeoutMs, startingOffsets, maxOffsetsPerTrigger, jobName)
      //val parsedMessage = EventHubsIntegration.parseEventData(sparkSession, eventDataFrame, errorConfig, sourceFileType, xsltFileName, blobConnectionString, containerName, root, domainLevelKeys, pCompressedContainerName, pCompressedPath, scrtValuesList, blobAccNm, pCompressedFs, blobKey, blobName, jobName, aiInstrumentationKey, fileSizeFilter, largeCollectionEHConfig, isBacklogEnabled)
      //EventHubsIntegration.eventHubsProducer(parsedMessage, pTopic, pBootstrapServers, pEventHubsSasl, checkpointPathQuery, triggerInterval, maxOffsetsPerTrigger, jobName)
      ""
    }
  }
}




