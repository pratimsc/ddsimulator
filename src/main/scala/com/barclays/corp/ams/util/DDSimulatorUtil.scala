package com.barclays.corp.ams.util

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import com.barclays.corp.ams.log.Logging
import org.joda.time.format.DateTimeFormatter
import com.barclays.corp.ams.util.btr.BTRRecord
import com.barclays.corp.ams.util.btr.BTRHeaderLabelRecord100
import com.barclays.corp.ams.util.btr.EmptyBTR
import com.barclays.corp.ams.util.btr.BTRCurrencyHeaderLabelRecord300

object DDSimulatorUtil extends Logging {
  val DT_FORMAT_CCYYMMDD = "CCYYMMdd"
  val DT_FORMAT_DDMMYY = "ddMMYY"
  val DT_FORMAT_YYMMDD = "YYMMdd"
  val DT_FORMAT_YYDDD = "YYDDD"

  def getDateInFormat(date: DateTime, outFormat: String): String = {
    try {
      date.toString(DateTimeFormat.forPattern(outFormat))
    } catch {
      case t: Throwable =>
        "01010101"
    }
  }

  def getDateTime(dateStr: String, inputFormat: String): DateTime = {
    try {
      DateTimeFormat.forPattern(inputFormat).parseDateTime(dateStr)
    } catch {
      case _: Throwable =>
        DateTimeFormat.forPattern(DT_FORMAT_CCYYMMDD).parseDateTime("01010101")
    }
  }

  def convertRecordToBTRRecord(record: String): BTRRecord = {
    if (record == null || record.isEmpty())
      EmptyBTR
    else {
      //Pick up only the Header/Footers for GPB and Sterling files
      (record.substring(0, 4), record.substring(10, 11).charAt(0)) match {
        case ("FLH1", 'T') =>
          new BTRHeaderLabelRecord100(
            labelIdentifier = record.substring(0, 3),
            labelNumber = record.substring(3, 4).charAt(0),
            barclaysIdentifier = record.substring(4, 10),
            fileID = record.substring(10, 11).charAt(0),
            volumeIdentifier = record.substring(11, 17),
            volumeSequenceNumber = record.substring(17, 19),
            setIdentifier = record.substring(19, 25),
            recordFormat = record.substring(25, 26).charAt(0),
            blockLength = record.substring(26, 31),
            recordLength = record.substring(31, 36),
            creationDate = DDSimulatorUtil.getDateTime(record.substring(36, 41), DDSimulatorUtil.DT_FORMAT_YYDDD),
            customerIdentifier = record.substring(46, 51),
            workDate = DDSimulatorUtil.getDateTime(record.substring(51, 57), DDSimulatorUtil.DT_FORMAT_DDMMYY))
        //Pick up only the Header/Footers for CCY and Currency files
        case ("FLH1", 'F') =>
          new BTRCurrencyHeaderLabelRecord300(
            labelIdentifier = record.substring(0, 3),
            labelNumber = record.substring(3, 4).charAt(0),
            barclaysIdentifier = record.substring(4, 10),
            fileID = record.substring(10, 11).charAt(0),
            volumeIdentifier = record.substring(11, 17),
            volumeSequenceNumber = record.substring(17, 19),
            setIdentifier = record.substring(19, 25),
            recordFormat = record.substring(25, 26).charAt(0),
            blockLength = record.substring(26, 31),
            recordLength = record.substring(31, 36),
            creationDate = DDSimulatorUtil.getDateTime(record.substring(36, 41), DDSimulatorUtil.DT_FORMAT_YYDDD),
            customerIdentifier = record.substring(46, 51),
            workDate = DDSimulatorUtil.getDateTime(record.substring(51, 57), DDSimulatorUtil.DT_FORMAT_DDMMYY))
        case _ =>
          EmptyBTR
      }

    }
  }
}
