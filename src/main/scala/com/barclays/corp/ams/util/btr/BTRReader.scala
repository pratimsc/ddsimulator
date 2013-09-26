package com.barclays.corp.ams.util.btr

import scala.io.Source
import com.barclays.corp.ams.util.payments.AccountNumber
import com.barclays.corp.ams.util.DDSimulatorUtil
import com.barclays.corp.ams.log.Logging
import com.barclays.corp.ams.util.payments.AccountNumber

object BTRReader extends Logging {
  /**
   * Get all the BTR Account Trailer records for Sterling Feed
   */

  def getMapOfSterlingAccountBalancesInFeed(previousBTRFeed: String, codec: String): Map[AccountNumber, BTRRecord] = {
    val btrSource = Source.fromFile(previousBTRFeed, codec)
    //Read only the Balance containing trailer record of length 100.
    val btrBalanceRecords = btrSource.getLines().filter(rec => (rec.length() == 100 && rec.substring(0, 1) == "9" )).toList
    //Close the file immediately after finishing the reading
    btrSource.close
    val accountBalanceRecords = btrBalanceRecords.map(btrBalRec => convertToGBPBTRRecordToObjects(btrBalRec.trim()))
    val acBalRecTuples = accountBalanceRecords.map(rec => (rec.fullAccountNumber, rec))
    acBalRecTuples.foreach(recO => debug("getMapOfSterlingAccountBalancesInFeed : BTR Balance Records Tuples:\t" + recO))

    acBalRecTuples.toMap
  }

  /**
   * Get all the BTR Account records for Currency Feed
   */

  def getMapOfCurrencyAccountBalancesInFeed(previousBTRFeed: String, codec: String): List[AccountNumber] = {
    val btrSource = Source.fromFile(previousBTRFeed, codec)
    //Read only the Balance containing trailer record of length 300 and having Account Type=0
    //Account Type is at position 15 i.e. (14,15)
    // Check section 3.2.3 of the DD Manual.
    val ccyRecords = btrSource.getLines().toList
    //Select only the records contributing to balance records use them for extracting Account Numbers
    val btrBalanceRecords = ccyRecords.filter(rec => (rec.length() == 300 &&
      rec.substring(0, 3) != "FLH" &&
      rec.substring(0, 3) != "FLT" &&
      rec.substring(14, 15) == "0"))
    //Close the file immediately after finishing the reading
    btrSource.close

    btrBalanceRecords match {
      case Nil => Nil
      case b :: bl =>
        val accountBalanceRecords = btrBalanceRecords.map(btrBalRec => convertToCCYBTRRecordToObjects(btrBalRec.trim()))
        val ccyAccountL = accountBalanceRecords.map(rec => {
          val r = rec.asInstanceOf[BTRCurrencyAccountTrailerRecord300]
          new AccountNumber(r.sortCode, r.accountNumber)
        }).distinct

        ccyAccountL.foreach(recO => debug("getMapOfCurrencyAccountBalancesInFeed : Following Account Numbers are present:\t" + recO))
        ccyAccountL
    }
  }

  def convertToGBPBTRRecordToObjects(record: String): BTRRecord = record.substring(0, 1) match {
    case "9" => {
      //Create BTR balance record
      new BTRAccountTrailerRecord100(sortCode = record.substring(50, 56),
        accountNumber = record.substring(56, 64),
        accountingDay = DDSimulatorUtil.getDateTime(record.substring(1, 7), DDSimulatorUtil.DT_FORMAT_DDMMYY),
        entriesCount = record.substring(7, 14).toLong,
        debitOrCreditSignForNetValue = record.substring(14, 15).toCharArray()(0),
        netValue = record.substring(15, 26).toLong,
        debitOrCreditSignForOpeningBalance = record.substring(26, 27).toCharArray()(0),
        openingBalance = record.substring(27, 38).toLong,
        debitOrCreditSignForClosingBalance = record.substring(38, 39).toCharArray()(0),
        closingBalance = record.substring(39, 50).toLong)

    }
    case "8" => EmptyBTR
    case _ => {
      //Create BTR transaction record
      new BTRAccountEntriesRecord100(sortCode = record.substring(0, 6),
        accountNumber = record.substring(6, 14),
        tlaCode = record.substring(14, 15).toCharArray()(0),
        transactionCode = record.substring(15, 17),
        originatorSortCode = record.substring(17, 23),
        originatorAccountNumber = record.substring(23, 31),
        referenceNumber = record.substring(31, 35),
        amountOfEntry = record.substring(35, 46).toLong,
        narrative1 = record.substring(46, 64),
        narrative2 = record.substring(64, 82),
        entryType = record.substring(82, 83).toCharArray()(0),
        chequeNumber = record.substring(83, 90),
        accountingDay = DDSimulatorUtil.getDateTime(record.substring(90, 96), DDSimulatorUtil.DT_FORMAT_DDMMYY))
    }
  }

  def convertToCCYBTRRecordToObjects(record: String): BTRRecord = record.substring(14, 15) match {
    case "0" => {
      //Create BTR Account Balance record
      new BTRCurrencyAccountTrailerRecord300(sortCode = record.substring(0, 6),
        accountNumber = record.substring(6, 14),
        accountType = record.substring(14, 15).charAt(0),
        transactionCode = record.substring(15, 17),
        totalValue = record.substring(53, 70).toLong,
        entryDay = DDSimulatorUtil.getDateTime(record.substring(101, 106), DDSimulatorUtil.DT_FORMAT_YYDDD))

    }
    case "1" => EmptyBTR
    case _ => EmptyBTR
  }
}

