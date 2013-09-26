package com.barclays.corp.ams.util.btr

import com.barclays.corp.ams.util.payments.AccountNumber
import com.barclays.corp.ams.util.DDSimulatorUtil
import org.joda.time.DateTime

trait BTRRecord {
  /*
   * Get the account number with sort code and account number
   */
  def fullAccountNumber: AccountNumber = new AccountNumber("000000", "00000000")

  /*
   * Add empty spaces as filler.
   */
  def fillWithCharacter(count: Int, char: Char): String = (for (i <- 1 to count) yield char).mkString

  /*
   * Left justified text with size 
   */
  def leftJustfiedFormattedString(v: String, size: Int): String = String.format("%1$-" + size + "s", v)
}

/**
 * BTR record signifying empty
 */
object EmptyBTR extends BTRRecord

/**
 * Class representing the Standard Layout 3 100 Character length trailer record that holds all balance related information.
 */

class BTRAccountTrailerRecord100(val sortCode: String,
  val accountNumber: String,
  val accountingDay: DateTime,
  val entriesCount: Long,
  val debitOrCreditSignForNetValue: Char,
  val netValue: Long,
  val debitOrCreditSignForOpeningBalance: Char,
  val openingBalance: Long,
  val debitOrCreditSignForClosingBalance: Char,
  val closingBalance: Long) extends BTRRecord {

  override def fullAccountNumber = new AccountNumber(sortCode, accountNumber)

  override def toString: String = {
    "9" + DDSimulatorUtil.getDateInFormat(accountingDay, DDSimulatorUtil.DT_FORMAT_DDMMYY) + "%07d".format(entriesCount) +
      debitOrCreditSignForNetValue + "%011d".format(netValue) +
      debitOrCreditSignForOpeningBalance + "%011d".format(openingBalance) +
      debitOrCreditSignForClosingBalance + "%011d".format(closingBalance) +
      sortCode + accountNumber + fillWithCharacter(36, ' ')
  }
}

/**
 * Class representing the Standard Layout 3 100 Character Transaction record that holds all transaction related information, for an account.
 */

class BTRAccountEntriesRecord100(val sortCode: String,
  val accountNumber: String,
  val tlaCode: Char,
  val transactionCode: String,
  val originatorSortCode: String,
  val originatorAccountNumber: String,
  val referenceNumber: String,
  val amountOfEntry: Long,
  val narrative1: String,
  val narrative2: String,
  val entryType: Char,
  val chequeNumber: String,
  val accountingDay: DateTime) extends BTRRecord {

  override def fullAccountNumber = new AccountNumber(sortCode, accountNumber)

  override def toString: String = {
    sortCode + accountNumber + tlaCode + transactionCode +
      originatorSortCode + originatorAccountNumber +
      referenceNumber + "%011d".format(amountOfEntry) +
      leftJustfiedFormattedString(narrative1, 18) + leftJustfiedFormattedString(narrative2, 18) +
      entryType + leftJustfiedFormattedString(chequeNumber, 7) +
      DDSimulatorUtil.getDateInFormat(accountingDay, DDSimulatorUtil.DT_FORMAT_DDMMYY) + fillWithCharacter(4, ' ')
  }
}

class BTRHeaderLabelRecord100(val labelIdentifier: String,
  val labelNumber: Char,
  val barclaysIdentifier: String,
  val fileID: Char,
  val volumeIdentifier: String,
  val volumeSequenceNumber: String,
  val setIdentifier: String,
  val recordFormat: Char,
  val blockLength: String,
  val recordLength: String,
  val creationDate: DateTime,
  val customerIdentifier: String,
  val workDate: DateTime) extends BTRRecord {

  /**
   * This function checks whether the input file is a direct data GBP file or not
   */
  def isDirectDataStandardFormatLayout3Sterling100File: Boolean = (fileID == 'T') && recordLength.equals("00100")

  override val toString: String = {
    labelIdentifier +
      labelNumber +
      barclaysIdentifier +
      fileID +
      volumeIdentifier +
      volumeSequenceNumber +
      setIdentifier +
      recordFormat +
      blockLength +
      recordLength +
      DDSimulatorUtil.getDateInFormat(creationDate, DDSimulatorUtil.DT_FORMAT_YYDDD) +
      DDSimulatorUtil.getDateInFormat(creationDate.plusDays(10), DDSimulatorUtil.DT_FORMAT_YYDDD) +
      customerIdentifier +
      DDSimulatorUtil.getDateInFormat(workDate, DDSimulatorUtil.DT_FORMAT_DDMMYY) +
      fillWithCharacter(43, ' ')
  }

}

class BTRTrailerLabelRecord100(val labelIdentifier: String="FLT",
  val labelNumber: Char = '1',
  val blockCount: Int = 0,
  val totalValueOfDebitItems: Int =0,
  val totalValueOfCreditItems: Int =0,
  val countOfDebitEntries: Int =0,
  val countOfCreditEntries: Int =0,
  val dataRecordCount: Int =0) extends BTRRecord {
  override def toString: String = {
    labelIdentifier + labelNumber +
      "%06d".format(blockCount) +
      "%013d".format(totalValueOfDebitItems) +
      "%013d".format(totalValueOfCreditItems) +
      "%07d".format(countOfDebitEntries) +
      "%07d".format(countOfCreditEntries) +
      "%08d".format(dataRecordCount) +
      fillWithCharacter(42, ' ')
  }
}

/**
 * Class representing the Standard Layout 3 300 Character length trailer record
 * that holds all balance related information for Currency.
 * Reference DirectData manual Section 3.2.3
 */

class BTRCurrencyAccountTrailerRecord300(val sortCode: String,
  val accountNumber: String,
  val accountType: Char,
  val transactionCode: String,
  val totalValue: Long,
  val entryDay: DateTime) extends BTRRecord {

  override def fullAccountNumber = new AccountNumber(sortCode, accountNumber)

  override def toString: String = {
    sortCode + accountNumber + accountType + transactionCode + fillWithCharacter(6, '0') +
      fillWithCharacter(8, '0') + fillWithCharacter(4, '0') + fillWithCharacter(11, '0') + fillWithCharacter(7, ' ') +
      "%017d".format(totalValue) + fillWithCharacter(12, ' ') + fillWithCharacter(18, ' ') +
      fillWithCharacter(1, ' ') + DDSimulatorUtil.getDateInFormat(entryDay, DDSimulatorUtil.DT_FORMAT_YYDDD) + //The date is in format 0YYDDD. 
      fillWithCharacter(194, ' ')
  }
}

/**
 * Class representing the Standard Layout 3 300 Character length Account Entry record.
 * Reference DirectData manual Section 3.2.3
 */

class BTRCurrencyAccountEntriesRecord300(val sortCode: String,
  val accountNumber: String,
  val accountType: Char,
  val transactionCodeDebitOrCredit: String,
  val entryDate: DateTime,
  val transactionCode: String,
  val currencyCode: String,
  val dayOneFloat: Long,
  val dayTwoFloat: Long,
  val accountingDay: DateTime,
  val amountOfEntry: Long,
  val numberOfNarrativesUsed: Char,
  val narrative1: String,
  val narrative2: String,
  val narrative3: String,
  val narrative4: String,
  val narrative5: String) extends BTRRecord {

  override def fullAccountNumber = new AccountNumber(sortCode, accountNumber)

  override def toString: String = {
    sortCode + accountNumber + accountType + transactionCodeDebitOrCredit +
      fillWithCharacter(6, ' ') + fillWithCharacter(8, ' ') + fillWithCharacter(4, '0') + fillWithCharacter(11, '0') +
      fillWithCharacter(18, ' ') + fillWithCharacter(18, ' ') + fillWithCharacter(18, ' ') +
      fillWithCharacter(1, ' ') + DDSimulatorUtil.getDateInFormat(entryDate, DDSimulatorUtil.DT_FORMAT_YYDDD) +
      fillWithCharacter(1, '0') + fillWithCharacter(6, '0') + fillWithCharacter(8, '0') +
      transactionCode + currencyCode + 
      fillWithCharacter(1, ' ')+ "%015d".format(dayOneFloat) + 
      fillWithCharacter(1, ' ') + "%015d".format(dayTwoFloat) +
      DDSimulatorUtil.getDateInFormat(accountingDay, DDSimulatorUtil.DT_FORMAT_YYMMDD) +
      fillWithCharacter(6, '0') + "%015d".format(amountOfEntry) + 
      fillWithCharacter(2, ' ') + numberOfNarrativesUsed + 
      leftJustfiedFormattedString(narrative1, 22) + leftJustfiedFormattedString(narrative2, 22) +
      leftJustfiedFormattedString(narrative3, 22) + leftJustfiedFormattedString(narrative4, 22) +
      leftJustfiedFormattedString(narrative5, 22)
  }
}

class BTRCurrencyHeaderLabelRecord300(val labelIdentifier: String = "FLH",
  val labelNumber: Char = '1',
  val barclaysIdentifier: String = "222222",
  val fileID: Char = 'F',
  val volumeIdentifier: String = "96CART",
  val volumeSequenceNumber: String = "01",
  val setIdentifier: String = "96CART",
  val recordFormat: Char = 'F',
  val blockLength: String = "05400",
  val recordLength: String = "00300",
  val creationDate: DateTime,
  val customerIdentifier: String = "63097",
  val workDate: DateTime) extends BTRRecord {

  /**
   * This function checks whether the input file is a direct data Currency file or not
   */
  val isDirectDataStandardFormatLayout3Currency300File: Boolean = (fileID == 'F') && recordLength.equals("00300")

  override val toString: String = {
    labelIdentifier + labelNumber + barclaysIdentifier + fileID +
      volumeIdentifier + volumeSequenceNumber + setIdentifier + recordFormat +
      blockLength + recordLength +
      DDSimulatorUtil.getDateInFormat(creationDate, DDSimulatorUtil.DT_FORMAT_YYDDD) +
      DDSimulatorUtil.getDateInFormat(creationDate.plusDays(10), DDSimulatorUtil.DT_FORMAT_YYDDD) +
      customerIdentifier +
      DDSimulatorUtil.getDateInFormat(workDate, DDSimulatorUtil.DT_FORMAT_DDMMYY) +
      fillWithCharacter(243, ' ')
  }
}

class BTRCurrencyTrailerLabelRecord300(val labelIdentifier: String = "FLT",
  val labelNumber: Char = '1',
  val blockCount: Int = 0,
  val countOfDebitEntries: Int = 0,
  val countOfCreditEntries: Int = 0,
  val dataRecordCount: Int = 0,
  val totalValueOfDebitItems: Int = 0,
  val totalValueOfCreditItems: Int = 0) extends BTRRecord {
  override val toString: String = {
    labelIdentifier + labelNumber +
      "%06d".format(blockCount) + "%013d".format(0) + "%013d".format(0) +
      "%07d".format(countOfDebitEntries) +
      "%07d".format(countOfCreditEntries) +
      "%08d".format(dataRecordCount) +
      "%017d".format(totalValueOfDebitItems) +
      "%017d".format(totalValueOfCreditItems) +
      fillWithCharacter(208, ' ')
  }
}
