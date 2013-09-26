package com.barclays.corp.ams.util.btr

import scala.io.Source
import scala.math.abs
import com.barclays.corp.ams.util.payments.PaymentReader
import com.barclays.corp.ams.util.payments.TransactionCreator
import com.barclays.corp.ams.util.payments.AccountTransaction
import com.barclays.corp.ams.util.payments.AccountNumber
import java.io.File
import com.barclays.corp.ams.util.payments.Money
import com.barclays.corp.ams.log.Logging
import org.joda.time.DateTime
import com.barclays.corp.ams.util.DDSimulatorUtil

object BTRCreator extends Logging {

  def main(args: Array[String]): Unit = {
    require(!(args.length < 1), "Please enter the Configuration file name. e.g. xyx.config")

    val propertyFile = args(0)
    val records = Source.fromFile(propertyFile).getLines()
    val propRecords = records.filterNot(rec => rec.isEmpty() || rec.trim.substring(0, 1).equals("#"))
    val props = propRecords.map(rec => {
      val sRec = rec.split('=')
      (sRec(0).trim(), sRec(1).trim())
    }).toMap

    info("Following properties will be used for processing")
    info("_______________________________________________________________________")
    props.foreach(p => info("Key:\t" + p._1 + ", Value:\t" + p._2))

    val DD_PREVIOUS_ACC_DATE_FEED_FOLDER = props.get("DD_PREVIOUS_ACC_DATE_FEED_FOLDER").get
    val DD_PRESENT_ACC_DATE_FEED_FOLDER = props.get("DD_PRESENT_ACC_DATE_FEED_FOLDER").get + File.separatorChar
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER = props.get("DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER").get
    val DD_PRESENT_ACC_DATE = props.get("DD_PRESENT_ACC_DATE").get
    val DF_CODEC = props.get("DF_CODEC").get
    val DD_CODEC = props.get("DD_CODEC").get

    info("Getting all payment order from Payment files from location '" + DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER + "'")
    val paymentOrders = PaymentReader.getDataFromFolderContainingPaymentFiles(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER, DF_CODEC)
    info("Generating transaction for all payment orders......")
    val paymentTransactions = TransactionCreator.registerPaymentOrderListPayments(paymentOrders)

    val accountTransactions = transactionListToMap(paymentTransactions)
    info("Following transactions will be used for creating the Direct Data feeds.")
    info("_______________________________________________________________________")
    accountTransactions.foreach(tr => {
      info("Transaction for Account Number :\t" + tr._1)
      tr._2.foreach(x => info("\t\tTransaction ->\t" + x))
    })
    info("_______________________________________________________________________")

    //Now pick up each of GBP direct data file and create a MAP of balance information
    val ddInputFolder = new File(DD_PREVIOUS_ACC_DATE_FEED_FOLDER)
    val ddInputFiles = ddInputFolder.listFiles().toList.filterNot(_.isDirectory())
    if (ddInputFiles == Nil || ddInputFiles.size != 3) error("There should be ONLY 3 files in the previous accounting day folder." +
      "Two files containing the GBP(Sterling) account transactions." +
      "One file containing the CCY(Currency) account transactions.")

    //Split the file list into 2 categories i.e. GBP and CCY
    val (ddGBPFiles, ddCCYFiles) = ddInputFiles.partition(file =>
      getFileHeader(file.getCanonicalPath(), DD_CODEC) match {
        case r: BTRHeaderLabelRecord100 => r.isDirectDataStandardFormatLayout3Sterling100File
        case _ => false
      })

    info("Processing following Sterling GBP files -")
    ddGBPFiles.foreach(f => info(f.getCanonicalPath()))
    info("Processing following Currency CCY files -")
    ddCCYFiles.foreach(f => info(f.getCanonicalPath()))

    //Calculate the Accounting Date. It should be date as present in the Transactions.
    val accountingDateBasedOnTransactionList = calculateTheTargetAccountingDate(paymentTransactions) match {
      case null => DDSimulatorUtil.getDateTime(DD_PRESENT_ACC_DATE, DDSimulatorUtil.DT_FORMAT_CCYYMMDD)
      case date => date
    }

    //Processing accounts for GBP files
    info("Begin processing of the GBP Sterling files.......")
    for (gbpFile <- ddGBPFiles) {
      val ddData = generateSterlingDDRecordsForFollowingAccountingDate(BTRReader.getMapOfSterlingAccountBalancesInFeed(gbpFile.getCanonicalPath(), DD_CODEC), accountTransactions, accountingDateBasedOnTransactionList)
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      ddData.foreach(data => {
        debug("--------->New Account Information for GBP Account -> \t" + data._1)
        val trnRec = data._2._2
        trnRec.foreach(r => debug("Trans Record:\t" + r))
        debug("Header Record :\t" + data._2._1)
      })
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      val outputFileName = DD_PRESENT_ACC_DATE_FEED_FOLDER + gbpFile.getName() + "_" + System.currentTimeMillis()
      info("Creating the direct data feed for input file : '" + gbpFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
      val prevFileHeader = getFileHeader(gbpFile.getCanonicalPath(), DD_CODEC)

      generateBTRFeedGBPStandardLayoutFormat3Record100(outputFileName, ddData, prevFileHeader, accountingDateBasedOnTransactionList)
      info("Creation of the SIMULATED direct data feed COMPLETED for input file : '" + gbpFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
    }
    info("Finished processing of the GBP Sterling files.......")
    info("Begin processing of the CCY Currency files.......")
    //Processing accounts for CCY : Currency files 
    for (ccyFile <- ddCCYFiles) {
      val ddData = generateCurrencyDDRecordsForFollowingAccountingDate(BTRReader.getMapOfCurrencyAccountBalancesInFeed(ccyFile.getCanonicalPath(), DD_CODEC),
        accountTransactions, accountingDateBasedOnTransactionList)
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      ddData.foreach(data => {
        debug("--------->New Account Information for CCY Account -> \t" + data._1)
        val btrRec = data._2
        btrRec.foreach(r => debug("BTR Record:\t" + r))

      })
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      val outputFileName = DD_PRESENT_ACC_DATE_FEED_FOLDER + ccyFile.getName() + "_" + System.currentTimeMillis()
      info("Creating the direct data feed for input file : '" + ccyFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
      val prevFileHeader = getFileHeader(ccyFile.getCanonicalPath(), DD_CODEC) match {
        case EmptyBTR => new BTRCurrencyHeaderLabelRecord300(creationDate = accountingDateBasedOnTransactionList.minusDays(1), workDate = accountingDateBasedOnTransactionList.minusDays(1))
        case x: BTRCurrencyHeaderLabelRecord300 => x
      }
      generateBTRFeedCCYStandardLayoutFormat3Record300(outputFileName, ddData, prevFileHeader, accountingDateBasedOnTransactionList)
      info("Creation of the SIMULATED direct data feed COMPLETED for input file : '" + ccyFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
    }
    info("Finished processing of the CCY Currency files.......")

    info("++++++++++++++++++++++++++++++++++++++++++++")
    info("+  FINISHED Creating all simulated files.  +")
    info("++++++++++++++++++++++++++++++++++++++++++++")

  }

  /**
   * Create a Map, where all the transaction associated with an Account can be fetched using Account number.
   */
  private def transactionListToMap(paymentTransactions: List[AccountTransaction]): Map[AccountNumber, List[AccountTransaction]] = paymentTransactions match {
    case Nil => Map()
    case t :: tl =>
      //Get all transactions belonging to one Account at a time
      val transaction = t.accountNumber
      val transactionList = t :: tl.filter(x => t.accountNumber == x.accountNumber)
      val accMap = Map(t.accountNumber -> transactionList)
      accMap ++ transactionListToMap(tl.filterNot(x => t.accountNumber == x.accountNumber))
  }

  /**
   * This function checks whether the input file is a direct data GBP file or not
   */
  def getFileHeader(file: String, codec: String): BTRRecord = {
    val src = Source.fromFile(file, codec);
    val rec = src.getLines.filter(rec => !rec.trim.isEmpty()).toList
    src.close
    rec match {
      case Nil => EmptyBTR
      case h :: hl => DDSimulatorUtil.convertRecordToBTRRecord(h)
    }

  }

  /**
   * This function will take in  2 paramters.
   * prevBTRBalances = Previous accounting day's balance as reported in GBP sterling file name
   * transactions = list of payment transactions processed on the prsent account date
   * It will return a Map of record containing Account number and List containing BTR record objects.
   */
  private def generateSterlingDDRecordsForFollowingAccountingDate(prevBTRBalances: Map[AccountNumber, BTRRecord],
    transactions: Map[AccountNumber, List[AccountTransaction]],
    accountingDate: DateTime): Map[AccountNumber, (BTRRecord, List[BTRRecord])] = {
    //Iterate over each element in the Map and create a new Map with Updated Balance
    val newBalances = for (prevBalance <- prevBTRBalances) yield {
      val account = prevBalance._1
      val prevAccBal = prevBalance._2.asInstanceOf[BTRAccountTrailerRecord100]

      debug("Carrying out Transaction processing for Account ->" + account)
      if (transactions.get(account) == None) {
        //Return the Old account without after updating the Accounting date
        val presentAccBal = new BTRAccountTrailerRecord100(sortCode = account.sortCode,
          accountNumber = account.accountNumber,
          accountingDay = accountingDate,
          entriesCount = 0,
          debitOrCreditSignForNetValue = ' ',
          netValue = 0,
          debitOrCreditSignForOpeningBalance = prevAccBal.debitOrCreditSignForOpeningBalance,
          openingBalance = prevAccBal.openingBalance,
          debitOrCreditSignForClosingBalance = prevAccBal.debitOrCreditSignForClosingBalance,
          closingBalance = prevAccBal.closingBalance)

        (account, (presentAccBal, List(EmptyBTR)))
      } else {
        val accTranL = transactions.get(account).get

        //Create information to be included in the new Account Balance object
        //Get all credit transactions
        val creditTranL = accTranL.filter(t => t.isCreditTransaction)
        debug("Following CREDIT Transactions will be used for ->\t" + account)
        creditTranL.foreach(ct => debug("Credit Transaction ->\t" + ct))

        //Get all debit transactions
        val debitTranL = accTranL.filterNot(t => t.isCreditTransaction)
        debug("Following DEBIT Transactions will be used for ->\t" + account)
        debitTranL.foreach(dt => debug("Debit Transaction ->\t" + dt))

        //Get total of all credit transactions 
        val totalCreditValue = creditTranL.foldLeft(new Money(0L, ""))(
          (sum, tran) =>
            new Money(sum.amountInMinorCurrency + tran.transactionValue.amountInMinorCurrency, tran.transactionValue.currencyCode))
        //Get total of all debit transactions
        val totalDebitValue = debitTranL.foldLeft(new Money(0L, ""))(
          (sum, tran) =>
            new Money(sum.amountInMinorCurrency + tran.transactionValue.amountInMinorCurrency, tran.transactionValue.currencyCode))
        //Get the net value of the transaction for the day
        val netValue = totalCreditValue.amountInMinorCurrency - totalDebitValue.amountInMinorCurrency
        //Get the present closing balance
        val prevClosingBalace = if (prevAccBal.debitOrCreditSignForClosingBalance == '-') (-1) * prevAccBal.closingBalance else prevAccBal.closingBalance
        val closingBalance = prevClosingBalace + netValue

        val presentAccBal = new BTRAccountTrailerRecord100(sortCode = account.sortCode,
          accountNumber = account.accountNumber,
          accountingDay = accountingDate,
          entriesCount = creditTranL.size + debitTranL.size,
          debitOrCreditSignForNetValue = if (netValue < 0) '-' else ' ',
          netValue = abs(netValue),
          debitOrCreditSignForOpeningBalance = prevAccBal.debitOrCreditSignForClosingBalance,
          openingBalance = prevAccBal.closingBalance,
          debitOrCreditSignForClosingBalance = if (closingBalance < 0) '-' else ' ',
          closingBalance = closingBalance)

        //Get all the transactions in BTR Record format
        val creditBTRRecords = creditTranL.map(convertToBTR(_))
        val debitBTRRecords = debitTranL.map(convertToBTR(_))

        //Return the Account information with new enriched data
        debug("generateDDRecordsForFollowingAccountingDate : Returning the BTR records for the account ->\t" + account)
        (account, (presentAccBal, (debitBTRRecords ++ creditBTRRecords)))
      }
    }
    newBalances.toMap
  }

  /**
   * This function will take in  2 paramters.
   * prevBTRBalances = Previous accounting day's balance as reported in Currency sterling file name
   * transactions = list of payment transactions processed on the present account date
   * It will return a Map of record containing Account number and List containing BTR record objects.
   */
  private def generateCurrencyDDRecordsForFollowingAccountingDate(currencyAccounts: List[AccountNumber],
    transactions: Map[AccountNumber, List[AccountTransaction]], batchDate: DateTime): Map[AccountNumber, List[BTRRecord]] = {
    //Iterate over each element in the Map and create a new Map with Updated Balance
    val newBalances = for (account <- currencyAccounts) yield {

      debug("generateCurrencyDDRecordsForFollowingAccountingDate: Carrying out Currency Transaction processing for Account ->" + account)
      if (transactions.get(account) == None) {
        //Create 2 BTR Records for Credit and Debit
        val dbRec = new BTRCurrencyAccountTrailerRecord300(sortCode = account.sortCode, accountNumber = account.accountNumber,
          accountType = '0', transactionCode = "44", totalValue = 0, entryDay = batchDate)
        val crRec = new BTRCurrencyAccountTrailerRecord300(sortCode = account.sortCode, accountNumber = account.accountNumber,
          accountType = '0', transactionCode = "54", totalValue = 0, entryDay = batchDate)
        //Return a Map with Account information
        (account -> List(dbRec, crRec))
      } else {
        val accTranL = transactions.get(account).get

        //Create information to be included in the new Account Balance object
        //Get all credit transactions
        val creditTranL = accTranL.filter(t => t.isCreditTransaction)
        debug("generateCurrencyDDRecordsForFollowingAccountingDate: Following CREDIT Transactions will be used for ->\t" + account)
        creditTranL.foreach(ct => debug("Credit Transaction ->\t" + ct))

        //Get all debit transactions
        val debitTranL = accTranL.filterNot(t => t.isCreditTransaction)
        debug("generateCurrencyDDRecordsForFollowingAccountingDate: Following DEBIT Transactions will be used for ->\t" + account)
        debitTranL.foreach(dt => debug("Debit Transaction ->\t" + dt))

        //Get total of all credit transactions 
        val totalCreditValue = creditTranL.foldLeft(new Money(0L, ""))(
          (sum, tran) =>
            new Money(sum.amountInMinorCurrency + tran.transactionValue.amountInMinorCurrency, tran.transactionValue.currencyCode))
        //Get total of all debit transactions
        val totalDebitValue = debitTranL.foldLeft(new Money(0L, ""))(
          (sum, tran) =>
            new Money(sum.amountInMinorCurrency + tran.transactionValue.amountInMinorCurrency, tran.transactionValue.currencyCode))

        //Get the net value of the transaction for the day
        val netValue = totalCreditValue.amountInMinorCurrency - totalDebitValue.amountInMinorCurrency

        //Get all the transactions in BTR Record format
        val dbTranL = debitTranL.map(convertToBTR(_))
        val crTranL = creditTranL.map(convertToBTR(_))
        //Create 2 BTR Records for Credit and Debit
        val dbRec = new BTRCurrencyAccountTrailerRecord300(sortCode = account.sortCode, accountNumber = account.accountNumber,
          accountType = '0', transactionCode = "44", totalValue = totalDebitValue.amountInMinorCurrency, entryDay = batchDate)
        val crRec = new BTRCurrencyAccountTrailerRecord300(sortCode = account.sortCode, accountNumber = account.accountNumber,
          accountType = '0', transactionCode = "54", totalValue = totalCreditValue.amountInMinorCurrency, entryDay = batchDate)

        //Return the Account information with new enriched data
        debug("generateCurrencyDDRecordsForFollowingAccountingDate : Returning the BTR records for the account ->\t" + account)
        (account -> ((dbTranL ++ crTranL) ++ List(dbRec, crRec)))
      }
    }
    newBalances.toMap
  }

  /**
   * This is a helper funtion that would convert a transaction record to a BTR record
   */
  private def convertToBTR(transaction: AccountTransaction): BTRRecord = transaction.transactionValue.currencyCode match {

    case "GBP" => new BTRAccountEntriesRecord100(sortCode = transaction.accountNumber.sortCode,
      accountNumber = transaction.accountNumber.accountNumber,
      tlaCode = transaction.tlaCode.charAt(0),
      transactionCode = transaction.transactionCode,
      originatorSortCode = transaction.originatingAccountNumber.sortCode,
      originatorAccountNumber = transaction.originatingAccountNumber.accountNumber,
      referenceNumber = transaction.transactionReferenceNumber,
      amountOfEntry = transaction.transactionValue.amountInMinorCurrency,
      narrative1 = transaction.narrative1,
      narrative2 = transaction.narrative2,
      entryType = '2', //Automated entry. DD Manual section 2.3.5
      chequeNumber = "0000000", //Zero-filled check number
      accountingDay = transaction.transacionDate)

    case _ => new BTRCurrencyAccountEntriesRecord300(sortCode = transaction.accountNumber.sortCode,
      accountNumber = transaction.accountNumber.accountNumber,
      accountType = '1',
      transactionCodeDebitOrCredit = if (transaction.isCreditTransaction) "60" else "11",
      entryDate = transaction.transacionDate,
      transactionCode = if (transaction.isCreditTransaction) "T200" else "T201",
      currencyCode = transaction.transactionValue.currencyCode,
      dayOneFloat = 0,
      dayTwoFloat = 0,
      accountingDay = transaction.transacionDate,
      amountOfEntry = transaction.transactionValue.amountInMinorCurrency,
      numberOfNarrativesUsed = '2',
      narrative1 = transaction.narrative1,
      narrative2 = transaction.narrative2,
      narrative3 = "",
      narrative4 = "",
      narrative5 = "")

  }

  /**
   * This helper function with take a MAP of the BTR records and file name.
   * And write GBP BTR the records to file
   */

  private def generateBTRFeedGBPStandardLayoutFormat3Record100(gbpOutFeedName: String,
    data: Map[AccountNumber, (BTRRecord, List[BTRRecord])],
    fileHeader: BTRRecord,
    batchDate: DateTime): Unit = {

    val formatedData = data.flatMap(d => {
      val trailer = d._2._1
      val transactions = d._2._2
      val dataL1: List[String] = List(trailer.toString())
      val dataL2: List[String] = transactions match {
        case EmptyBTR :: Nil => List[String]()
        case t :: tl => transactions.map(_.toString())
      }
      dataL2 ::: dataL1
    })

    val previousFileHeader = fileHeader.asInstanceOf[BTRHeaderLabelRecord100]
    val headerRecord = new BTRHeaderLabelRecord100(labelIdentifier = previousFileHeader.labelIdentifier,
      labelNumber = previousFileHeader.labelNumber,
      barclaysIdentifier = previousFileHeader.barclaysIdentifier,
      fileID = previousFileHeader.fileID,
      volumeIdentifier = previousFileHeader.volumeIdentifier,
      volumeSequenceNumber = previousFileHeader.volumeSequenceNumber,
      setIdentifier = previousFileHeader.setIdentifier,
      recordFormat = previousFileHeader.recordFormat,
      blockLength = previousFileHeader.blockLength,
      recordLength = previousFileHeader.recordLength,
      creationDate = batchDate,
      customerIdentifier = previousFileHeader.customerIdentifier,
      workDate = batchDate)

    val trailer = new BTRTrailerLabelRecord100(
      dataRecordCount = formatedData.size).toString

    val fileData: List[String] = (headerRecord.toString :: formatedData.toList) ::: List(trailer.toString())

    printToFile(new File(gbpOutFeedName))(op => fileData.foreach(op.println(_)))
  }

  /**
   * This helper function with take a MAP of the BTR records and file name.
   * And write CCY BTR the records to file
   */

  private def generateBTRFeedCCYStandardLayoutFormat3Record300(gbpOutFeedName: String,
    data: Map[AccountNumber, List[BTRRecord]], prevFileHeader: BTRRecord, batchDate: DateTime): Unit = {
    val fileTrailer = List(new BTRCurrencyTrailerLabelRecord300())
    val fileHeader = new BTRCurrencyHeaderLabelRecord300(creationDate = batchDate,
      customerIdentifier = prevFileHeader.asInstanceOf[BTRCurrencyHeaderLabelRecord300].customerIdentifier,
      workDate = batchDate)
    val fileData = (fileHeader :: data.flatMap(d => d._2).toList) ::: fileTrailer

    printToFile(new File(gbpOutFeedName))(op => fileData.foreach(r => op.println(r.toString())))
  }

  /**
   * Helper function to write to a file
   */
  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /**
   * Helper function to get the Batch date
   *
   */

  private def calculateTheTargetAccountingDate(transactions: List[AccountTransaction]): DateTime = {
    def max(transactions: List[AccountTransaction], date: DateTime): DateTime = transactions match {
      case Nil => date
      case t :: tl =>
        if (date.getMillis - t.transacionDate.getMillis > 0) max(tl, date) else max(tl, t.transacionDate)
    }

    transactions match {
      case Nil => null
      case t :: tl => max(tl, t.transacionDate)
    }
  }

}
