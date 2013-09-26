package com.barclays.corp.ams.util.payments

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.Iterator
import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import com.barclays.corp.ams.util.DDSimulatorUtil
import com.barclays.corp.ams.log.Logging

object PaymentReader extends Logging {

  /**
   * This will return all the payments present in payment files in folder.
   */
  def getDataFromFolderContainingPaymentFiles(paymentFolder: String, codec: String): List[PaymentOrderRecord] = {
    val dir = new File(paymentFolder)
    val paymentFiles = dir.listFiles().toList.filterNot(_.isDirectory())
    debug(paymentFiles.map(x => "getDataFromFolderContainingPaymentFiles:Payment Files ->\t" + x.getCanonicalPath()))
    val paymentOrdersList = paymentFiles flatMap (x => getDataFromPaymentFile(x.getCanonicalPath(), codec))
    paymentOrdersList
  }

  /**
   * This return all the lines in the Payment file that contributes to any Payment
   */
  def getDataFromPaymentFile(paymentFileName: String, codec: String): List[PaymentOrderRecord] = {
    debug("getDataFromPaymentFile: starting processing for file :" + paymentFileName)
    val src = Source.fromFile(paymentFileName, codec)
    val linesInFile = src.getLines.toList

    //All payments in the PAYEXT file contain records separated by character "'"
    // Give a list of the records in same sequence, after splitting them to chunks
    //val splittedRecords = for (lines <- linesInFile) yield lines.trim.split("\\'").filter(doesItContributeToPaymentRecord(_)).toList    
    //val paymentContributingRecordLines = splittedRecords.foldLeft(new ListBuffer[String])((fList, iList) => {
    //  fList.appendAll(iList)
    //  fList
    //}).toList
    val paymentContributingRecordLines = linesInFile flatMap (lines => lines.trim.split("\\'").filter(doesItContributeToPaymentRecord(_)))
    src.close

    //Return all the PaymentOrder present in the payment file
    val paymentOrders = getPaymentOrderRecords(paymentContributingRecordLines)
    debug(paymentOrders.map(x => println("getDataFromPaymentFile : Payment Orders :\t\t" + paymentOrders)))
    paymentOrders

  }

  private def getPaymentOrderRecords(paymentFileLines: List[String]): List[PaymentOrderRecord] = {

    //Start the BIG loop to create the list of Payment orders and payments.
    //Note : To be improved later
    var isPaymentOrderBeingConstructed = false
    var isPaymentBeingConstructed = false
    val paymentOrdersL: ListBuffer[PaymentOrderRecord] = new ListBuffer

    /*
     * Below the variables are declared. 
     */
    var orderingPartyName: String = null
    var paymentOrderExecutionDateTime: String = "01010101"
    var originatorReferenceNumberAEK: String = null
    var paymentOrderMoney: Money = null
    var paymentOrderDebitOrCreditCode: String = null
    var originatorAccountNumber: AccountNumber = null
    var paymentInstructions: ListBuffer[PaymentRecord] = new ListBuffer

    /*
     * Below are the variables for Individual payments
     */
    var beneficiaryMoney: Money = null
    var beneficiaryReferenceNumberCR: String = null
    var beneficiaryAccountNumber: AccountNumber = null
    var beneficiaryDebitOrCreditCode: String = null

    for (rec <- paymentFileLines) {
      debug("getPaymentOrderRecords : processing record :" + rec)
      edifactTagBelongingToTheRecord(rec) match {
        case "LIN" =>
          /*
       * Here the Payment order creation for previous has to be finished
       * LIN+1++1932325STILESHARO
       */

          if (isPaymentOrderBeingConstructed) {
            //Check whether from Payment instruction has been added or not
            if (isPaymentBeingConstructed) {
              val payInst = new PaymentRecord(
                originatorAccountNumber = originatorAccountNumber,
                beneficiaryAccountNumber = beneficiaryAccountNumber,
                originatorReferenceNumberAEK = originatorReferenceNumberAEK,
                beneficiaryReferenceNumberCR = beneficiaryReferenceNumberCR,
                debitOrCreditCode = beneficiaryDebitOrCreditCode,
                monetaryAmount = beneficiaryMoney,
                paymentDate = DDSimulatorUtil.getDateTime(paymentOrderExecutionDateTime, DDSimulatorUtil.DT_FORMAT_CCYYMMDD))
              paymentInstructions.append(payInst)
            }
            //Write a code to finish the completion of the Payment Order here
            val paymentOrder = new PaymentOrderRecord(originatorAccountNumber,
              originatorReferenceNumberAEK,
              paymentOrderDebitOrCreditCode,
              paymentOrderMoney,
              paymentInstructions.toList)
            paymentOrdersL.append(paymentOrder)
          }

          //Initialize all values relayted to Payment Order
          paymentOrderExecutionDateTime = "01010101"
          originatorReferenceNumberAEK = null
          paymentOrderMoney = null
          originatorAccountNumber = null
          paymentInstructions = new ListBuffer
          isPaymentOrderBeingConstructed = true
          isPaymentBeingConstructed = false
          orderingPartyName = rec.split("\\+").toList.last

        case "DTM+203" =>
          //DTM+203:20130325:102
          paymentOrderExecutionDateTime = rec.split("\\+").toList.last.split("\\:").toList.tail.head

        case "RFF+AEK" =>
          //RFF+AEK:I13032500000001
          originatorReferenceNumberAEK = rec.split("\\+").last.split("\\:").toList.last

        case "MOA" =>
          //MOA+9:0.00:GBP
          val moneyList = rec.split("\\+").toList.last.split("\\:").toList
          val money = new Money(getAmountInMinorCurrency(moneyList.tail.head), moneyList.tail.last)

          //Check whether the money belong to Payment Order or Instruction
          if (isPaymentBeingConstructed) {
            beneficiaryMoney = money
            if (beneficiaryMoney.amountInMinorCurrency < 0)
              beneficiaryDebitOrCreditCode = "DEBIT"
            else
              beneficiaryDebitOrCreditCode = "CREDIT"
          } else {
            paymentOrderMoney = money
            if (paymentOrderMoney.amountInMinorCurrency < 0)
              paymentOrderDebitOrCreditCode = "DEBIT"
            else
              paymentOrderDebitOrCreditCode = "CREDIT"
          }

        case "FII+OR" =>
          val fiiOrL = rec.split("\\+").toArray
          val accountNumberA = fiiOrL(2).split("\\:").toArray
          val sortCodeA = fiiOrL(3).split("\\:").toArray
          val brCode = if (sortCodeA(0).isEmpty() && !sortCodeA(3).isEmpty()) sortCodeA(3) else sortCodeA(0)
          val sortCode = if (sortCodeA(0).length() <= 6) brCode else brCode.substring(brCode.length() - 6)
          val accNum = if (accountNumberA(0).length() <= 8) accountNumberA(0) else accountNumberA(0).substring(accountNumberA(0).length() - 8)
          originatorAccountNumber = new AccountNumber(sortCode, accNum)

        case "SEQ" =>
          //SEQ++0001
          if (isPaymentBeingConstructed) {
            val payInst = new PaymentRecord(
              originatorAccountNumber = originatorAccountNumber,
              beneficiaryAccountNumber = beneficiaryAccountNumber,
              originatorReferenceNumberAEK = originatorReferenceNumberAEK,
              beneficiaryReferenceNumberCR = beneficiaryReferenceNumberCR,
              debitOrCreditCode = beneficiaryDebitOrCreditCode,
              monetaryAmount = beneficiaryMoney,
              paymentDate = DDSimulatorUtil.getDateTime(paymentOrderExecutionDateTime, DDSimulatorUtil.DT_FORMAT_CCYYMMDD))
            paymentInstructions.append(payInst)
          }

          //Initialize all values related to Paymet Instruction
          beneficiaryMoney = null
          beneficiaryReferenceNumberCR = null
          beneficiaryAccountNumber = null
          beneficiaryDebitOrCreditCode = null
          isPaymentBeingConstructed = true

        case "RFF+CR" =>
          //RFF+CR:IW000204361
          beneficiaryReferenceNumberCR = rec.split("\\+").tail.head.split("\\:").last

        case "FII+BF" =>
          //FII+BF+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133
          val fiiBfL = rec.split("\\+").toArray
          val sortCodeA = fiiBfL(3).split("\\:").toArray
          val accountNumberA = fiiBfL(2).split("\\:").toArray
          val brCode = if (sortCodeA(0).isEmpty() && !sortCodeA(3).isEmpty()) sortCodeA(3) else sortCodeA(0)
          val sortCode = if (sortCodeA(0).length() <= 6) brCode else brCode.substring(brCode.length() - 6)
          val accNum = if (accountNumberA(0).length() <= 8) accountNumberA(0) else accountNumberA(0).substring(accountNumberA(0).length() - 8)
          beneficiaryAccountNumber = new AccountNumber(sortCode, accNum)

        case "NAD+OY" =>
        //NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB

        case "NAD+BE" =>
        //NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB

        case _ =>
        //println("This record could not be parsed and is not used for processing :\t" + rec + " Contributing tag" + edifactTagBelongingToTheRecord(rec))
      }

    }
    /*
     * Check whether some payment order and payment instruction needs completing. 
     */
    if (isPaymentOrderBeingConstructed) {
      //Check whether from Payment instruction has been added or not
      if (isPaymentBeingConstructed) {
        val payInst = new PaymentRecord(
          originatorAccountNumber = originatorAccountNumber,
          beneficiaryAccountNumber = beneficiaryAccountNumber,
          originatorReferenceNumberAEK = originatorReferenceNumberAEK,
          beneficiaryReferenceNumberCR = beneficiaryReferenceNumberCR,
          debitOrCreditCode = beneficiaryDebitOrCreditCode,
          monetaryAmount = beneficiaryMoney,
          paymentDate = DDSimulatorUtil.getDateTime(paymentOrderExecutionDateTime, DDSimulatorUtil.DT_FORMAT_CCYYMMDD))
        paymentInstructions.append(payInst)
      }
      //Write a code to finish the completion of the Payment Order here
      val paymentOrder = new PaymentOrderRecord(originatorAccountNumber,
        originatorReferenceNumberAEK,
        paymentOrderDebitOrCreditCode,
        paymentOrderMoney,
        paymentInstructions.toList)
      paymentOrdersL.append(paymentOrder)
    }

    paymentOrdersL.toList

  }

  private def doesItContributeToPaymentRecord(recordLine: String): Boolean = {
    val rec = recordLine.trim()
    val nonContributingTags = List[String]("UNB", "UNH", "BGM", "DTM+137", "UNT", "UNZ")

    if (rec.isEmpty())
      false
    else
      !checkForPresence(rec, nonContributingTags)
  }

  private def edifactTagBelongingToTheRecord(recordLine: String): String = {
    val rec = recordLine.trim()
    val contributingTags = List[String]("LIN", "DTM+203", "RFF+AEK", "MOA", "FII+OR", "SEQ", "RFF+CR", "PAI", "FII+BF", "NAD+OY", "NAD+BE")

    if (checkForPresence(rec, contributingTags))
      contributingTags.filter(rec.trim.startsWith(_)).toList.head
    else
      rec

  }

  //This function checks whether the search tags are present in the begining of the record or not
  private def checkForPresence(rec: String, searchTags: List[String]): Boolean = searchTags.filter(rec.trim.startsWith(_)).length > 0

  //Below splitting and merging to remove the decimal places and have everything in minor currency
  private def getAmountInMinorCurrency(amount: String): Long = {
    val al = amount.split("\\.").toList
    if (al.length == 1)
      return al.head.toLong * 100
    else if (al.length == 2)
      return getAmountInMinorCurrency(al.head) + al.last.toLong
    else
      return 0
  }

}

case class AccountNumber(val sortCode: String, val accountNumber: String) {
  override def toString = sortCode + accountNumber

  override def equals(other: Any): Boolean = other match {
    case that: AccountNumber => (that canEqual this) &&
      sortCode.equalsIgnoreCase(that.sortCode) &&
      accountNumber.equalsIgnoreCase(that.accountNumber)

    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[AccountNumber]

  override def hashCode: Int = sortCode.hashCode() + accountNumber.hashCode()
}

case class Money(val amountInMinorCurrency: Long, val currencyCode: String) {
  override def toString = "%011d".format(amountInMinorCurrency) + currencyCode
}

case class PaymentRecord(
  val originatorAccountNumber: AccountNumber,
  val beneficiaryAccountNumber: AccountNumber,
  val originatorReferenceNumberAEK: String,
  val beneficiaryReferenceNumberCR: String,
  val debitOrCreditCode: String,
  val monetaryAmount: Money,
  val paymentDate: DateTime) {
  override def toString = {
    "{ originatorAccountNumber:" + originatorAccountNumber +
      ", beneficiaryAccountNumber:" + beneficiaryAccountNumber +
      ", originatorReferenceNumberAEK:" + originatorReferenceNumberAEK +
      ", beneficiaryReferenceNumberCR:" + beneficiaryReferenceNumberCR +
      ", debitOrCreditCode:" + debitOrCreditCode +
      ", monetaryAmount:" + monetaryAmount +
      ", paymentDate:" + DDSimulatorUtil.getDateInFormat(paymentDate, DDSimulatorUtil.DT_FORMAT_CCYYMMDD) + "}"
  }
}

case class PaymentOrderRecord(val originatorAccountNumber: AccountNumber,
  val originatorReferenceNumberAEK: String,
  val debitOrCreditCode: String,
  val totalMonetaryAmount: Money,
  val paymentInstructions: List[PaymentRecord]) {
  override def toString = {

    "{originatorAccountNumber:" + originatorAccountNumber +
      ", originatorReferenceNumberAEK:" + originatorReferenceNumberAEK +
      ", debitOrCreditCode::" + debitOrCreditCode +
      ", totalMonetaryAmount:" + totalMonetaryAmount +
      ", paymentOrders:[" + paymentInstructions.mkString("\n") + "]}"
  }

}
  
