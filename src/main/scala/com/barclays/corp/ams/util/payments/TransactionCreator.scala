package com.barclays.corp.ams.util.payments

import scala.collection.immutable.List
import org.joda.time.DateTime
import com.barclays.corp.ams.util.DDSimulatorUtil

object TransactionCreator {

  def registerPaymentOrderListPayments(paymentOrderList: List[PaymentOrderRecord]): List[AccountTransaction] =
    paymentOrderList flatMap (po => registerPaymentOrderPayments(po))

  def registerPaymentOrderPayments(paymentOrder: PaymentOrderRecord): List[AccountTransaction] =
    registerPayments(paymentOrder.paymentInstructions)

  def registerPayments(paymentList: List[PaymentRecord]): List[AccountTransaction] =
    paymentList flatMap (po => registerTransactionsRelatedToPayments(po))

  private def registerTransactionsRelatedToPayments(payment: PaymentRecord): List[AccountTransaction] = {

    val amount = new Money(scala.math.abs(payment.monetaryAmount.amountInMinorCurrency), payment.monetaryAmount.currencyCode)
    /*
     * Transaction Code and TLA code
     * Check it at section 2.6 in the document -
     * http://groupspaces.intranet.barclays.co.uk/sites/GCPS/ProductDocs/DirectData/9971735COM%20DirectData%20Customer%20Manual.pdf
     * 
     * Transaction code 82 is for Debit and 85 is for Credit
     * Narrative 1 contains reference for Debits and Narrative 2 contains reference for Credits
     */
    if (payment.debitOrCreditCode == "CREDIT") {
      //Create a transaction for the Payment originating account
      val originatorTransaction = new AccountTransaction(
        accountNumber = payment.originatorAccountNumber,
        originatingAccountNumber = payment.beneficiaryAccountNumber,
        transactionValue = new Money(amount.amountInMinorCurrency, amount.currencyCode),
        transacionDate = payment.paymentDate,
        //DirectFile Debit - Debit
        transactionCode = "82", tlaCode = "0",
        transactionReferenceNumber = "0000",
        narrative1 = amount.currencyCode match {
          case "GBP" => "EDI BGM " + payment.beneficiaryReferenceNumberCR
          case _ => "NTRF" + payment.beneficiaryReferenceNumberCR
        },
        narrative2 = amount.currencyCode match {
          case "GBP" => ""
          case _ => ""
        })
      //Create a transaction for the Payment beneficialy account
      val beneficiaryTransaction = new AccountTransaction(
        accountNumber = payment.beneficiaryAccountNumber,
        originatingAccountNumber = payment.originatorAccountNumber,
        transactionValue = new Money(amount.amountInMinorCurrency, amount.currencyCode),
        transacionDate = payment.paymentDate,
        //Transfer/CHAPS/Foreign - Credit
        transactionCode = "85", tlaCode = "8",
        transactionReferenceNumber = "0000",
        narrative1 = amount.currencyCode match {
          case "GBP" => ""
          case _ => "NTRF" + payment.beneficiaryReferenceNumberCR
        },
        narrative2 = amount.currencyCode match {
          case "GBP" => payment.beneficiaryReferenceNumberCR
          case _ => ""
        })

      //Register both credit and debit transaction associated with the Payment
      List(originatorTransaction, beneficiaryTransaction)
    } else {
      //Create a Debit instruction on the Beneficiary account

      //Create a transaction for the Payment originating account
      val originatorTransaction = new AccountTransaction(
        accountNumber = payment.originatorAccountNumber,
        originatingAccountNumber = payment.beneficiaryAccountNumber,
        transactionValue = new Money(amount.amountInMinorCurrency, amount.currencyCode),
        transacionDate = payment.paymentDate,
        //DirectFile - Credit
        transactionCode = "85", tlaCode = "8",
        transactionReferenceNumber = "0000",
        narrative1 = amount.currencyCode match {
          case "GBP" => ""
          case _ => "NTRF" + payment.beneficiaryReferenceNumberCR
        },
        narrative2 = amount.currencyCode match {
          case "GBP" => payment.beneficiaryReferenceNumberCR
          case _ => ""
        })
      //Create a transaction for the Payment beneficiary account
      val beneficiaryTransaction = new AccountTransaction(
        accountNumber = payment.beneficiaryAccountNumber,
        originatingAccountNumber = payment.originatorAccountNumber,
        transactionValue = new Money(amount.amountInMinorCurrency, amount.currencyCode),
        transacionDate = payment.paymentDate,
        //Transfer/CHAPS/Foreign - Debit
        transactionCode = "82", tlaCode = "0",
        transactionReferenceNumber = "0000",
        narrative1 = amount.currencyCode match {
          case "GBP" => "EDI BGM " + payment.beneficiaryReferenceNumberCR
          case _ => "NTRF" + payment.beneficiaryReferenceNumberCR
        },
        narrative2 = amount.currencyCode match {
          case "GBP" => ""
          case _ => ""
        })

      //Register both credit and debit transaction associated with the Payment
      List(originatorTransaction, beneficiaryTransaction)
    }

  }

}

class AccountTransaction(
  val accountNumber: AccountNumber,
  val originatingAccountNumber: AccountNumber,
  val transactionValue: Money,
  val transacionDate: DateTime,
  val transactionCode: String,
  val tlaCode: String,
  val transactionReferenceNumber: String,
  val narrative1: String,
  val narrative2: String) {

  def isCreditTransaction: Boolean = {
    transactionCode match {
      case "84" => true
      case "85" => true
      case _ => false
    }
  }

  def isDebitTransaction: Boolean = !isCreditTransaction

  override def toString = {
    "{" + "accountNumber: " + accountNumber +
      ", originatingAccountNumber: " + originatingAccountNumber +
      ", transactionValue: " + transactionValue +
      ", transacionDate: " + DDSimulatorUtil.getDateInFormat(transacionDate, DDSimulatorUtil.DT_FORMAT_CCYYMMDD) +
      ", transactionCode: " + transactionCode +
      ", tlaCode: " + tlaCode +
      ", transactionReferenceNumber: " + transactionReferenceNumber +
      ", narrative1: " + narrative1 +
      ", narrative2: " + narrative2 +
      "}"
  }
}
  
  
