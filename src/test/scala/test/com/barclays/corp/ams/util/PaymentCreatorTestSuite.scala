package test.com.barclays.corp.ams.util

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.barclays.corp.ams.util.payments.AccountNumber
import com.barclays.corp.ams.util.payments.AccountTransaction
import com.barclays.corp.ams.util.payments.PaymentReader
import com.barclays.corp.ams.util.payments.TransactionCreator

@RunWith(classOf[JUnitRunner])
class PaymentCreatorTestSuite extends FunSuite {

  //val codec = "UTF-8"
   val codec = "ISO-8859-1"
         
  test("TEST1 - Checking whether the payment files are converted to correct list or not") {
    val paymentFile = """C:\data\downloads\tmp\payments\test1\BARP_DFP_201303250001"""
    println("\n TEST1----------------------------------------------------------------------------------------")
    val paymentOrderList = PaymentReader.getDataFromPaymentFile(paymentFile,codec)
    paymentOrderList.foreach(x => println("PaymentCreatorTestSuite : TEST1 : PaymentOrders :\t" + x))
    println("\n TEST1----------------------------------------------------------------------------------------")
    assert(paymentOrderList.size == 3)
  }

  test("TEST2 - Checking whether all Payments order in the folder are read or not") {
    println("\n TEST2----------------------------------------------------------------------------------------")
    val paymentFolder = """C:\data\downloads\tmp\payments\test1"""
    val paymentOrderList = PaymentReader.getDataFromFolderContainingPaymentFiles(paymentFolder,codec)
    paymentOrderList.foreach(x => println("PaymentCreatorTestSuite : TEST2 : PaymentOrders :\t" + x))
    println("\n TEST2----------------------------------------------------------------------------------------")
    assert(paymentOrderList.size == 4)
  }

  test("TEST3 - Checking whether all Transactions (CREDIT and DEBIT) related to a PaymentOrder are generated or not") {
    println("\n TEST3----------------------------------------------------------------------------------------")
    val paymentFile = """C:\data\downloads\tmp\payments\test1\BARP_DFP_201303250002"""
    val paymentOrderList = PaymentReader.getDataFromPaymentFile(paymentFile,codec)
    val transactionList = TransactionCreator.registerPaymentOrderPayments(paymentOrderList.head)
    transactionList.foreach(t => println("TEST3 : transactionList :" + t))
    assert(paymentOrderList.size == 1)
    assert(paymentOrderList.head.paymentInstructions.size == 2)
    assert(transactionList.size == 4)
    println("\n TEST3----------------------------------------------------------------------------------------")
  }

  test("TEST4 - Checking whether all Transactions (CREDIT and DEBIT) related to a List of PaymentOrder are generated or not") {
    println("\n TEST4----------------------------------------------------------------------------------------")
    val paymentFolder = """C:\data\downloads\tmp\payments\test1"""
    val paymentOrderList = PaymentReader.getDataFromFolderContainingPaymentFiles(paymentFolder,codec)
    val transactionList = TransactionCreator.registerPaymentOrderListPayments(paymentOrderList)
    transactionList.foreach(t => println("TEST4 : transactionList :" + t))
    println("\n TEST4----------------------------------------------------------------------------------------")
    assert(paymentOrderList.size == 4)
    assert(transactionList.size == 20)
  }

}
