package test.com.barclays.corp.ams.util

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.barclays.corp.ams.util.payments.AccountNumber
import com.barclays.corp.ams.util.payments.AccountTransaction
import com.barclays.corp.ams.util.payments.PaymentReader
import com.barclays.corp.ams.util.payments.TransactionCreator
import scala.collection.mutable.HashMap
import java.nio.charset.Charset
import com.barclays.corp.ams.util.btr.BTRCreator
import java.io.File
import com.barclays.corp.ams.util.btr.BTRHeaderLabelRecord100
import com.barclays.corp.ams.util.btr.BTRAccountTrailerRecord100
import com.barclays.corp.ams.util.btr.BTRHeaderLabelRecord100

@RunWith(classOf[JUnitRunner])
class BTRCreatorTestSuite extends FunSuite {

  //val codec = "UTF-8"
  val codec = "ISO-8859-1"

  test("TEST1 - isDirectDataStandardFormatLayout3Sterling100File - Validates a GBP file") {
    val ddFile1 = """C:\data\downloads\tmp\btr\BARBT_GBPC2OUPQHX_20130325.GBP"""
   
    println("\n TEST1----------------------------------------------------------------------------------------")    
    val header = BTRCreator.getFileHeader(ddFile1, codec)
    val testHeader = """FLH1222222T96CART0196CARTF0180000100130811309163097220313                                           """
    println("Recieved Header ->\t["+header+"]")
    println("Expected Header ->\t["+testHeader+"]")
    println("\n TEST1----------------------------------------------------------------------------------------")
    assert(header.isInstanceOf[BTRHeaderLabelRecord100])
    assert(header.toString().equals(testHeader))
  }
  
  test("TEST2 - isDirectDataStandardFormatLayout3Sterling100File - Validates a GBP file") {
    val ddFile1 = """C:\data\downloads\tmp\btr\BARBT_GBPC2OUPQHX_20130325.GBP"""
    val ddFile2 = """C:\data\downloads\tmp\btr\BARFC_FCTRAN_20130325.TXT"""
    println("\n TEST2----------------------------------------------------------------------------------------")
    
   
    val isSterlingDDFile = BTRCreator.getFileHeader(ddFile1, codec) match {
      case r: BTRHeaderLabelRecord100 => r.isDirectDataStandardFormatLayout3Sterling100File
      case _ => false
    }
    val isCurrencyFile = BTRCreator.getFileHeader(ddFile2, codec) match {
      case r: BTRHeaderLabelRecord100 => r.isDirectDataStandardFormatLayout3Sterling100File
      case _ => false
    }

    println("\n TEST2----------------------------------------------------------------------------------------")
    assert(isSterlingDDFile == true)
    assert(isCurrencyFile == false)
  }

}
