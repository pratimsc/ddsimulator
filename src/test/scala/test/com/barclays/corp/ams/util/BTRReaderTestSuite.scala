package test.com.barclays.corp.ams.util

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.barclays.corp.ams.util.btr.BTRReader
import com.barclays.corp.ams.util.payments.AccountNumber

@RunWith(classOf[JUnitRunner])
class BTRReaderTestSuite extends FunSuite {
  val codec = "ISO-8859-1"
    
  test("TEST 1 - Check whether the records are read properly or not") {
    println("\n TEST1----------------------------------------------------------------------------------------")
    val btrSourceFileName = """C:\data\downloads\tmp\btr\BARBT_GBPC2OUPQHX_20130325.GBP"""
    val btrRecords = BTRReader.getMapOfSterlingAccountBalancesInFeed(btrSourceFileName,codec);
    for (record <- btrRecords; if (record._1 == new AccountNumber("200085","63512762"))) println("Key->" + record._1 + ", Value->" + record._2)
    
    val tBalanceRec = "92203130000004-00000000174-00000000226-0000000040020008563512762                                    "
    val tBtrRec = btrRecords.get(new AccountNumber("200085","63512762")).get 
    println("tBalanceRec ->\t"+tBalanceRec)
    println("tBtrRec	 ->\t"+tBtrRec.toString())
    assert(tBtrRec.toString() == tBalanceRec)
    println("\n TEST1----------------------------------------------------------------------------------------")
  }

}
