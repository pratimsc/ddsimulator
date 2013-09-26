package test.com.barclays.corp.ams.util

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.joda.time.DateTime
import com.barclays.corp.ams.util.DDSimulatorUtil

@RunWith(classOf[JUnitRunner])
class DDSimulatorUtilTest extends FunSuite {

  test("Test1 : getDateInFormat : DT_FORMAT_YYDDD") {
    println("\n TEST1----------------------------------------------------------------------------------------")
    val date = new DateTime()
    val yearYY = date.getYearOfCentury()
    val dayOfYearDDD = date.getDayOfYear()
    val formattedDateString = DDSimulatorUtil.getDateInFormat(date, DDSimulatorUtil.DT_FORMAT_YYDDD)
    println("date->\t\t" + date)
    println("\n TEST1----------------------------------------------------------------------------------------")
    assert(formattedDateString.equals(yearYY + "" + dayOfYearDDD))
  }

  test("Test2 : getDateInFormat : DT_FORMAT_DDMMYY") {
    println("\n TEST2----------------------------------------------------------------------------------------")
    val date = new DateTime()
    val yearYY = date.getYearOfCentury()
    val monthOfYearMM = date.getMonthOfYear()
    val dayOfMonthrDD = date.getDayOfMonth()
    val expectedDateString = "%06d".format(dayOfMonthrDD * 10000 + monthOfYearMM * 100 + yearYY)
    val formattedDateString = DDSimulatorUtil.getDateInFormat(date, DDSimulatorUtil.DT_FORMAT_DDMMYY)
    println("expectedDateString->\t" + expectedDateString)
    println("formattedDateString->\t" + formattedDateString)
    println("date->\t\t" + date)
    println("\n TEST2----------------------------------------------------------------------------------------")
    assert(formattedDateString.equals(expectedDateString))
  }

  test("Test3 : getDateTime : DT_FORMAT_DDMMYY") {
    println("\n TEST3----------------------------------------------------------------------------------------")
    val date = new DateTime()
    val yearYY = date.getYearOfCentury()
    val monthOfYearMM = date.getMonthOfYear()
    val dayOfMonthrDD = date.getDayOfMonth()
    val expectedDateString = "%06d".format(dayOfMonthrDD * 10000 + monthOfYearMM * 100 + yearYY)

    println("expectedDateString DT_FORMAT_DDMMYY ->\t\t" + expectedDateString)
    val dateFromformattedDateString = DDSimulatorUtil.getDateTime(expectedDateString, DDSimulatorUtil.DT_FORMAT_DDMMYY)
    val formattedDateString = DDSimulatorUtil.getDateInFormat(dateFromformattedDateString, DDSimulatorUtil.DT_FORMAT_DDMMYY)

    println("dateFromformattedDateString->\t\t" + dateFromformattedDateString)
    println("\n TEST3----------------------------------------------------------------------------------------")
    assert(expectedDateString == formattedDateString)
  }

}
