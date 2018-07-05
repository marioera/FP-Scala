package stackoverflow

import org.scalatest.{ FunSuite, BeforeAndAfterAll }
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {

  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  val conf: SparkConf = new SparkConf().setAppName("stackoverflow-testsuite").setMaster("local[4]")
  val sc: SparkContext = new SparkContext(conf)
  lazy val lines = sc.textFile(getClass.getClassLoader.getResource("stackoverflow/stackoverflow_original.csv").getPath)
  //lazy val lines = sc.textFile(getClass.getClassLoader.getResource("stackoverflow/stackoverflow.csv").getPath)
  lazy val raw = testObject.rawPostings(lines)

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("reading lines from CSV file") {
    assert(raw.count() === 4000000)
  }

}
