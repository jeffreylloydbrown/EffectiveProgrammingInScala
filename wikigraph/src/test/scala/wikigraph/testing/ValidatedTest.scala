package wikigraph.testing

import munit.FunSuite
import wikigraph.Validated
import wikigraph.Validated.*

import java.time.{LocalDate, Period}
import scala.util.{Failure, Success, Try}

class ValidatedTest extends FunSuite:

  private def July27 = "2020-07-27"
  private def July27Date = LocalDate.parse(July27)
  private def Christmas = "2020-12-25"
  private def ChristmasDate = LocalDate.parse(Christmas)
  private def bad1 = "2020-19-27"
  private def bad2 = "2020-22-25"

  type Input = String
  type ValidatedDate[A] = Validated[Input, A]

  private def parseDate(input: Input): ValidatedDate[LocalDate] =
    Try(LocalDate.parse(input)) match {
      case Success(date) => Right(date)
      case Failure(ex) =>   Left(Seq(ex.getMessage))
    }

  test("parseDate positive"){
    assertEquals(parseDate(Christmas), Right(ChristmasDate))
  }

  test("parseDate negative"){
    parseDate(bad1) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 1)
        assert(errors.head.contains(bad1))
      case _ => fail("didn't get the expected Left for a bad date")
    }
  }

  test("zip positive"){
    assertEquals(zip(parseDate(July27), parseDate(Christmas)),
      Right((July27Date, ChristmasDate)))
  }

  test("zip negative"){
    zip(parseDate(bad1), parseDate(Christmas)) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 1)
        assert(errors.head.contains(bad1))
      case _ => fail("didn't get the expected Left for bad 1st date")
    }

    zip(parseDate(July27), parseDate(bad2)) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 1)
        assert(errors.head.contains(bad2))
      case _ => fail("didn't get the expected Left for bad 2nd date")
    }

    zip(parseDate(bad1), parseDate(bad2)) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 2)
        Seq(bad1, bad2).foreach(date => assert(errors.exists(_.contains(date))))
      case _ => fail("didn't get the expected Left when both dates bad")
    }
  }

  test("traverse positive"){
    assertEquals(traverse(Seq(July27, Christmas))(parseDate), Right(Seq(July27Date, ChristmasDate)))
  }

  test("traverse negative"){
    traverse(Seq(bad1, Christmas))(parseDate) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 1)
        assert(errors.head.contains(bad1))
      case _ => fail("didn't get the expected Left for bad 1st date")
    }

    traverse(Seq(July27, bad2))(parseDate) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 1)
        assert(errors.head.contains(bad2))
      case _ => fail("didn't get the expected Left for bad 2nd date")
    }

    traverse(Seq(bad1, bad2))(parseDate) match {
      case Left(errors: Seq[Input]) =>
        assertEquals(errors.size, 2)
        Seq(bad1, bad2).foreach(date => assert(errors.exists(_.contains(date))))
      case _ => fail("didn't get the expected Left for both bad dates")
    }
  }

end ValidatedTest

