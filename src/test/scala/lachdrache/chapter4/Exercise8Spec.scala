package lachdrache.chapter4

import org.specs2.mutable.Specification

class Exercise8Spec extends Specification {

  case class Person(name: Name, age: Age)
  sealed case class Name(value: String)
  sealed case class Age(value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  /** Gathers the error messages */
  def mkPersonValidate(name: String, age: Int): Either[List[String], Person] =
    mkName(name).map2Validate(mkAge(age))(Person(_, _))

  "mkPerson" should {
    {
      mkPerson("Tst", 13) === Right(Person(Name("Tst"), Age(13)))
    }.eg

    {
      mkPerson("", 3) === Left("Name is empty.")
    }.eg

    {
      mkPerson("Tst", -1) === Left("Age is out of range.")
    }.eg

    {
      mkPerson("", -1) === Left("Name is empty.")
    }.eg
  }

  "mkPerson2" should {
    {
      mkPersonValidate("", -1) === Left(List("Name is empty.", "Age is out of range."))
    }.eg

    {
      mkPersonValidate("Tst", 13) === Right(Person(Name("Tst"), Age(13)))
    }.eg

    {
      mkPersonValidate("", 3) === Left(List("Name is empty."))
    }.eg

    {
      mkPersonValidate("Tst", -1) === Left(List("Age is out of range."))
    }.eg

  }
}