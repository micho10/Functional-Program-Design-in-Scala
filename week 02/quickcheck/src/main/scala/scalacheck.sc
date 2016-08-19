import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

val propConcatLists = forAll {
  (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
}.check



val propSqrt = forAll {
  (n: Int) => scala.math.sqrt(n * n) == n
}.check



val propReverseList = forAll {
  l: List[String] => l.reverse.reverse == l
}.check



val propConcatString = forAll {
  (s1: String, s2: String) => (s1 + s2).endsWith(s2)
}.check


// Using a generator
val smallInteger = Gen.choose(0, 100)
val propSmallInteger = Prop.forAll(smallInteger) {
  n => n >= 0 && n <= 100
}.check



// Using the implication operator (==>) to introduce pre-conditions
val propMakeList = forAll {
  n: Int => (n >= 0 && n < 10000) ==> (List.fill(n)("").length == n)
}.check



val propTrivial = forAll {
  n: Int => (n == 0) ==> (n == 0)
}.check

// Using implications a property might not only pass or fail,
// it could also be undecided if the implication condition is not fulfilled



// Labeling properties
def myMagicFunction(a: Int, b: Int): Int = a + b
val complexProp = forAll {
  (m: Int, n: Int) =>
    val res = myMagicFunction(n, m)
    (res >= m)    :| "result > #1" &&
    (res >= n)    :| "result > #2" &&
    (res < m + n) :| "result not sum"
//      ("result > #1"    |: res >= m) &&
//      ("result > #2"    |: res >= m) &&
//      ("result not sum" |: res < m + n)
}.check















































































