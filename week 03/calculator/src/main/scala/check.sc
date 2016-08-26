import org.scalacheck.Gen
import org.scalacheck.Prop._

def ordered(l: List[Int]) = l == l.sorted

val myProp = forAll { l: List[Int] =>
  classify(ordered(l), "ordered") {
    classify(l.length > 5, "large", "small") {
      l.reverse.reverse == l
    }
  }
}

myProp.check


val dummProp = forAll(Gen.choose(1, 1000)) {
  n => collect(n) {
    n == n
  }
}

dummProp.check

