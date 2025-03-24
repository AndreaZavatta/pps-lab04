package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double, Double)
    def complex(re: Double, im: Double): Complex = (re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case Complex(re, _) => re
      def im(): Double = complex match
        case Complex(_, im) => im
      def sum(other: Complex): Complex = (complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex = (complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case Complex(re, im) if re == 0.0 && im == 0.0 => "0.0"
        case Complex(re, im) if re == 0.0 => im + "i"
        case Complex(re, im) if im == 0.0 => ""+re
        case _ => complex.re() + getSign(complex.im()) + getNumberMod(complex.im())+"i"
      private def getSign(num: Double): String = if num < 0 then " - " else " + "
      private def getNumberMod(num: Double): String = if num < 0 then (num * -1).toString else num.toString