import scala.math._

object Day4 extends Day {
  type Input = (Int, Int)
  type Output = Int

  def parseInput(s: String): Input = {
    val Array(from, to) = s.split("-").map(_.toInt)
    (from, to)
  }

  def partOne(i: Input): Output = {
    val from = i._1
    val to   = i._2

    inline def (password:Int) isValid: Boolean =
      (password>=from) && (password<=to) && {
        def aux(rightDigit: Int, seenSame: Boolean, remaining: Int, number: Int): Boolean =
          if remaining <= 0
          then seenSame
          else {
            val leftDigit = number % 10
            if leftDigit > rightDigit
            then false
            else aux(leftDigit, seenSame || (leftDigit == rightDigit), remaining - 1, number / 10) 
          }
        
        aux(password % 10, false, 5, password / 10)
      }

    (from to to).filter(_.isValid).size  
  }


  def partTwo(i: Input): Output = {
    val from = i._1
    val to   = i._2
    
    enum MachtingRuleState {
      def next(lastDigitsWereTheSame: Boolean): MachtingRuleState =
        this match {
          case SeenMatchingGroupOf2 => SeenMatchingGroupOf2
          case GroupOf1             => if lastDigitsWereTheSame
                                       then GroupOf2
                                       else GroupOf1
          case GroupOf2             => if lastDigitsWereTheSame
                                       then GroupOfMoreThan2
                                       else SeenMatchingGroupOf2
          case GroupOfMoreThan2     => if lastDigitsWereTheSame
                                       then GroupOfMoreThan2
                                       else GroupOf1 
        }

      case SeenMatchingGroupOf2, GroupOf1, GroupOf2, GroupOfMoreThan2
    }

    inline def (password:Int) isValid: Boolean =
      (password>=from) && (password<=to) && {
        def aux(rightDigit: Int, seenSame: MachtingRuleState, remaining: Int, number: Int): Boolean =
          if remaining <= 0
          then seenSame.next(false) == MachtingRuleState.SeenMatchingGroupOf2 
          else {
            val leftDigit = number % 10
            if leftDigit > rightDigit
            then false
            else aux(leftDigit, seenSame.next(leftDigit == rightDigit), remaining - 1, number / 10) 
          }
        
        aux(password % 10, MachtingRuleState.GroupOf1, 5, password / 10)
      }

    (from to to).filter(_.isValid).size  
  }
}