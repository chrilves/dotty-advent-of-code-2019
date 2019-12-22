package dottyaoc

import scala.annotation.tailrec

object Day2 extends Day {
  type Input = List[Int]
  type Output = Int

  def parseInput(s: String): List[Int] =
    s.split(",").map(_.toInt).toList
  
  def runProgram(initialProgram: List[Int], noun: Int, verb: Int): Int = {
    val program = initialProgram.toArray
    
    program(1) = noun
    program(2) = verb
    
    @tailrec
    def aux(p: Int): Int =
      program(p) match {
        case 1 =>
          program(program(p+3).toInt) = program(program(p+1).toInt) + program(program(p+2).toInt)
          aux(p + 4)
        case 2 =>
          program(program(p+3).toInt) = program(program(p+1).toInt) * program(program(p+2).toInt)
          aux(p + 4)
        case 99 =>
          program(0)
      }
    
    aux(0)
  }

  def partOne(initialProgram: List[Int]): Int =
    runProgram(initialProgram, 12, 2) 

  def partTwo(initialProgram: List[Int]): Int = {

    @tailrec
    def aux(noun: Int, verb: Int): Int =
      if runProgram(initialProgram, noun, verb) == 19690720
      then 100 * noun + verb
      else if verb >= 99
           then aux(noun + 1, 0)
           else aux(noun, verb + 1)
      
    aux(0,0)
  }
}