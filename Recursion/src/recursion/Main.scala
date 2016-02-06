package recursion

object Main {
  
  //=======================
  // 1. FACTORIAL
  
  /* Inefficient recursive version of factorial.
   * This version isn't tail-recursive.
   * 
   * Ninad, 7/25/2014
   */
  def factorial_inefficient (n: Int): Int =
    if (n == 1) 1
    else n * factorial_inefficient (n -1)

  //---------------------------------------------------------------------------
  def factorial (n: Int): Int = {
    
    def fact (i: Int, accum: Int): Int =
      if (i <= 1) accum
      else fact (i -1, i * accum)
      
    fact (n, 1)
    
  } // factorial
  
  //===========================================================================
  // 2. Summing integers from low to high.
  
  /* Sum all the integers from a to b, both inclusive.
   * Inefficient, non tail-recursive method.
   */
  def sumNum_inefficient (a: Int, b: Int): Int = {
    def sumIter (x: Int, y: Int): Int =
      if (x > y) 0
      else x + sumIter (x + 1, y)
    
    if (a < b) sumIter (a, b) else sumIter (b, a)
    
  } // sumNum_Inefficient
  
  //---------------------------------------------------------------------------
  def sumNum (a: Int, b: Int): Int = {
    
    def sumIter (x: Int, y: Int, accum: Int): Int =
      if (x > y) accum
      else sumIter (x + 1, y, x + accum)
    
    if (a < b) sumIter (a, b, 0) else sumIter (b, a, 0)
    
  } // sumNum
  
  //===========================================================================
  // Testing Functions
  
  def testFactorial (n: Int) = {
    println ("Factorial")
    println (n + "! = " + factorial (n))
    println()
  }

  //---------------------------------------------------------------------------
  def testSumNum (a: Int, b: Int) = {
    println ("Sum of Integer Range")
    println ("sigma (" + a + ", " + b + ") = " + sumNum (a, b))
    println()
  }  
  
  //---------------------------------------------------------------------------
  def main (args: Array[String]) {
    
    testFactorial (5)
    testSumNum (6, 4)
    testSumNum (3, 7)
    /***
    println ("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print (col, row)
      println()
    }
    */
    
  } // main

}