package com.rokuan.autoroute

import com.rokuan.autoroute.matchers.{Route, Transformer}


object Main {
  import com.rokuan.autoroute.rules.Rule._

  def main(args: Array[String]): Unit = {
    val path = token(4) ~ (token(8)?)

    val route = path { case (a: Int) :: (b: Option[Int]) :: Nil =>
      a + b.getOrElse(0)
    } | ((token(7)+) ~ (token(2)+)) { case (a: List[Int]) :: (b: List[Int]) :: Nil =>
      a.length + b.length
    }

    val finalRoute = (route ~ token(4)) { case (a: Int) :: (b: Int) :: Nil =>
      a.toString + "/" + b.toString
    }
    val buffer: Producer[Int] = 7 :: 7 :: 2 :: 4 :: Nil
    val v = new Route(finalRoute)(buffer)
    println(v)
  }
}
