package com.rokuan.autoroute

import com.rokuan.autoroute.matchers.Route


object Main {
  import com.rokuan.autoroute.rules.Rule._

  def main(args: Array[String]): Unit = {
    val path = token(4) ~ opt(token(8))

    val route = path { case List(a: Int, b: Option[Int]) =>
      a + b.getOrElse(0)
    } | (list(token(7)) ~ list(token(2))) { case List(a: List[Int], (b: List[Int])) =>
      a.length + b.length
    }

    val finalRoute = (token(8) ~ route ~ token(4)) { case List(a: Int, b: Int, c: Int) =>
      a + "/" + b + "/" + c
    }
    val buffer = List(8, 7, 7, 2, 4)
    val v = new Route(finalRoute)(buffer)
    println(v)
  }
}
