package com.rokuan.autoroute

import com.rokuan.autoroute.matchers.Route


object Main {
  import com.rokuan.autoroute.rules.Rule._

  def main(args: Array[String]): Unit = {
    val path = token(4) ~ (token(8)?)
    val branch = path { case (a: Int) :: (b: Option[Int]) :: Nil =>
      a + b.getOrElse(0)
    }
    val buffer = +::(4, +::(8, PNil))
    val v = new Route(List(branch))(buffer)
    println(v)
  }
}
