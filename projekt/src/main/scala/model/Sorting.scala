package jp1.akka.lab13.model

import akka.actor.ActorRef


object Sorting {

    def comp(a: (ActorRef, List[Int]), b: (ActorRef, List[Int])): Boolean = {
        if (a._2.sum > b._2.sum) true
        else if (a._2.sum == b._2.sum && a._2(0) > b._2(0) && a._2(2) <= a._2(2)) true
        else if (a._2.sum == b._2.sum && a._2(0) > b._2(0) && a._2(2) > a._2(2)) true
        else if (a._2.sum == b._2.sum && a._2(0) <= b._2(0) && a._2(2) > a._2(2)) true
        else false
    }

    def finalSorting(a: List[(ActorRef, List[Int])]): String = {
    a.sortWith(comp).zipWithIndex.map(x => (x._1, x._2 + 1))
        .groupBy(x => x._1._2).toList.map(x => x._2).map(x => 
        if (x.size > 1) {
        val c = x.sortBy(x => x._2)
        val min = x(0)._2
        x.map(y => (y._1, min))

        } else {
        x
        })
        .flatten
        .sortBy(x => x._2)
        .map(x => (s"Miejsce ${x._2}.", x._1._1.path.name, x._1._2, x._1._2.sum))
        .mkString("\n")

    }
}