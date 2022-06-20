package jp1.akka.lab13.model

import akka.actor._
import Sorting._

val akkaPathAllowedChars = ('a' to 'z').toSet union
  ('A' to 'Z').toSet union
  "-_.*$+:@&=,!~';.)".toSet

object Organizator {
  case object Start
  case object Eliminacje
  case object Finał
  case object Runda
  case object Wyniki
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  case class WynikiKońcowe(w: Map[ActorRef, Option[Ocena]])
  case object Stop
}

class Organizator extends Actor {
  import Organizator._

  def receive: Receive = {
    case Start =>
      val zawodnicy = List.fill(50) {
        val o = Utl.osoba()
        context.actorOf(Props(Zawodnik(o)), s"${o.imie}-${o.nazwisko}" filter akkaPathAllowedChars)
      }

      val grupa = context.actorOf(Props(Grupa(zawodnicy)), "grupa")
      context.become(qualifications(grupa))

    case Wyniki => 
      println("Kwalifikacje jeszcze się nie odbyły :(")
  }

  def qualifications(grupa: ActorRef): Receive = {

    case Eliminacje => 
      grupa ! Grupa.Runda

    case Wyniki => 
      println("Kwalifikacje jeszcze się nie odbyły :(")
    
    case WynikiKońcowe(n) => 
      
      val res2 = n.toList
      .sortBy(x => x._2.toList.map(x => List(x._1, x._2, x._3)).flatten.sum)
      .map(x => (x._1, x._2.toList.map(x => List(x._1, x._2, x._3)).flatten))
      .reverse
      .take(20)
      
      val deads = n.toList.filter(x => res2.contains(x))

      val res = finalSorting(res2)

      val toSend = res2.map(x => x._1)

      val grupafinałowa = context.actorOf(Props(Grupa(toSend)), "grupafinalowa")

      println("Koniec eliminacji! Można zaczynać finał :) Lub zakończyć używając stop :(")

      context.become(finale(res2, grupafinałowa, res))
      
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }

  def finale(finalisci: List[(ActorRef, List[Int])], group: ActorRef, wynikiPoElem: String): Receive = {

    case Finał => 
      group ! Grupa.Runda

    case Wyniki => 
      println(s"Wyniki po kwalifikacjach")
      println(wynikiPoElem)
    
    case WynikiKońcowe(n) =>

      println("Koniec zawodów!")

      val res1 = n.toList.map(x => (x._1, x._2.toList.map(x => List(x._1, x._2, x._3)).flatten))
      val resp = res1 ::: finalisci

      val res2 = resp.groupBy(x => x._1)
      .toList
      .map(x => (x._1, x._2.map(y => y._2.toList).flatten))
      .filter(x => x._2.size == 6)
      .map(x => (x._1, List(x._2(0) + x._2(3), x._2(1) + x._2(4), x._2(2) + x._2(5))))

      val res = finalSorting(res2)

      context.become(afterFinale(res))
          
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }

  def afterFinale(wynikiPoFinale: String): Receive = {
    case Wyniki => 
      println("WYNIKI PO FINALE")
      println(wynikiPoFinale)
    
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }
}
