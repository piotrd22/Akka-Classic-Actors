package jp1.akka.lab13.model

import akka.actor._

object Grupa {
  case object Runda
  case class WynikiKońcowe(w: Map[ActorRef, Option[Ocena]])
  case class Wynik(ocena: Option[Ocena])
  case object Koniec
}
class Grupa(zawodnicy: List[ActorRef]) extends Actor {
  import Grupa._
  def receive: Receive = {
    case Runda => 
      zawodnicy.foreach(x => x ! Zawodnik.Próba)
      context.become(nextStep(zawodnicy, List(), zawodnicy.size, sender()))
  }

  def nextStep(zawodnicy: List[ActorRef], best: List[(ActorRef, Option[Ocena])], licznik: Int, szefu: ActorRef): Receive = {
    case Wynik(ocena) if (ocena == None && licznik > 1) =>
      context.become(nextStep(zawodnicy.filter(x => x != sender()), best, licznik - 1, szefu))

    case Wynik(ocena) if (ocena != None && licznik > 1) => 
      context.become(nextStep(zawodnicy, (sender(), ocena) :: best, licznik - 1, szefu))

    case Wynik(ocena) if (ocena == None && licznik == 1) =>
      context.become(nextStep(zawodnicy.filter(x => x != sender()), best, licznik - 1, szefu))
      self ! Koniec

    case Wynik(ocena) if (ocena != None && licznik == 1) => 
      context.become(nextStep(zawodnicy, (sender(), ocena) :: best, licznik - 1, szefu))
      self ! Koniec
    
    case Koniec => 
      szefu ! Organizator.WynikiKońcowe(best.toMap)
  }
}
