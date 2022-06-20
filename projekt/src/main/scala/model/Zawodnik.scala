package jp1.akka.lab13.model

import akka.actor.Actor

object Zawodnik {
  case object Próba
}

class Zawodnik(o: Osoba) extends Actor {
  import Zawodnik._

  override def preStart(): Unit = {
    println(s"${self.path.name}")
  }
  
  def receive: Receive = {
    case Próba => 
      val res = Utl.ocena()
      sender() ! Grupa.Wynik(res)
  }
}
