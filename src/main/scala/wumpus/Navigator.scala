package wumpus

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import Navigator.{ActionRequest, Breeze, Bump, Glitter, Scream, Stench}

class Navigator {


  private def parseMessage(messageText: String): WumpusPercept = {
    val sentences = messageText.split("\\. ")

    val percepts = sentences.map(parseSentence)

    var stench: Boolean = false
    var glitter: Boolean = false
    var breeze: Boolean = false
    var scream: Boolean = false
    var bump: Boolean = false

    if (percepts.contains(Stench)) stench = true
    if (percepts.contains(Glitter)) glitter = true
    if (percepts.contains(Breeze)) breeze = true
    if (percepts.contains(Scream)) scream = true
    if (percepts.contains(Bump)) bump = true

    WumpusPercept(glitter, stench, breeze, bump, scream)
  }

  private def parseSentence(str: String): Navigator.Percept = {
    if (str.contains("breeze")) Breeze
    else if (str.contains("stench")) Stench
    else if (str.contains("glitter")) Glitter
    else if (str.contains("scream")) Scream
    else if (str.contains("bump")) Bump
    else throw new RuntimeException("Perception string must be about the state of the cave!")
  }

  def navigatorActor: Behavior[ActionRequest] = Behaviors.receive((context, message) => {

//    val messageText = message.message
//    val percept = parseMessage(messageText)
//    context.log.atInfo().log("Got request to get the next action based on perception")
    Console.println("(НАВ) Навигатор задачу принял, ща придумаю, что делать")
    val percept = message.wumpusPercept
//    Console.println(percept)
    val action = Pathfinder.calculateAction(percept)

    message.sender ! Navigator.ActionResponse(action._2, action._1)
    Behaviors.same
  })
}


object Navigator {
  case class ActionRequest(wumpusPercept: WumpusPercept, message: String, sender: ActorRef[ActionResponse])
  case class ActionResponse(action: SpeleologistAction, look: Look)

  sealed trait Percept

  case object Stench extends Percept
  case object Breeze extends Percept
  case object Scream extends Percept
  case object Glitter extends Percept
  case object Bump extends Percept
}