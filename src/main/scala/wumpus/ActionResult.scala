package wumpus

trait ActionResult

case object KeepGoing extends ActionResult
case object GotGold extends ActionResult
case object AgentDied extends ActionResult
