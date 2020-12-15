package wumpus

trait SpeleologistAction

case object Climb extends SpeleologistAction
case object Forward extends SpeleologistAction
case object Grab extends SpeleologistAction
case object Shoot extends SpeleologistAction

trait Look
case object LookUp extends Look
case object LookDown extends Look
case object LookLeft extends Look
case object LookRight extends Look
