package com.evolutiongaming.bootcamp.akka.actors.homework

import akka.actor.{Actor, ActorRef, Props}
import com.evolutiongaming.bootcamp.akka.actors.homework.BinaryTreeSet.Operation
import com.evolutiongaming.bootcamp.akka.actors.homework.BinaryTreeSet.OperationReply.{
  ContainsResult,
  OperationFinished
}

object BinaryTreeNode {
  sealed private trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._

  private var subtrees = Map[Position, ActorRef]()
  private var removed  = initiallyRemoved

  override def receive: Receive = {
    case insert:   Insert   => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove:   Remove   => doRemove(remove)
  }

  private def nextPosition(m: Operation): Position = {
    if (m.elem > elem) Right else Left
  }

  private def forward(m: Operation, ifEmpty: => Unit): Unit = {
    subtrees.get(nextPosition(m)).fold(ifEmpty)(_ ! m)
  }

  private def fold(m: Operation)(ifEmpty: => Unit)(ifMatch: => Unit): Unit = {
    if (m.elem == elem) ifMatch
    else forward(m, ifEmpty)
  }

  private def finish(m: Operation): Unit = {
    m.requester ! OperationFinished(m.id)
  }

  private def doInsert(m: Insert): Unit = {
    fold(m) {
      val actorRef = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
      subtrees += (nextPosition(m) -> actorRef)
      finish(m)
    } {
      removed = false
      finish(m)
    }
  }

  private def doContains(m: Contains): Unit = {
    fold(m) {
      m.requester ! ContainsResult(m.id, result = false)
    } {
      m.requester ! ContainsResult(m.id, !removed)
    }
  }

  private def doRemove(m: Remove): Unit = {
    fold(m) {
      finish(m)
    } {
      removed = true
      finish(m)
    }
  }
}
