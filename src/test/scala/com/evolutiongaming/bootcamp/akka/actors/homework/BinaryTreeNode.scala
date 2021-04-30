package com.evolutiongaming.bootcamp.akka.actors.homework

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import com.evolutiongaming.bootcamp.akka.actors.homework.BinaryTreeSet.ExtraOperation.CopyChildren
import com.evolutiongaming.bootcamp.akka.actors.homework.BinaryTreeSet.ExtraOperationReply.CopyChildrenFinished
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

  override def receive: Receive = working

  private def working: Receive = {
    case insert:   Insert       => doInsert(insert)
    case contains: Contains     => doContains(contains)
    case remove:   Remove       => doRemove(remove)
    case copy:     CopyChildren => doCopyChildren(copy)
  }

  private def copying(expected: Set[ActorRef], inserted: Boolean): Receive = {
    case OperationFinished(Int.MinValue) => tryFinishCopyChildren(expected, isInserted = true)
    case CopyChildrenFinished            => tryFinishCopyChildren(expected - sender(), inserted)
  }

  private def nextPosition(m: Operation): Position = {
    if (m.elem > elem) Right else Left
  }

  private def forwardOperation(m: Operation)(ifEmpty: => Unit): Unit = {
    subtrees.get(nextPosition(m)).fold(ifEmpty)(_ ! m)
  }

  private def foldOperation(m: Operation)(ifEmpty: => Unit)(ifMatch: => Unit): Unit = {
    if (m.elem == elem) ifMatch
    else forwardOperation(m)(ifEmpty)
  }

  private def finishOperation(m: Operation): Unit = {
    m.requester ! OperationFinished(m.id)
  }

  private def doInsert(m: Insert): Unit = {
    foldOperation(m) {
      val actorRef = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
      subtrees += (nextPosition(m) -> actorRef)
      finishOperation(m)
    } {
      removed = false
      finishOperation(m)
    }
  }

  private def doContains(m: Contains): Unit = {
    foldOperation(m) {
      m.requester ! ContainsResult(m.id, result = false)
    } {
      m.requester ! ContainsResult(m.id, !removed)
    }
  }

  private def doRemove(m: Remove): Unit = {
    foldOperation(m) {
      finishOperation(m)
    } {
      removed = true
      finishOperation(m)
    }
  }

  private def doCopyChildren(m: CopyChildren): Unit = {
    val children = subtrees.values.toSet

    if (!removed) {
      m.newRoot ! Insert(self, Int.MinValue, elem)
    }

    children.foreach(_ ! CopyChildren(m.newRoot))
    tryFinishCopyChildren(children, isInserted = removed) // don't wait for insertion of removed node
  }

  private def tryFinishCopyChildren(expectedChildren: Set[ActorRef], isInserted: Boolean): Unit = {
    if (expectedChildren.isEmpty && isInserted) {
      context.parent ! CopyChildrenFinished
      self ! PoisonPill
    } else {
      context.become(copying(expectedChildren, isInserted))
    }
  }
}
