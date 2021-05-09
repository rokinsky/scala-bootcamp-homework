package com.evolutiongaming.bootcamp.akka.actors.homework

import akka.actor._
import com.evolutiongaming.bootcamp.akka.actors.homework.BinaryTreeSet.ExtraOperation.{CopyChildren, GarbageCollection}
import com.evolutiongaming.bootcamp.akka.actors.homework.BinaryTreeSet.ExtraOperationReply.CopyChildrenFinished

object BinaryTreeSet {

  sealed trait Operation {
    def requester: ActorRef
    def id:        Int
    def elem:      Int
  }

  // requests with identifier `id`
  // `requester` should be notified when an operation is completed.
  object Operation {
    // insert an element `elem` into the tree.
    final case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

    // check whether an element `elem` is present in the tree
    final case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

    // remove the element `elem` from the tree
    final case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation
  }

  sealed trait OperationReply {
    def id: Int
  }

  object OperationReply {
    // answer to the Contains request with identifier `id`.
    // `result` is true if and only if the element is present in the tree
    final case class ContainsResult(id: Int, result: Boolean) extends OperationReply

    // successful completion of an insert or remove operation
    final case class OperationFinished(id: Int) extends OperationReply
  }

  trait ExtraOperation
  object ExtraOperation {
    final case object GarbageCollection extends ExtraOperation
    final case class CopyChildren(newRoot: ActorRef) extends ExtraOperation
  }

  sealed trait ExtraOperationReply
  object ExtraOperationReply {
    final case object CopyChildrenFinished extends ExtraOperationReply
  }
}

final class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._

  def receive: Receive = working(createRoot)

  private def working(root: ActorRef): Receive = {
    case GarbageCollection =>
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyChildren(newRoot)

    case m: Operation => root ! m
  }

  private def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyChildrenFinished =>
      unstashAll()
      context.become(working(newRoot))

    case _: Operation =>
      stash()
  }

  private def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
}
