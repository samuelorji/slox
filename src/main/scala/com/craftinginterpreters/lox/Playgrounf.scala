package com.craftinginterpreters.lox

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
object Playground extends App {

  val list = (1 to 100).toList

  val iterator = list.grouped(20)

  def asyncBatchFuture[A,B](iterator : Iterator[Seq[A]])(f : A => Future[B]) : Future[Unit] = {
    if (iterator.hasNext) {
      val next = iterator.next()
      Future.traverse(next) {
        f
      }.flatMap(_ => {
        println(s"*"* 500)
        asyncBatchFuture(iterator)(f)
      })
    } else Future.unit
  }

//  val res = asyncBatchFuture(iterator) {id : Int =>
//    Future(println(s"printing $id on thread ${Thread.currentThread().getName}"))
//  }
//
//  Await.ready(res,Duration.Inf)

  Future.traverse(iterator){batches =>
    Future.traverse(batches){id =>
      Future(println(s"printing $id on thread ${Thread.currentThread().getName}"))
    }.map(_ => println(s"*"* 500))
  }

}
