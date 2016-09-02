package de.codecentric

import org.apache.spark._

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

import cats._
import cats.kernel.Monoid
import cats.instances.tuple._
import cats.kernel.instances.option._
import cats.kernel.instances.int._
import cats.syntax.semigroup._

// Value classes broken in 2.10: SI-7685
case class Min[A](value: A)  // extends AnyVal
case class Max[A](value: A)  // extends AnyVal
case class Prod[A](value: A) // extends AnyVal

object Min {
  implicit def minSemi[A: Order] = new Semigroup[Min[A]] {
    private val o = implicitly[Order[A]]
    def combine(x: Min[A], y: Min[A]): Min[A] = (x, y) match {
      case (Min(xx), Min(yy)) => Min(o.min(xx, yy))
    }
  }
}

object Max {
  implicit def maxSemi[A: Order] = new Semigroup[Max[A]] {
    private val o = implicitly[Order[A]]
    def combine(x: Max[A], y: Max[A]): Max[A] = (x, y) match {
      case (Max(xx), Max(yy)) => Max(o.max(xx, yy))
    }
  }
}

object Main {
  def mfilter[A](cond: Boolean)(m: A)(implicit M: Monoid[A]): A = if (cond) m else M.empty
  def main(args: Array[String]) = {
    def expand(word: String) = {
      (Option(Max(word.length)), Option(Min(word.length)), word.length, 1, mfilter(word.startsWith("de"))(1))
    }

    val file = "/home/markus/repos/clones/stack/README.md" // Should be some file on your system
    val conf = new SparkConf().setJars(Seq("/home/markus/src/scala/spark-sandbox/target/scala-2.10/Spark-Sandbox-assembly-0.1-SNAPSHOT.jar")).setMaster("spark://nixos:7077").setAppName("spark-cats")
    val sc = new SparkContext(conf)
    val data = sc.textFile(file).flatMap(_.split("""\s+""")).map(expand)
    val z = Monoid.empty[(Option[Max[Int]],Option[Min[Int]],Int,Int,Int)]
    val (max,min,chars,words,filterWords) = data.fold(z)(_ |+| _)
    println(s"""Finished calculation:
               |  - max word length: $max
               |  - min word length: $min
               |  - total characters: $chars
               |  - total words: $words
               |  - average word length: ${chars/words}
               |  - filtered words: ${filterWords}
               |""".stripMargin)
  }
}
