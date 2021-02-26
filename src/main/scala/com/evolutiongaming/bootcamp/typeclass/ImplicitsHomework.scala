package com.evolutiongaming.bootcamp.typeclass

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {

  /** Lo and behold! Brand new super-useful collection library for Scala!
    *
    * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
    * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
    * of the data stored.
    *
    * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
    * a thing called size score. Its calculation rules:
    * - size score of a Byte is 1
    * - Int - 4 (as primitive JVM int consists of 4 bytes)
    * - Long - 8
    * - Char - 2 (one UTF-16 symbol is 2 bytes)
    * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
    * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
    * the fields
    * - score for any sequence (Array[T], List[T], Vector[T]) is
    * 12 (our old friend object header) + sum of scores of all elements
    * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
    */
  object SuperVipCollections4s {
    type SizeScore = Int
    val ByteSize         = 1
    val CharSize         = 2
    val IntSize          = 4
    val LongSize         = 8
    val ObjectHeaderSize = 12

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[T: GetSizeScore]: GetSizeScore[T] = implicitly
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = GetSizeScore[T].apply(inner)
      }
    }

    /** Mutable key-value cache which limits the size score of the data scored.
      *
      * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
      * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
      * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
      * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
      *
      * @param maxSizeScore max size score for the stored data
      * @tparam K key type
      * @tparam V value type
      */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import instances._
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      private def currentSizeScore: SizeScore =
        map.sizeScore - map.empty.sizeScore

      private def canPut(key: K, value: V): Boolean =
        currentSizeScore + key.sizeScore + value.sizeScore <= maxSizeScore

      private def putOption(key: K, value: V): Option[map.type] =
        Option.when(canPut(key, value))(map += (key -> value))

      private def pop(): Option[map.type] =
        map.keys.headOption.map(map -= _)

      // FIXME: Which version is better? This one is cleaner but without tailrec optimization
      def put(key: K, value: V): Unit =
        putOption(key, value).orElse(pop().map(_ => put(key, value)))

      @tailrec
      def put2(key: K, value: V): Unit =
        if (putOption(key, value).isEmpty && pop().nonEmpty) put2(key, value)

      def get(key: K): Option[V] = map.get(key)
    }

    /** Cool custom immutable multi-map collection - does not extend the standard library collection types
      * (yes, this is a feature)
      */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /** Type-class allowing us to iterate over different "collection-like" types with one type arg
      */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      def apply[F[_]: Iterate]: Iterate[F] = implicitly
    }

    /** Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
      */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      def apply[F[_, _]: Iterate2]: Iterate2[F] = implicitly
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val mutableMapIterate2: Iterate2[mutable.Map] = new Iterate2[mutable.Map] {
        override def iterator1[T, S](f: mutable.Map[T, S]): Iterator[T] = f.keysIterator
        override def iterator2[T, S](f: mutable.Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.toMap.keysIterator
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.toMap.valuesIterator
      }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */
      implicit val byteGetSizeScore:   GetSizeScore[Byte]   = _ => ByteSize
      implicit val charGetSizeScore:   GetSizeScore[Char]   = _ => CharSize
      implicit val intGetSizeScore:    GetSizeScore[Int]    = _ => IntSize
      implicit val longGetSizeScore:   GetSizeScore[Long]   = _ => LongSize
      implicit val stringGetSizeScore: GetSizeScore[String] = s => ObjectHeaderSize + s.length * CharSize

      implicit def iterableGetSizeScore[F[_]: Iterate, T: GetSizeScore]: GetSizeScore[F[T]] = ft =>
        ObjectHeaderSize +
          Iterate[F].iterator(ft).map(_.sizeScore).sum

      implicit def iterable2GetSizeScore[F[_, _]: Iterate2, T: GetSizeScore, S: GetSizeScore]: GetSizeScore[F[T, S]] =
        fts =>
          ObjectHeaderSize +
            Iterate2[F].iterator1(fts).map(_.sizeScore).sum +
            Iterate2[F].iterator2(fts).map(_.sizeScore).sum
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
      id:         Long,
      userId:     Int,
      hashTags:   Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes:   List[FbiNote],
    )

    final case class FbiNote(
      month:                 String,
      favouriteChar:         Char,
      watchedPewDiePieTimes: Long,
    )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id:   Long): Option[Twit]
    }

    implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] = {
      case FbiNote(month, favouriteChar, watchedPewDiePieTimes) =>
        ObjectHeaderSize + month.sizeScore + favouriteChar.sizeScore + watchedPewDiePieTimes.sizeScore
    }

    // format: off
    implicit val twitGetSizeScore: GetSizeScore[Twit] = {
      case Twit(id, userId, hashTags, attributes, fbiNotes) =>
        ObjectHeaderSize + id.sizeScore + userId.sizeScore + hashTags.sizeScore + attributes.sizeScore + fbiNotes.sizeScore
    }
    // format: on

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val mutableCache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit         = mutableCache.put(twit.id, twit)
      override def get(id:   Long): Option[Twit] = mutableCache.get(id)
    }
  }
}
