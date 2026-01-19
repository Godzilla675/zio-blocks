package zio.blocks.schema.bson

import zio.blocks.schema.Modifier

import scala.annotation.meta.field

@field final case class bsonField(name: String) extends Modifier.Term

final case class bsonHint(name: String) extends Modifier.Term

final case class bsonDiscriminator(name: String) extends Modifier.Reflect

final class bsonNoExtraFields extends Modifier.Reflect

@field final class bsonExclude extends Modifier.Term
