package zio.blocks.schema.annotation

import zio.blocks.schema.Modifier

import scala.annotation.meta.field

final case class caseName(name: String) extends Modifier.Term

final case class caseNameAliases(aliases: String*) extends Modifier.Term

@field final case class fieldName(name: String) extends Modifier.Term

@field final case class fieldNameAliases(aliases: String*) extends Modifier.Term

@field final case class transientField() extends Modifier.Term

final case class transientCase() extends Modifier.Term

final case class discriminatorName(tag: String) extends Modifier.Reflect

final class noDiscriminator extends Modifier.Reflect

final class rejectExtraFields extends Modifier.Reflect
