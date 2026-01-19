package zio.blocks.schema

import org.bson.types.ObjectId
import zio.blocks.schema.Modifier

package object bson {
  val ObjectIdTag: String = "$oid"
  private[bson] val ObjectIdConfigKey: String = "bson.objectId"

  implicit val ObjectIdSchema: Schema[ObjectId] = {
    val base     = Schema[String].wrapTotal(new ObjectId(_), _.toHexString)
    val typeName = new TypeName[ObjectId](new Namespace(Seq("org", "bson", "types")), "ObjectId")
    new Schema(base.reflect.typeName(typeName)).modifier(Modifier.config(ObjectIdConfigKey, "true"))
  }
}
