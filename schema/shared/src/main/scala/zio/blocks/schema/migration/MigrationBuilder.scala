package zio.blocks.schema.migration

import zio.blocks.schema._

/**
 * A builder for constructing [[Migration]] instances.
 *
 * The builder provides a fluent API for defining migration actions using
 * selector expressions. In the full implementation, selector-accepting methods
 * would be implemented via macros that:
 *  1. Inspect the selector expression
 *  2. Validate it is a supported projection
 *  3. Convert it to a [[DynamicOptic]]
 *  4. Store the optic in the migration action
 *
 * This implementation provides runtime versions of these methods that accept
 * [[DynamicOptic]] paths directly. The macro layer can be added for compile-time
 * selector validation.
 *
 * @tparam A the source type
 * @tparam B the target type
 */
class MigrationBuilder[A, B](
  val sourceSchema: Schema[A],
  val targetSchema: Schema[B],
  val actions: Vector[MigrationAction]
) {

  // ==================== Record Operations ====================

  /**
   * Add a field to the target record.
   *
   * @param path    path to the new field (typically _.fieldName)
   * @param default expression producing the default value
   */
  def addField(path: DynamicOptic, default: DynamicSchemaExpr): MigrationBuilder[A, B] = {
    val (parentPath, fieldName) = splitPath(path)
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.AddField(parentPath, fieldName, default)
    )
  }

  /**
   * Add a field with a literal default value.
   */
  def addField[T](path: DynamicOptic, default: T)(implicit schema: Schema[T]): MigrationBuilder[A, B] = {
    val defaultValue = schema.toDynamicValue(default)
    addField(path, DynamicSchemaExpr.Literal(defaultValue))
  }

  /**
   * Drop a field from the source record.
   *
   * @param path path to the field to drop
   * @param defaultForReverse expression producing value when reversing (optional)
   */
  def dropField(
    path: DynamicOptic,
    defaultForReverse: DynamicSchemaExpr = DynamicSchemaExpr.DefaultValue
  ): MigrationBuilder[A, B] = {
    val (parentPath, fieldName) = splitPath(path)
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.DropField(parentPath, fieldName, defaultForReverse)
    )
  }

  /**
   * Rename a field.
   *
   * @param from path to the source field
   * @param to   path to the target field (must be at same level as source)
   */
  def renameField(from: DynamicOptic, to: DynamicOptic): MigrationBuilder[A, B] = {
    val (parentPath, fromName) = splitPath(from)
    val (_, toName) = splitPath(to)
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.RenameField(parentPath, fromName, toName)
    )
  }

  /**
   * Rename a field using string names.
   */
  def renameField(from: String, to: String): MigrationBuilder[A, B] =
    renameField(DynamicOptic.root.field(from), DynamicOptic.root.field(to))

  /**
   * Transform a field value.
   *
   * @param path      path to the field
   * @param transform expression to transform the value
   * @param reverseTransform expression to reverse the transform (for reverse migration)
   */
  def transformField(
    path: DynamicOptic,
    transform: DynamicSchemaExpr,
    reverseTransform: DynamicSchemaExpr = DynamicSchemaExpr.DefaultValue
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.TransformValue(path, transform, reverseTransform)
    )

  /**
   * Make an optional field mandatory.
   *
   * @param path    path to the optional field
   * @param default expression producing value when None
   */
  def mandateField(
    path: DynamicOptic,
    default: DynamicSchemaExpr
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.Mandate(path, default)
    )

  /**
   * Make an optional field mandatory with a literal default.
   */
  def mandateField[T](path: DynamicOptic, default: T)(implicit schema: Schema[T]): MigrationBuilder[A, B] =
    mandateField(path, DynamicSchemaExpr.Literal(schema.toDynamicValue(default)))

  /**
   * Make a mandatory field optional.
   *
   * @param path path to the field
   */
  def optionalizeField(path: DynamicOptic): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.Optionalize(path)
    )

  /**
   * Change the type of a field (primitive-to-primitive only).
   *
   * @param path             path to the field
   * @param converter        expression to convert to the new type
   * @param reverseConverter expression to convert back
   */
  def changeFieldType(
    path: DynamicOptic,
    converter: DynamicSchemaExpr,
    reverseConverter: DynamicSchemaExpr = DynamicSchemaExpr.DefaultValue
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.ChangeType(path, converter, reverseConverter)
    )

  /**
   * Change a field from one primitive type to another using type names.
   */
  def changeFieldType(path: DynamicOptic, toType: String, fromType: String): MigrationBuilder[A, B] =
    changeFieldType(
      path,
      DynamicSchemaExpr.CoercePrimitive(DynamicSchemaExpr.Path(path), toType),
      DynamicSchemaExpr.CoercePrimitive(DynamicSchemaExpr.Path(path), fromType)
    )

  /**
   * Join multiple fields into one.
   *
   * @param target      path to the target combined field
   * @param sourcePaths paths to source fields
   * @param combiner    expression to combine source values
   * @param splitter    expression to split back (for reverse)
   */
  def joinFields(
    target: DynamicOptic,
    sourcePaths: Vector[DynamicOptic],
    combiner: DynamicSchemaExpr,
    splitter: DynamicSchemaExpr
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.Join(target, sourcePaths, combiner, splitter)
    )

  /**
   * Split a field into multiple fields.
   *
   * @param source     path to the source field
   * @param targetPaths paths to target fields
   * @param splitter   expression to split the source value
   * @param combiner   expression to combine back (for reverse)
   */
  def splitField(
    source: DynamicOptic,
    targetPaths: Vector[DynamicOptic],
    splitter: DynamicSchemaExpr,
    combiner: DynamicSchemaExpr
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.Split(source, targetPaths, splitter, combiner)
    )

  // ==================== Enum Operations ====================

  /**
   * Rename a case in a variant/enum.
   *
   * @param from original case name
   * @param to   new case name
   */
  def renameCase(from: String, to: String): MigrationBuilder[A, B] =
    renameCaseAt(DynamicOptic.root, from, to)

  /**
   * Rename a case in a variant at a specific path.
   */
  def renameCaseAt(path: DynamicOptic, from: String, to: String): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.RenameCase(path, from, to)
    )

  /**
   * Transform the structure within a specific case.
   *
   * @param caseName       name of the case to transform
   * @param caseMigration  function that configures the nested migration
   */
  def transformCase(
    caseName: String,
    caseMigration: MigrationBuilder[Any, Any] => MigrationBuilder[Any, Any]
  ): MigrationBuilder[A, B] =
    transformCaseAt(DynamicOptic.root, caseName, caseMigration)

  /**
   * Transform a case at a specific path.
   */
  def transformCaseAt(
    path: DynamicOptic,
    caseName: String,
    caseMigration: MigrationBuilder[Any, Any] => MigrationBuilder[Any, Any]
  ): MigrationBuilder[A, B] = {
    // Create an empty builder and let the user configure it
    val emptyBuilder = new MigrationBuilder[Any, Any](
      Schema.dynamicValue.asInstanceOf[Schema[Any]],
      Schema.dynamicValue.asInstanceOf[Schema[Any]],
      Vector.empty
    )
    val configuredBuilder = caseMigration(emptyBuilder)
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.TransformCase(path, caseName, configuredBuilder.actions)
    )
  }

  // ==================== Collection Operations ====================

  /**
   * Transform all elements in a sequence.
   *
   * @param path      path to the sequence
   * @param transform expression to apply to each element
   * @param reverseTransform expression to reverse the transform
   */
  def transformElements(
    path: DynamicOptic,
    transform: DynamicSchemaExpr,
    reverseTransform: DynamicSchemaExpr = DynamicSchemaExpr.DefaultValue
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.TransformElements(path, transform, reverseTransform)
    )

  // ==================== Map Operations ====================

  /**
   * Transform all keys in a map.
   *
   * @param path      path to the map
   * @param transform expression to apply to each key
   * @param reverseTransform expression to reverse the transform
   */
  def transformKeys(
    path: DynamicOptic,
    transform: DynamicSchemaExpr,
    reverseTransform: DynamicSchemaExpr = DynamicSchemaExpr.DefaultValue
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.TransformKeys(path, transform, reverseTransform)
    )

  /**
   * Transform all values in a map.
   *
   * @param path      path to the map
   * @param transform expression to apply to each value
   * @param reverseTransform expression to reverse the transform
   */
  def transformValues(
    path: DynamicOptic,
    transform: DynamicSchemaExpr,
    reverseTransform: DynamicSchemaExpr = DynamicSchemaExpr.DefaultValue
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(
      sourceSchema,
      targetSchema,
      actions :+ MigrationAction.TransformValues(path, transform, reverseTransform)
    )

  // ==================== Build Methods ====================

  /**
   * Build the migration with full validation.
   *
   * In the full implementation, this performs macro-level validation to ensure
   * that applying the migration actions to the source schema structure produces
   * the target schema structure.
   *
   * Current implementation delegates to `buildPartial`.
   */
  def build: Migration[A, B] =
    // TODO: Add full validation that simulates actions on source schema
    // and verifies the result matches target schema
    buildPartial

  /**
   * Build the migration without full structural validation.
   *
   * Use this when you want to skip validation, such as for partial migrations
   * or when validation is too restrictive.
   */
  def buildPartial: Migration[A, B] =
    new Migration(new DynamicMigration(actions), sourceSchema, targetSchema)

  // ==================== Helper Methods ====================

  /**
   * Split a path into parent path and field name.
   */
  private def splitPath(path: DynamicOptic): (DynamicOptic, String) = {
    val nodes = path.nodes
    if (nodes.isEmpty) {
      (DynamicOptic.root, "")
    } else {
      nodes.last match {
        case DynamicOptic.Node.Field(name) =>
          (new DynamicOptic(nodes.dropRight(1)), name)
        case other =>
          // For non-field nodes, use a generated name
          (new DynamicOptic(nodes.dropRight(1)), s"node_${nodes.length}")
      }
    }
  }
}

object MigrationBuilder {

  /**
   * Create a new builder for migrating from A to B.
   */
  def apply[A, B](implicit
    sourceSchema: Schema[A],
    targetSchema: Schema[B]
  ): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, Vector.empty)

  /**
   * Convenient syntax for creating paths.
   */
  object paths {
    def field(name: String): DynamicOptic = DynamicOptic.root.field(name)
    def field(names: String*): DynamicOptic = names.foldLeft(DynamicOptic.root)(_.field(_))
    def elements: DynamicOptic = DynamicOptic.elements
    def mapKeys: DynamicOptic = DynamicOptic.mapKeys
    def mapValues: DynamicOptic = DynamicOptic.mapValues
  }

  /**
   * Convenient syntax for creating expressions.
   */
  object exprs {
    def literal[T](value: T)(implicit schema: Schema[T]): DynamicSchemaExpr =
      DynamicSchemaExpr.Literal(schema.toDynamicValue(value))

    def path(optic: DynamicOptic): DynamicSchemaExpr =
      DynamicSchemaExpr.Path(optic)

    def path(fieldName: String): DynamicSchemaExpr =
      DynamicSchemaExpr.Path(DynamicOptic.root.field(fieldName))

    def concat(left: DynamicSchemaExpr, right: DynamicSchemaExpr): DynamicSchemaExpr =
      DynamicSchemaExpr.StringConcat(left, right)

    def defaultValue: DynamicSchemaExpr =
      DynamicSchemaExpr.DefaultValue

    def coerce(expr: DynamicSchemaExpr, targetType: String): DynamicSchemaExpr =
      DynamicSchemaExpr.CoercePrimitive(expr, targetType)
  }
}
