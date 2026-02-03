package zio.blocks.schema.migration

import zio.blocks.schema._
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Scala 2 implicit class extension for [[MigrationBuilder]] that provides
 * type-safe selector syntax.
 *
 * These extensions allow using lambda expressions like `_.fieldName` instead of
 * constructing [[DynamicOptic]] paths manually.
 *
 * Usage:
 * {{{
 * import MigrationBuilderSyntax._
 *
 * MigrationBuilder[PersonV1, PersonV2]
 *   .addField[Int](_.age, 0)
 *   .renameField(_.firstName, _.givenName)
 *   .dropField(_.middleName)
 *   .build
 * }}}
 */
object MigrationBuilderSyntax {

  // ==================== Selector-only Syntax ====================
  //
  // These implicit classes exist solely to support the selector syntax
  // consumed by `SelectorMacros`. They intentionally fail if evaluated outside
  // of selector macros.

  implicit class ValueSelectorOps[A](private val a: A) extends AnyVal {
    @compileTimeOnly("Can only be used inside migration selector macros")
    def when[B <: A]: B = ???

    @compileTimeOnly("Can only be used inside migration selector macros")
    def wrapped[B]: B = ???
  }

  implicit class SequenceSelectorOps[C[_], A](private val c: C[A]) extends AnyVal {
    @compileTimeOnly("Can only be used inside migration selector macros")
    def at(index: Int): A = ???

    @compileTimeOnly("Can only be used inside migration selector macros")
    def atIndices(indices: Int*): A = ???

    @compileTimeOnly("Can only be used inside migration selector macros")
    def each: A = ???
  }

  implicit class MapSelectorOps[M[_, _], K, V](private val m: M[K, V]) extends AnyVal {
    @compileTimeOnly("Can only be used inside migration selector macros")
    def atKey(key: K): V = ???

    @compileTimeOnly("Can only be used inside migration selector macros")
    def atKeys(keys: K*): V = ???

    @compileTimeOnly("Can only be used inside migration selector macros")
    def eachKey: K = ???

    @compileTimeOnly("Can only be used inside migration selector macros")
    def eachValue: V = ???
  }

  // ==================== Builder Syntax ====================

  implicit class MigrationBuilderOps[A, B](private val builder: MigrationBuilder[A, B]) extends AnyVal {

    /**
     * Add a field with a type-safe selector and literal default.
     */
    def addField[T](selector: B => T, default: T)(implicit schema: Schema[T]): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.addFieldImpl[A, B, T]

    /**
     * Add a field with a type-safe selector and expression default.
     */
    def addFieldExpr[T](selector: B => T, default: DynamicSchemaExpr): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.addFieldExprImpl[A, B, T]

    /**
     * Drop a field using a type-safe selector.
     */
    def dropField[T](selector: A => T): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.dropFieldImpl[A, B, T]

    /**
     * Rename a field using type-safe selectors.
     */
    def renameField[T, U](from: A => T, to: B => U): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.renameFieldImpl[A, B, T, U]

    /**
     * Transform a field using a type-safe selector.
     */
    def transformField[T](selector: A => T, transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.transformFieldImpl[A, B, T]

    /**
     * Make an optional field mandatory with type-safe selector.
     */
    def mandateField[T](selector: B => T, default: T)(implicit schema: Schema[T]): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.mandateFieldImpl[A, B, T]

    /**
     * Make a mandatory field optional using a type-safe selector.
     */
    def optionalizeField[T](selector: A => T): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.optionalizeFieldImpl[A, B, T]

    /**
     * Transform all elements in a sequence field.
     */
    def transformElements[T](selector: A => Seq[T], transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.transformElementsImpl[A, B, T]

    /**
     * Transform map keys using type-safe selector.
     */
    def transformKeys[K, V](selector: A => Map[K, V], transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.transformKeysImpl[A, B, K, V]

    /**
     * Transform map values using type-safe selector.
     */
    def transformValues[K, V](selector: A => Map[K, V], transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
      macro MigrationBuilderSyntaxImpl.transformValuesImpl[A, B, K, V]
  }
}

class MigrationBuilderSyntaxImpl(val c: blackbox.Context) {
  import c.universe._

  private val selectorMacrosImpl = new SelectorMacrosImpl(c)

  def addFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[B => T],
    default: c.Expr[T]
  )(schema: c.Expr[Schema[T]]): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[B, T](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.addField[$T]($optic, $default)($schema)"
    )
  }

  def addFieldExprImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[B => T],
    default: c.Expr[DynamicSchemaExpr]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[B, T](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.addField($optic, $default)"
    )
  }

  def dropFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[A => T]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[A, T](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.dropField($optic)"
    )
  }

  def renameFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag, U: WeakTypeTag](
    from: c.Expr[A => T],
    to: c.Expr[B => U]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val fromOptic = selectorMacrosImpl.toOpticImpl[A, T](from)
    val toOptic = selectorMacrosImpl.toOpticImpl[B, U](to)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.renameField($fromOptic, $toOptic)"
    )
  }

  def transformFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[A => T],
    transform: c.Expr[DynamicSchemaExpr]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[A, T](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.transformField($optic, $transform)"
    )
  }

  def mandateFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[B => T],
    default: c.Expr[T]
  )(schema: c.Expr[Schema[T]]): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[B, T](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.mandateField[$T]($optic, $default)($schema)"
    )
  }

  def optionalizeFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[A => T]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[A, T](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.optionalizeField($optic)"
    )
  }

  def transformElementsImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    selector: c.Expr[A => Seq[T]],
    transform: c.Expr[DynamicSchemaExpr]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[A, Seq[T]](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.transformElements($optic, $transform)"
    )
  }

  def transformKeysImpl[A: WeakTypeTag, B: WeakTypeTag, K: WeakTypeTag, V: WeakTypeTag](
    selector: c.Expr[A => Map[K, V]],
    transform: c.Expr[DynamicSchemaExpr]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[A, Map[K, V]](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.transformKeys($optic, $transform)"
    )
  }

  def transformValuesImpl[A: WeakTypeTag, B: WeakTypeTag, K: WeakTypeTag, V: WeakTypeTag](
    selector: c.Expr[A => Map[K, V]],
    transform: c.Expr[DynamicSchemaExpr]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val optic = selectorMacrosImpl.toOpticImpl[A, Map[K, V]](selector)
    val builder = c.prefix.tree
    c.Expr[MigrationBuilder[A, B]](
      q"$builder.builder.transformValues($optic, $transform)"
    )
  }
}
