# Into and As Type Classes

`Into` and `As` are type classes for safe, automatic data transformation and schema evolution in ZIO Schema.

- `Into[A, B]` defines a one-way conversion from `A` to `B`.
- `As[A, B]` defines a bidirectional conversion (bijection or reversible transformation) between `A` and `B`.

## Derivation

ZIO Schema provides automatic macro derivation for `Into` and `As` for case classes and sealed traits.

### Usage

```scala
import zio.blocks.schema.{Into, As}

case class PersonV1(name: String, age: Int)
case class PersonV2(name: String, age: Long, active: Boolean = true)

// Derive one-way conversion
val conversion = Into[PersonV1, PersonV2]

val v1 = PersonV1("Alice", 30)
val v2 = conversion.into(v1)
// Right(PersonV2("Alice", 30L, true))
```

### Derivation Rules

1.  **Fields Mapping**: Fields are matched by name.
    - If types match exactly, value is copied.
    - If types differ, an implicit `Into[FieldA, FieldB]` is looked up.
    - Primitive widening (e.g., `Int` -> `Long`) is supported automatically.
2.  **Default Values**: If a field in the target is missing in the source, the default value from the target's constructor is used.
3.  **Coproducts**: Sealed traits (enums) are matched by case name.
    - `case object A` maps to `case object A`.
    - `case class B(...)` maps to `case class B(...)` recursively using `Into`.
4.  **Recursion**: Derivation works deeply for nested case classes and collections.

### Collections

Automatic conversion is provided for standard collections:
- `List[A] -> List[B]`
- `List[A] -> Vector[B]`
- `Option[A] -> Option[B]`
- `Map[K1, V1] -> Map[K2, V2]` (requires `Into` for keys and values)

### Error Handling

`into` returns `Either[SchemaError, B]`. Errors are accumulated.

- **Validation Errors**: Narrowing conversions (e.g., `Long` -> `Int`) fail if values are out of bounds.
- **Missing Fields**: If a required field is missing and has no default, derivation fails at compile time.

## As (Bidirectional)

`As[A, B]` extends `Into[A, B]` and adds `from(b: B): Either[SchemaError, A]`.

```scala
case class Point(x: Int, y: Int)
case class PointL(x: Long, y: Long)

// Derives both Into[Point, PointL] and Into[PointL, Point]
val as = As[Point, PointL]
// Note: This will fail compilation if narrowing conversion (Long -> Int) is not safe or possible without runtime checks?
// Actually, As derivation allows narrowing but warns/fails at runtime if values overflow.
```

Note: `As` derivation typically requires that the structure is symmetric or defaults are available for both directions (though strictly symmetric is preferred for `As`).
