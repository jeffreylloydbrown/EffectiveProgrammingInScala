## Overview

First, download the [handout archive](https://moocs.scala-lang.org/~dockermoocs/handouts/scala-3/effective-codecs.zip) and extract it somewhere on your filesystem.

The goal of this assignment is to implement a small serialization library. This library will be able to encode Scala values (such as instances of case classes) into [JSON](https://www.json.org) documents that can be sent over the wire (or saved to a file). Conversely, the library will be able to decode JSON documents as Scala values. JSON serialization is often used by web servers.

Please make sure you are familiar with the [JSON](https://www.json.org) serialization format before starting the assignment.

The library will follow a "type-directed" approach. This means that it will use a type-class Encoder[A] to model the ability of encoding a value of type A into JSON, and a type-class Decoder[A] to model the ability of decoding a JSON document as a value of type A.

First, you will define given instances for simple types (e.g., Int, String, etc.). Then, you will define conditional instances to combine encoders and decoders together to handle more complex types.

To make things easier, the encoders and decoders implemented in this assignment don’t directly work with JSON blobs, but they work with an intermediate Json data type:

```bash
sealed trait Json
object Json:
  /** The JSON `null` value */
  case object Null extends Json
  /** JSON boolean values */
  case class Bool(value: Boolean) extends Json
  /** JSON numeric values */
  case class Num(value: BigDecimal) extends Json
  /** JSON string values */
  case class Str(value: String) extends Json
  /** JSON objects */
  case class Obj(fields: Map[String, Json]) extends Json
  /** JSON arrays */
  case class Arr(items: List[Json]) extends Json
end Json
```

Here is an example of a JSON value:

```bash
{
  "foo": 0,
  "bar": [true, false]
}
```

It can be represented as follows with the Json data type:

```bash
Json.Obj(Map(
  "foo" -> Json.Num(0),
  "bar" -> Json.Arr(Json.Bool(true), Json.Bool(false))
))
```

With this Json type being defined, **encoding** a value of type A consists of transforming it into a value of type Json:

```bash
trait Encoder[-A]:
  /** Encodes a value of type `A` into JSON */
  def encode(value: A): Json
```

Unlike arbitrary JSON values, JSON objects have the special property that two objects can be combined to build a bigger JSON object containing both objects’ fields. We use this combining property of JSON objects to define the zip method on Encoder returning JSON objects. We do so in a subclass of Encoder called ObjectEncoder:

```bash
trait ObjectEncoder[-A] extends Encoder[A]:
  // Refines the encoding result to `Json.Obj`
  def encode(value: A): Json.Obj

  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] = ...
```

Conversely, **decoding** a value of type A consists in transforming a value of type Json into a value of type A:

```bash
trait Decoder[+A]:
  /**
    * @param data The data to de-serialize
    * @return The decoded value wrapped in `Some`, or `None` if decoding failed
    */
  def decode(data: Json): Option[A]
```

Note that the decoding operation returns an Option[A] instead of just an A because the decoding process can fail (None means that it was impossible to produce an A from the supplied JSON data).

Given instances of encoders and decoders are defined in the EncoderInstances and DecoderInstances traits, respectively. These traits are inherited by the Encoder and Decoder companion objects, respectively. By doing so, we make the given instances automatically available, removing the need for explicitly importing them.

We provide you with a parseJson function that parses a JSON blob from a String into a Json value, and a renderJson function that turns a Json value into a JSON blob in a String. You can use them to experiment with the encoders and decoders that you write. The functions parseJson and renderJson are defined in the file **Util.scala**.

## Your Task

Your work consists in writing codec instances that can be combined together to build codec instances for more complex types. You will start by writing instances for basic types (such as String or Boolean) and you will end up writing instances for case classes.

Open the file Codecs.scala. It contains the definition of the types Json, Encoder, ObjectEncoder and Decoder. Complete the partially implemented given instance definitions (replace the ??? with proper implementations) and introduce new given instance definitions as needed (look for TODO comments).

At any time, you can follow your progress by running the test sbt task. You can also use the run sbt task to run the Main program defined at the bottom of the codecs.json file. Last, you can create a REPL session with the console sbt task, and then experiment with your code:

```bash
sbt:progfun2-codecs> console

scala> import codecs._
import codecs._

scala> Util.parseJson(""" { "name": "Bob", "age": 10 } """)
val res0: Option[codecs.Json] = Some(Obj(Map(name -> Str(Bob), age -> Num(10))))

scala> res0.flatMap(_.decodeAs[Person]) // This will crash until you implement it in this assignment
val res1: Option[codecs.Person] = Some(Person(Bob,10))

scala> summon[Encoder[Int]]
val res2: codecs.Encoder[Int] = codecs.Encoder$$anon$1@74d8fde0

scala> res2.encode(42)
val res3: codecs.Json = Num(42)

scala> :quit

```

Remember that if you make any change to the code, you'll need to quit the console and start it again to run the updated code.

### Basic Codecs

Start by implementing the given instances of Encoder[String] and Encoder[Boolean], and the corresponding given instances of Decoder[Int], Decoder[String] and Decoder[Boolean].

Hint: use the provided methods Encoder.fromFunction, Decoder.fromFunction, and Decoder.fromPartialFunction.

Make sure that your Int decoder rejects JSON floating point numbers.

### Derived Codecs

The next step consists in implementing a codec for lists of elements of type A, given a codec for type A. The encoder instance for lists is already implemented. Fill in the definition of the corresponding decoder.

Once you have defined the encoder and decoder for lists, you should be able to **summon** them for any list containing elements that can be encoded and decoded. You can try, for instance, to evaluate the following expressions in the REPL:

```bash
summon[Encoder[List[Int]]]
summon[Decoder[List[Boolean]]]
```

### JSON Object Codecs

Next, implement codecs for JSON objects. The approach consists in defining codecs for JSON objects having a single field, and then combining such codecs to handle JSON objects with multiple fields.

For example, consider the following JSON object with one field x:

```bash
{
  "x": 1
}
```

An encoder for this JSON object can be defined by using the provided ObjectEncoder.field operation, which takes the field name as a parameter and an implicit Encoder for the field value:

```bash
val xField = ObjectEncoder.field[Int]("x")
```

Here is an example of use of the xField encoder:

```bash
scala> xField.encode(42)
val res0: codecs.Json.Obj = Obj(Map(x -> Num(42)))
```

Last, to define an encoder producing a JSON object with more than one field, you can combine two ObjectEncoder instances with the zip operation. This operation returns an ObjectEncoder producing a JSON object with the fields of the two combined encoders. For instance, you can define an encoder producing an object with two fields x and y as follows:

```bash
val pointEncoder: ObjectEncoder[(Int, Int)] =
  val xField = ObjectEncoder.field[Int]("x")
  val yField = ObjectEncoder.field[Int]("y")
  xField.zip(yField)
```

Implement a Decoder.field operation corresponding to the ObjectEncoder.field operation.

### Codecs for Case Classes

The zip operation mentioned in the previous section only returns codecs for tuples. It would be more convenient to work with high-level data types (for instance, a Point data type, rather than a pair of coordinates).

Both encoders and decoders have a transform operation that can be used to transform the type Json values are encoded from, or decoded to, respectively. You can see in the assignment how it is used to define a given Encoder[Person].

Define a corresponding Decoder[Person], and then define a given instance of Encoder[Contacts] and a given instance of Decoder[Contacts].

## Bonus Questions

*   Can you **explicitly** write the value inferred by the compiler when it summons the Encoder[List[Person]]?

*   Would it be possible to define codecs for optional fields?

*   Would it be possible to define a function that provides a given instance of Encoder[Option[A]] for any type A for which there is a given instance of Encoder[A] (like we do with Encoder[List[A]])?

*   Would it be possible to define codecs for sealed traits in addition to case classes?

### How to submit

Copy the token below and run the submission script included in the assignment download. When prompted, use your email address **email withheld**.

Generate new token

Your submission token is unique to you and should not be shared with anyone. You may submit as many times as you like.