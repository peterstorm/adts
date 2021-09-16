import cats.effect.{IO, IOApp}
import cats.effect.kernel.Async
import cats.syntax.all._
import cats.Functor
import cats.Traverse
import cats.Applicative
import cats.Eval

object Main extends IOApp.Simple:

  def myFoldLeft[A, B](xs: Seq[A], b: B)(f: (B, A) => B): B =
    def withAccum(xs: Seq[A], acc: B): B = xs match
      case Seq() => acc
      case x +: xs => withAccum(xs, f(acc, x))
    withAccum(xs, b)

  def myMap[A, B](xs: Seq[A])(f: A => B): Seq[B] =
    xs match
      case Seq() => Seq()
      case x +: xs => f(x) +: myMap(xs)(f)

  sealed trait JSON
  final case class JsObject(get: Map[String, JSON]) extends JSON
  final case class JsString(get: String) extends JSON
  final case class JsNumber(get: Double) extends JSON
  case object JsNull extends JSON

  trait JsonWriter[A]:
    
    def write(a: A): JSON

  opaque type Name = String

  object Name:

    def apply(value: String): Name = value


  final case class Person(
    name: Name
  )

  final case class PersonF[A](
    a: A
  )

  object PersonFscala2:

    implicit val personfFunctor: Functor[PersonF] =
      new Functor[PersonF] {
        def map[A, B](a: PersonF[A])(f: A => B): PersonF[B] =
          PersonF(f(a.a))
      }

    implicit class PersonfFunctorOps[PersonF[_], A](p: PersonF[A]) {
      def map[B](f: A => B)(implicit functor: Functor[PersonF]): PersonF[B] =
        functor.map(p)(f)
    }

  object PersonF:

    given Functor[PersonF] with
      def map[A, B](a: PersonF[A])(f: A => B): PersonF[B] =
          PersonF(f(a.a))

    extension[A](a: PersonF[A])
      def map[B](f: A => B): PersonF[B] =
        summon[Functor[PersonF]].map(a)(f)

    given Traverse[PersonF] with
      def traverse[F[_]: Applicative, A, B](a: PersonF[A])(f: A => F[B]): F[PersonF[B]] =
        a match
          case PersonF(a) => Applicative[F].map(f(a))(PersonF(_))

      def foldLeft[A, B](a: PersonF[A], b: B)(f: (B, A) => B): B =
        a match
           case PersonF(a) => f(b, a)

      def foldRight[A, B](a: PersonF[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???

  object scala3Instances:

    given JsonWriter[String] with
      def write(a: String): JSON =
        JsString(a)

    given JsonWriter[Person] with
      def write(a: Person): JSON =
        JsObject(Map("name" -> JsString(a.name)))


  object scala2Instances:

    implicit val stringWriter: JsonWriter[String] =
      new JsonWriter[String] {
        def write(a: String): JSON = JsString(a)
      }

    implicit val personWriter: JsonWriter[Person] =
      new JsonWriter[Person] {
        def write(a: Person): JSON =
          JsObject(Map("name" -> JsString(a.name)))
      }

  object jsonSyntax:

    implicit class JsonWriterOps[A](a: A) {
      def toJson(implicit w: JsonWriter[A]): JSON =
        w.write(a)
    }

  enum Direction:
    case North, East, South, West

  final abstract class Idle
  final abstract class Moving

  enum Command[Before, After]:
    case Face(d: Direction) extends Command[Idle, Idle]
    case Start              extends Command[Idle, Moving]
    case Stop               extends Command[Moving, Idle]
    case Chain[A, B, C](
      cmd1: Command[A, B],
      cmd2: Command[B, C]
    ) extends Command[A, C]

  extension[A, B](cmd1: Command[A, B])
      def ~>[C](cmd2: Command[B, C]): Command[A, C] =
        Command.Chain(cmd1, cmd2)

  def label(d: Direction): String= d match
    case Direction.North => "north"
    case Direction.East  => "east"
    case Direction.South => "south"
    case Direction.West  => "west"


  val start: Command[Idle, Moving] = Command.Start

  val stop: Command[Moving, Idle] = Command.Stop

  def face(dir: Direction): Command[Idle, Idle] =
    Command.Face(dir)

  def move(d: Direction): Command[Idle, Idle] =
    face(d) ~> start ~> stop ~> start ~> stop

  def parseCommand(cmd: Command[_, _]): String = cmd match
    case Command.Stop => "stopped"
    case Command.Start => "started"
    case Command.Face(d) => s"facing ${label(d)}"
    case Command.Chain(cmd1, cmd2) => parseCommand(cmd1) |+| s" and ${parseCommand(cmd2)}"

  import scala3Instances._
  import jsonSyntax._
  
  def run: IO[Unit] =
    IO.println(parseCommand(move(Direction.North))) >>
    IO.println(PersonF(2).map(_ + 1)) >>
    IO.println(PersonF(2).foldLeft(0)(_ + _)) >>
    IO.println(myFoldLeft((0 to 100), 0)(_ + _)) >>
    IO.println(myMap(List(1,2,3,4))(_ + 1))
