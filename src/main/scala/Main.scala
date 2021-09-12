import cats.effect.{IO, IOApp}
import cats.effect.kernel.Async
import cats.syntax.all._

object Main extends IOApp.Simple:

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
  
  def run: IO[Unit] =
    IO.println(parseCommand(move(Direction.North)))
