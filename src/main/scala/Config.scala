import caseapp._

@AppName("Advent of Code")
@AppVersion("1.0")
@ProgName("aoc")
final case class Config(
  @Name("d")
  @HelpMessage("Day of the challenge")
  day: Int,

  @Name("t")
  @HelpMessage("Run in test mode")
  test: Boolean = false
)