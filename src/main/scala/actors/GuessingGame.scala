package actors

import java.util.concurrent.atomic.AtomicInteger
import akka.actor.Actor
import scala.util.Random

/**
 * Actor that implements a guessing game.
 */
class GuessingGame extends Actor {
  /**
   * The Id of the current game (mainly used for correlation).
   */
  val gameId = GuessingGame.nextId.incrementAndGet()

  /**
   * The secret number that the players have to guess.
   */
  val secretNumber = new Random().nextInt(10) + 1

  var player1Name: String = null
  var player2Name: String = null

  /**
   * The combined number of guesses that both players have made.
   */
  var guessCount = 0

  /**
   * Once the game has been won, the name of the winning player.
   */
  var winningPlayerName: String = null

  import GuessingGame._

  // TODO: These guard clauses are overly complicated; this works much better as an FSM actor.
  override def receive: Receive = {
    case Introduce(playerName) if player1Name == null =>
      this.player1Name = playerName
      sender ! NotReady(gameId, stillWaitingForPlayers = 1)
    case Introduce(playerName) if player2Name == null =>
      this.player2Name = playerName
      sender ! Ready(gameId)
      sender ! YourTurn(gameId, currentPlayerName)
    case Introduce(playerName) =>
      sender ! GameInProgress(gameId)
    case Guess(playerName, value: Int) if player1Name == null =>
      sender ! NotReady(gameId, stillWaitingForPlayers = 2)
    case Guess(playerName, value: Int) if player2Name == null =>
      sender ! NotReady(gameId, stillWaitingForPlayers = 1)
    case Guess(playerName, value: Int) if winningPlayerName != null =>
      sender ! NotReady(gameId, stillWaitingForPlayers = 1)
    case Guess(playerName, value: Int) if playerName != currentPlayerName =>
      sender ! NotYourTurn(gameId, currentPlayerName)
    case Guess(playerName, value) =>
      guessCount += 1
      if (value != secretNumber) {
        val hint = if (secretNumber > value) Higher else Lower

        sender ! NopeTryAgain(gameId, currentPlayerName, value, hint)
      }
      else
        sender ! Win(gameId, playerName, value, guessCount)
  }

  /**
   * The name of the player whose turn it is to play.
   * @return
   */
  def currentPlayerName: String = {
    if ((guessCount + 1) % 2 == 0) player1Name else player2Name
  }
}

/**
 * Actor that implements a guessing game.
 */
object GuessingGame
{
  /**
   * The Id to be allocated to the next game.
   */
  private val nextId: AtomicInteger = new AtomicInteger(0)

  /**
   * Get the most-recently-allocated guessing game Id.
   * @return The guessing game Id.
   */
  def lastAllocatedId = nextId.get()

  /**
   * Get the next guessing game Id without allocating it.
   * @return The guessing game Id.
   */
  def peekNextId = nextId.get() + 1

  /**
   * The base class for messages representing requests to the game.
   * @param playerName The name of the player making the request.
   */
  sealed abstract class GameRequest(playerName: String)

  /**
   * Introduce a player to the game.
   * @param playerName The player name.
   */
  case class Introduce(playerName: String) extends GameRequest(playerName)

  /**
   * Attempt to guess the secret number.
   * @param playerName The name of the player making a guess.
   * @param value The player making 
   */
  case class Guess(playerName: String, value: Int) extends GameRequest(playerName)

  /**
   * The base class for messages representing responses from the game.
   * @param gameId The Id of the game to which the message relates.
   */
  sealed abstract class GameMessage(gameId: Int)

  /**
   * The game indicates that it is ready for the first player to go.
   * @param gameId The Id of the game to which the message relates.
   */
  case class Ready(gameId: Int) extends GameMessage(gameId)

  /**
   * The game indicates that it is not ready to play.
   * @param gameId The Id of the game to which the message relates.
   * @param stillWaitingForPlayers The number of players that still have to join the game before it is ready to play.
   */
  case class NotReady(gameId: Int, stillWaitingForPlayers: Int) extends GameMessage(gameId)

  /**
   * The game indicates that it is the specified player's turn to guess.
   * @param gameId The Id of the game to which the message relates.
   * @param playerName The name of the player whose turn it is to guess.
   */
  case class YourTurn(gameId: Int, playerName: String) extends GameMessage(gameId)

  /**
   * The game indicates that it is NOT the specified player's turn to guess.
   * @param gameId The Id of the game to which the message relates.
   * @param otherPlayerName The name of the (other) player whose turn it is to guess.
   */
  case class NotYourTurn(gameId: Int, otherPlayerName: String) extends GameMessage(gameId)

  /**
   * The game indicates that it is currently in progress (players cannot join once the game has started).
   * @param gameId The Id of the game to which the message relates.
   */
  case class GameInProgress(gameId: Int) extends GameMessage(gameId)

  /**
   * The game indicates that it is over (cannot join or guess once game is overe).
   * @param gameId The Id of the game to which the message relates.
   * @param winningPlayerName The name of the player that won the game.
   * @param winningGuess The value of the winning guess.
   * @param guessCount The number of guesses before the secret number was found.
   */
  case class GameOver(gameId: Int, winningPlayerName: String, winningGuess: Int, guessCount: Int) extends GameMessage(gameId)

  /**
   * The game indicates that the player has won the game.
   * @param gameId The Id of the game to which the message relates.
   * @param winningPlayerName The name of the winning player.
   * @param winningGuess The value of the winning guess.
   * @param guessCount The number of guesses before the secret number was found.
   */
  case class Win(gameId: Int, winningPlayerName: String, winningGuess: Int, guessCount: Int) extends GameMessage(gameId)

  /**
   * The game indicates that the player has lost the game.
   * @param gameId The Id of the game to which the message relates.
   * @param winningPlayerName The name of the winning player.
   * @param winningGuess The value of the winning guess.
   * @param guessCount
   * @note Not used yet.
   */
  case class Lose(gameId: Int, winningPlayerName: String, winningGuess: Int, guessCount: Int) extends GameMessage(gameId)

  /**
   * The game indicates that the player's guess was unsuccessful.
   * @param gameId The Id of the game to which the message relates.
   * @param nextPlayerName The name of the next player whose turn it is to guess (nevertheless, wait for a `YourTurn` message before proceeding).
   * @param incorrectValue The value that was incorrectly guessed by the player.
   * @param hint A `Hint` indicating that the next guess should be `Higher` or `Lower`.
   */
  case class NopeTryAgain(gameId: Int, nextPlayerName: String, incorrectValue: Int, hint: Hint) extends GameMessage(gameId)

  /**
   * A hint indicating the direction in which the next guess should diverge from the previous (incorrect) guess.
   */
  sealed trait Hint

  /**
   * The next guess should be higher.
   */
  case object Higher extends Hint

  /**
   * The next guess should be lower.
   */
  case object Lower extends Hint
}
