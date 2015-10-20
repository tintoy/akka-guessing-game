package actors

import java.util.concurrent.atomic.AtomicInteger
import akka.actor.{FSM, Actor}
import scala.util.Random

import GuessingGame._

/**
 * Actor that implements a guessing game.
 */
class GuessingGame extends Actor with FSM[State, Data] {

  startWith(NewGame,
    Data(
      gameId = nextId.incrementAndGet(),
      secretNumber = new Random().nextInt(10) + 1
    )
  )

  when(NewGame) {
    case Event(Introduce(playerName), data) =>
      sender ! NotReady(data.gameId, stillWaitingForPlayers = 1)

      goto(WaitingForSecondPlayer) using data.copy(
        playerNames = data.playerNames :+ playerName
      )

    case Event(anyOtherEvent, data) =>
      sender ! NotReady(data.gameId, stillWaitingForPlayers = 2)

      stay()
  }
  when(WaitingForSecondPlayer) {
    case Event(Introduce(playerName), data) =>
      val updatedPlayerNames = data.playerNames :+ playerName
      val updatedCurrentPlayer = updatedPlayerNames.last

      sender ! Ready(data.gameId)
      sender ! YourTurn(data.gameId, updatedCurrentPlayer)

      goto(Playing) using data.copy(
        playerNames = updatedPlayerNames,
        currentPlayer = updatedPlayerNames.indexOf(playerName)
      )

    case Event(anyOtherEvent, data) =>
      sender ! NotReady(data.gameId, stillWaitingForPlayers = 1)

      stay()
  }
  when(Playing) {
    case Event(Guess(playerName, value), data) if !data.currentPlayerName.contains(playerName) =>
      sender ! NotYourTurn(data.gameId, data.currentPlayerName.get)

      stay()

    case Event(Guess(playerName, value), data) =>
      val currentGuessCount = data.guessCount + 1
      val nextPlayer = (data.currentPlayer + 1) % data.playerNames.size
      if (value == data.secretNumber) {
        sender ! Won(data.gameId, playerName, value, currentGuessCount)
        
        goto(Over) using data.copy(
          guessCount = currentGuessCount,
          winningPlayerName = data.currentPlayerName.get
        )
      }
      else {
        val correctiveHint = if (value > data.secretNumber) Lower else Higher

        sender ! NopeTryAgain(data.gameId,
          nextPlayerName = data.playerNames(nextPlayer),
          incorrectValue = value,
          hint = correctiveHint
        )

        stay using data.copy(
          currentPlayer = nextPlayer,
          guessCount = currentGuessCount
        )
      }
    case Event(anyOtherEvent, data) =>
      sender ! GameInProgress(data.gameId)

      stay()
  }
  when(Over) {
    case Event(anyEvent, data) =>
      sender ! GameOver(
        data.gameId,
        data.winningPlayerName,
        data.secretNumber,
        data.guessCount
      )

      stay()
  }
}

/**
 * Actor that implements a guessing game.
 */
object GuessingGame
{
  /**
   * Represents the state for the guessing game.
   */
  sealed abstract class State

  /**
   * State representing a new guessing game.
   */
  private case object NewGame extends State

  /**
   * State representing a guessing game waiting for a second player.
   */
  private case object WaitingForSecondPlayer extends State

  /**
   * State representing a guessing game that is in progress.
   */
  private case object Playing extends State

  /**
   * State representing a guessing game that has been completed.
   */
  private case object Over extends State

  /**
   * Represents state data for the guessing game.
   * @param gameId The game Id.
   * @param secretNumber The secret number that the players have to guess.
   * @param playerNames The names of the players participating in the game.
   * @param currentPlayer The 0-based index (in `playerNames`) of the name of the player whose turn is next.
   * @param guessCount The combined number of guesses that both players have made.
   * @param winningPlayerName Once the game has been won, the name of the winning player.
   */
  sealed case class Data(
      gameId: Int,
      secretNumber: Int,
      playerNames: List[String] = List[String](),
      currentPlayer: Int = 0,
      guessCount: Int = 0,
      winningPlayerName: String = null) {

    def currentPlayerName: Option[String] = {
      if (currentPlayer != -1) Some(playerNames(currentPlayer)) else None
    }
  }

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
  case class Won(gameId: Int, winningPlayerName: String, winningGuess: Int, guessCount: Int) extends GameMessage(gameId)

  /**
   * The game indicates that the player has lost the game.
   * @param gameId The Id of the game to which the message relates.
   * @param winningPlayerName The name of the winning player.
   * @param winningGuess The value of the winning guess.
   * @param guessCount The number of guesses before the secret number was found.
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
