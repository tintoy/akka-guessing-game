import actors.GuessingGame
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit._
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}

import scala.concurrent.duration._

/**
 * Tests for the `GuessingGame` actor.
 */
class GuessingGameSpec
    extends TestKit(
      ActorSystem("TestKitUsageSpec",
        ConfigFactory.parseString(GuessingGameSpec.config)
      )
    )
    with DefaultTimeout with ImplicitSender
    with WordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  /**
   * The actor reference for the current game.
   */
  var gameRef: ActorRef = null

  /**
   * The Id of the game currently in progress.
   */
  var currentGameId: Int = 0

  /**
   * Setup logic for each test.
   */
  override protected def beforeEach(): Unit = {
    super.beforeEach()

    currentGameId = GuessingGame.peekNextId
    gameRef = system.actorOf(
      Props[GuessingGame]()
    )
  }

  /**
   * Tear-down logic for each test.
   */
  override protected def afterEach(): Unit = {
    super.afterEach()

    system.stop(gameRef)
    gameRef = null

    // Ensure there are no outstanding messages.
    expectNoMsg(
      max = 10 milliseconds
    )
  }

  /**
   * Tear-down logic for the test suite.
   */
  override protected def afterAll(): Unit = {
    shutdown()

    super.afterAll()
  }

  import GuessingGame._

  val maxSecretNumber = 10
  val player1Name = "Bo Diddly"
  val player2Name = "Fee Fifofum"

  "New guessing game" should {
    "reply that it is not ready when the first player is introduced" in {
      within(500 milliseconds) {
        gameRef ! Introduce(player1Name)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )
      }
    }

    "reply that it is not ready if any player makes a guess" in {
      within(500 milliseconds) {
        gameRef ! Guess(player1Name, 9)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 2)
        )
      }
    }
  }

  "Guessing game with one player" should {
    "reply that it is ready when the second player is introduced" in {
      within(500 milliseconds) {
        // Player 1
        gameRef ! GuessingGame.Introduce(player1Name)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )

        // Player 2
        gameRef ! GuessingGame.Introduce(player2Name)
        expectMsg(
          Ready(currentGameId)
        )
        expectMsg(
          YourTurn(currentGameId, player2Name)
        )
      }
    }

    "reply that it is not ready if the first player makes a guess" in {
      within(500 milliseconds) {
        gameRef ! Introduce(player1Name)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )

        gameRef ! Guess(player2Name, 9)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )
      }
    }
  }

  "Guessing game with two players" should {
    "tell player 2 to go first" in {
      within(200 milliseconds) {
        gameRef ! Introduce(player1Name)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )

        gameRef ! Introduce(player2Name)
        expectMsg(
          Ready(currentGameId)
        )
        expectMsg(
          YourTurn(currentGameId, player2Name)
        )
      }
    }

    "reply to a player when they attempt guess out-of-turn" in {
      within(200 milliseconds) {
        gameRef ! Introduce(player1Name)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )

        gameRef ! Introduce(player2Name)
        expectMsg(
          Ready(currentGameId)
        )
        expectMsg(
          YourTurn(currentGameId, player2Name)
        )

        gameRef ! Guess(player1Name, 5)
        expectMsg(
          NotYourTurn(currentGameId, otherPlayerName = player2Name)
        )
      }
    }

    "reply to each player when they attempt to guess the secret number" in {
      within(2 seconds) {
        gameRef ! Introduce(player1Name)
        expectMsg(
          NotReady(currentGameId, stillWaitingForPlayers = 1)
        )

        gameRef ! Introduce(player2Name)
        expectMsg(
          Ready(currentGameId)
        )

        val yourTurn = expectMsgClass(classOf[YourTurn])

        // Play until someone wins.
        var won = false
        var currentPlayerName: String = yourTurn.playerName
        var otherPlayerName: String = {
          if (currentPlayerName == player1Name) player2Name else player1Name
        }
        var currentGuessCount = 0
        var currentGuess = 5
        while (!won) {
          currentGuessCount += 1
          currentGuessCount should (be > 0)
          currentGuessCount should (be <= maxSecretNumber)

          gameRef ! Guess(currentPlayerName, currentGuess)
          expectMsgClass(classOf[GameMessage]) match {
            case Win(gameId, playerName, correctValue, guessCount) =>
              gameId should be(currentGameId)
              playerName should be(currentPlayerName)
              correctValue should be(currentGuess)
              guessCount should be(currentGuessCount)

              won = true

            case NopeTryAgain(gameId, playerName, incorrectValue, hint) =>
              gameId should be(currentGameId)
              playerName should be(otherPlayerName)
              incorrectValue should be(currentGuess)

              hint match {
                case Lower => currentGuess -= 1
                case Higher => currentGuess += 1
              }
          }

          // Swap players
          val nextPlayerName = otherPlayerName
          otherPlayerName = currentPlayerName
          currentPlayerName = nextPlayerName
        }
      }
    }
  }
}

/**
 * Static state for the guessing game specification.
 */
object GuessingGameSpec {
  /**
   * The Akka configuration for use in tests.
   */
  val config =
    """
    akka {
      loglevel = "WARNING"
    }
    """
}
