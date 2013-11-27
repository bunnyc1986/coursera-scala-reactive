package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate: Double = 0.01
    val dieProbablity: Double = 0.25
    val transmissibility: Double = 0.4
    val moveDelay: Int = 5
    val sickDelay: Int = 6
    val dieDelay: Int = 14
    val immuneDelay: Int = 16
    val turnHealthyDelay: Int = 18
  }

  import SimConfig._

  val prevalencePopulation = population * prevalenceRate
  val persons: List[Person] = (for {
    i <- 0 until population
  } yield {
    val p = new Person(i)
    if (i < prevalencePopulation) {
      p.becomeInfected
    }
    p.move
    p
  }).toList

  def isRoomHealty(r: Int, c: Int): Boolean = {
    persons.find(p => p.row == r && p.col == c && (p.sick || p.dead)).isEmpty
  }

  def isRoomInfected(r: Int, c: Int): Boolean = {
    persons.find(p => p.row == r && p.col == c && p.infected).size > 0
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def becomeInfected() {
      infected = true
      afterDelay(sickDelay)(becomeSick)
    }

    def becomeSick() {
      sick = true
    }

    def becomeDead() {
      if (random < dieProbablity)
        dead = true
    }

    def becomeImmune() {
      if (!dead) {
        sick = false
        immune = true
      }
    }

    def becomeHealty() {
      if (!dead) {
        immune = false
        infected = false
      }
    }

    def move() {
      def _move() {
        if (!dead) {
          val neighbors = List(
            ((row - 1 + roomRows) % roomRows, col), //up
            ((row + 1) % roomRows, col), //down
            (row, (col - 1 + roomColumns) % roomColumns), //right
            (row, (col + 1) % roomColumns)) //left
          val healtyNeighbors = neighbors.filter(room => isRoomHealty(room._1, room._2))
          if (!healtyNeighbors.isEmpty) {
            healtyNeighbors(randomBelow(healtyNeighbors.length)) match {
              case (r, c) => {
                row = r
                col = c
              }
            }
          }
          if (!immune && !infected && random < transmissibility && isRoomInfected(row, col)) {
            becomeInfected
          }
          move()
        }
      }
      afterDelay(randomBelow(moveDelay) + 1)(_move)
    }
  }
}
