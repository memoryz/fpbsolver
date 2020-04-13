package FermiPicoBagel.webapp

import FermiPicoBagel.webapp.Solver._
import org.scalajs.dom.{Event, document}
import org.scalajs.dom.html._

import scala.scalajs.js.annotation.JSExportTopLevel

object Dom{

  var solver: Solver = _

  private def getNoSolutionAlert: Div = {
    document.getElementById("no-solution").asInstanceOf[Div]
  }

  private def getGamePanel: FieldSet = {
    document.getElementById("gamePanel").asInstanceOf[FieldSet]
  }

  private def getCandidatesTextArea: TextArea = {
    document.getElementById("candidates").asInstanceOf[TextArea]
  }

  private def getGuess: Input = {
    document.getElementById("guess").asInstanceOf[Input]
  }

  private def getResponse: Input = {
    document.getElementById("response").asInstanceOf[Input]
  }

  private def getConfPanel: FieldSet = {
    document.getElementById("confPanel").asInstanceOf[FieldSet]
  }

  private def getNDigitsInput: Input = {
    document.getElementById("nDigits").asInstanceOf[Input]
  }

  private def getAllowDuplicates: Input = {
    document.getElementById("allowDuplicates").asInstanceOf[Input]
  }

  private def getAllowZeros: Input = {
    document.getElementById("allowZeros").asInstanceOf[Input]
  }

  private def getAllowLeadingZero: Input = {
    document.getElementById("allowLeadingZero").asInstanceOf[Input]
  }

  private def getStepsTable: Table = {
    document.getElementById("steps").asInstanceOf[Table]
  }

  @JSExportTopLevel("startGameClicked")
  def startGameClicked(): Unit = {

    getConfPanel.disabled = true
    getGamePanel.disabled = false

    val nDigits = getNDigitsInput.valueAsNumber.toInt
    val allowDuplicates = getAllowDuplicates.checked
    val allowZeros = getAllowZeros.checked
    val allowLeadingZero = getAllowLeadingZero.checked

    solver = new Solver(nDigits, allowDuplicates, allowZeros, allowLeadingZero)
    solver.initialize()
    println("Candidates: " + solver.candidates.size)
    getCandidatesTextArea.textContent = solver.candidates.mkString(", ")
  }

  @JSExportTopLevel("addGuessClicked")
  def addGuessClicked(event: Event): Unit = {

    val forms = document.getElementsByClassName("needs-validation")
    (0 until forms.length) foreach {
      i =>
        if (!forms(i).asInstanceOf[Form].checkValidity()) {
          return
        }

        forms(i).classList.add("was-validated")
    }

    val guess = getGuess.value
    val response = getResponse.value
    println(guess + ":" + response)
    solver.addStep(guess, response)

    if (solver.candidates.length < 1) {
      getNoSolutionAlert.style.display = "block"
      getCandidatesTextArea.textContent = solver.candidates.mkString(", ")
    } else {
      getCandidatesTextArea.textContent = solver.candidates.mkString(", ")

      val hint = solver.getHint

      val stepsTable = getStepsTable
      val newRow = stepsTable.insertRow(stepsTable.rows.length).asInstanceOf[TableRow]
      newRow.insertCell(0).asInstanceOf[TableCell].textContent = guess
      newRow.insertCell(1).asInstanceOf[TableCell].textContent = response

      val hintCell = newRow.insertCell(2).asInstanceOf[TableCell]
      hintCell.onclick = {
        _ =>
          getGuess.value = hint
      }

      val hintDiv = document.createElement("div")
      hintDiv.textContent = hint
      hintDiv.classList.add("btn")
      hintDiv.classList.add("btn-outline-primary")
      hintCell.appendChild(hintDiv)

      getGuess.value = ""
      getResponse.value = ""
    }
  }

  @JSExportTopLevel("newGameClicked")
  def newGameClicked(): Unit = {
    getConfPanel.disabled = false
    getGamePanel.disabled = true
    getGuess.value = ""
    getResponse.value=""
    getCandidatesTextArea.textContent = ""
    getNoSolutionAlert.style.display = "none"
    while (getStepsTable.rows.length > 1) {
      getStepsTable.deleteRow(1)
    }
  }

  @JSExportTopLevel("NDigitKeyUp")
  def NDigitKeyUp(element: Input): Unit = {
    if (element.valueAsNumber.toInt < element.min.toInt) {
      element.value = element.min
    }

    if (element.valueAsNumber.toInt > element.max.toInt) {
      element.value = element.max
    }
  }
}
