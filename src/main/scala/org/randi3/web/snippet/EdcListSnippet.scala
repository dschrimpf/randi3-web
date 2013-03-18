package org.randi3.web.snippet

import java.util.Locale

import scala.xml._
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.Text

import org.randi3.web.lib.DependencyFactory

import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb._
import scalaz.NonEmptyList
import org.randi3.web.util.{CurrentEDCTrial, CurrentLoggedInUser, CurrentUser}
import org.randi3.model._
import collection.mutable.{HashSet, ListBuffer}
import org.randi3.edc.model.openClinica.ConnectionOC


class EdcListSnippet {


  private var location = "http://localhost:8080/OpenClinica-ws/"
  private var username = "root"
  private var passwordHash = "5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8"

  def listRemoteTrials(nodeSeq: NodeSeq): NodeSeq = {

    def locationField(failure: Boolean = false): Elem = {
      val id = "location"
      generateEntry(id, failure, {
        ajaxText(location, v => {
          location = v
        }, "id" -> id)
      }
      )
    }

    def usernameField(failure: Boolean = false): Elem = {
      val id = "username"
      generateEntry(id, failure, {
        ajaxText(username, v => {
          username = v
        }, "id" -> id)
      }
      )
    }

    def passwordHashField(failure: Boolean = false): Elem = {
      val id = "passwordHash"
      generateEntry(id, failure, {
        ajaxText(passwordHash, v => {
          passwordHash = v
        }, "id" -> id)
      }
      )
    }

    def refreshListButton: Elem = {
      ajaxButton("refreshEdcList", () => {
        Replace("edcTrialList", edcTrials)
      })
    }

    def edcTrials: Elem = {
      <div id="edcTrialList">
        {if (!(location.isEmpty || username.isEmpty || passwordHash.isEmpty)) {
        val trials = DependencyFactory.get.openClinicaService.getTrials(
          new ConnectionOC(location = location, username = username, passwordHash = passwordHash)
        )
        if (trials.isEmpty) {
          <span>no trials available</span>
        } else {

          <table class="randi2Table">
            <thead>
              <tr>
                <th>Identifier</th>
                <th>Name</th>
                <th></th>
              </tr>
            </thead>
            <tfoot>

            </tfoot>
            <tbody>
              {
              trials.flatMap(trial =>
            <tr>
              <td>{trial.identifier}</td>
              <td>{trial.name}</td>
              <td>{link("/trial/generalInformation", () => {
                CurrentEDCTrial.set(Some(DependencyFactory.get.openClinicaService.getFullTrialOC(trial).get))
                redirectTo("/edcTrial/viewRemoteDetails")
                }, Text("select"))}</td>
            </tr>
              )
              }
            </tbody>
          </table>
        }

      } else {
        <span>please add the connection information</span>
      }}
      </div>

    }


    bind("edcTrial", nodeSeq,
      "location" -> locationField(),
      "username" -> usernameField(),
      "passwordHash" -> passwordHashField(),
      "refreshList" -> refreshListButton,
      "edcTrials" -> edcTrials

    )
  }

  private def generateEntry(id: String, failure: Boolean, element: Elem): Elem = {
    <li id={id + "Li"} class={if (failure) "errorHint" else ""}>
      <label for={id}>
        {id}
      </label>{element}<lift:msg id={id + "Msg"} errorClass="err"/>
    </li>
  }


  private def showErrorMessage(id: String, errors: NonEmptyList[String]) {
    S.error(id + "Msg", "<-" + errors.list.reduce((acc, el) => acc + ", " + el))
  }

  private def clearErrorMessage(id: String) {
    S.error(id + "Msg", "")
  }

}
