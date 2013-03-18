package org.randi3.web.snippet

import org.randi3.web.lib.DependencyFactory
import org.randi3.web.util.CurrentTrial
import net.liftweb.http.S._

import org.randi3.web.util.CurrentTrial
import scala.xml._
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.Text
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq}
import net.liftweb.util._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._
import js.JE

import org.randi3.model.{Role, User}


class TrialUserSnippet {

  def render(xhtml: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }
    val allUsers = DependencyFactory.get.userService.getAllFromTrial(trial).either match {
      case Left(failure) => return <div>
        {failure}
      </div>
      case Right(users) => users

    }

    bind("user", xhtml,
      "all" -> getUserTable(allUsers),
      "trialAdministrator" -> getUserTable(allUsers.filter(user => !user.rights.filter(right => right.role == Role.trialAdministrator).isEmpty)),
      "principalInvestigator" -> getUserTable(allUsers.filter(user => !user.rights.filter(right => right.role == Role.principleInvestigator).isEmpty)),
      "investigator" -> getUserTable(allUsers.filter(user => !user.rights.filter(right => right.role == Role.investigator).isEmpty)),
      "monitor" -> getUserTable(allUsers.filter(user => !user.rights.filter(right => right.role == Role.monitor).isEmpty)),
      "statistician" -> getUserTable(allUsers.filter(user => !user.rights.filter(right => right.role == Role.statistician).isEmpty))
    )
  }

  private def getUserTable(users: List[User]): Elem = {
    <table class="randi2Table">
      <thead>
        <tr>
          <th>Username</th>
          <th>First name</th>
          <th>Last name</th>
          <th>E-Mail</th>
          <th>Site</th>
        </tr>
      </thead>{if (users.isEmpty) {
      <tfoot>
        <tr>
          <td colspan="5">no users defined</td>
        </tr>
      </tfoot>
    } else {
      <tfoot></tfoot>
    }}<tbody>
      {users.flatMap(user => {
        <tr>
          <td>
            {user.username}
          </td>
          <td>
            {user.firstName}
          </td>
          <td>
            {user.lastName}
          </td>
          <td>
            {user.email}
          </td>
          <td>
            {user.site.name}
          </td>
        </tr>
      })}
    </tbody>
    </table>
  }

}
