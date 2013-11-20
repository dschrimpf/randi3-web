package org.randi3.web.snippet

import net.liftweb.http.StatefulSnippet
import scala.xml.NodeSeq
import org.randi3.web.util.CurrentTrial
import org.randi3.web.lib.DependencyFactory
import org.randi3.model.User
import scala.collection.mutable.HashSet
import org.randi3.model.Role
import org.randi3.model.TrialRight
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.S._
import js.JsCmds.{Replace, SetHtml}
import net.liftweb.util.Helpers._
import net.liftweb._
import http.SHtml._
import scala.xml.Elem
import org.randi3.model.Trial
import org.randi3.web.util.CurrentLocalEDCTrial

class TrialUserEdit  extends StatefulSnippet with GeneralFormSnippet {
  
  private val userService = DependencyFactory.get.userService
  
  def dispatch = {
    case "render" => editUsers(CurrentTrial.get.getOrElse(redirectTo("/trial/list")), _)
    case "renderEDC" => editUsers(CurrentLocalEDCTrial.get.getOrElse(redirectTo("/edcTrial/list")).trial.getOrElse(redirectTo("/edcTrial/list")), _)
  }
  
  private def editUsers(trial: Trial, xhtml: NodeSeq): NodeSeq = {
    
    var selectedUser: User = null
    var selectedRole = Role.investigator
    
    val actualRights = new HashSet[(User, TrialRight)]()

    val allUsers = userService.getAllPossibleFromTrial(trial).toEither match {
      case Left(failure) => return <div>
        {failure}
      </div>
      case Right(users) => users

    }

    selectedUser = if (allUsers.isEmpty) null else allUsers.head

    val allUsersTrial = DependencyFactory.get.userService.getAllFromTrial(trial).toEither match {
      case Left(failure) => return <div>
        {failure}
      </div>
      case Right(users) => users

    }

    actualRights.clear()

    allUsersTrial.foreach(user => {

      val actUser = allUsers.find(actUser => actUser.id == user.id)
     if(actUser.isDefined){
      actUser.get.rights.foreach(right => {
        if (right.trial.id == trial.id)
          actualRights.add((actUser.get, TrialRight(right.role, trial).toOption.get))
      })
    }else {
       S.error("trialMsg", "The following user doesn't belong to one of the participating sites, please remove the user or add the site: " + user.username)
       val dbUser = userService.get(user.id).toOption.get
       dbUser.get.rights.foreach(right => {
         if (right.trial.id == trial.id)
           actualRights.add((dbUser.get, TrialRight(right.role, trial).toOption.get))
       })
    }

    })

    val rightsBefore = actualRights.toList
    def save() {

      val newRights = actualRights.toList.filter(actRight => !rightsBefore.contains(actRight))
      val removedRights = rightsBefore.filter(right => !actualRights.contains(right))

      newRights.foreach(userRight => userService.addTrialRight(userRight._1.id, userRight._2).toEither match {
        case Left(failure) =>  S.error("trialMsg", "Failure by adding a new right: "+ failure)
        case Right(x) =>
      })
      removedRights.foreach(userRight => userService.removeTrialRight(userRight._1.id, userRight._2).toEither match {
        case Left(failure) =>  S.error("trialMsg","Failure by removeing right: "+failure)
        case Right(x) =>
      })
      S.notice("Saved!")
    }

    def usersSelectField: Elem = {
      if (!allUsers.isEmpty) {
        ajaxSelectObj(allUsers.map(user => (user, user.username)).toSeq, Empty, (user: User) => {
          selectedUser = user
          Replace("roles", roleField)
        }, "id" -> "possibleUsers")
      } else {
        <span id="possibleUsers">{S.?("trial.noUsersAvailable")}</span>
      }
    }

    def roleField: Elem = {
      if (!allUsers.isEmpty) {
        val roles = Role.values.map(role => (role, role.toString)).toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) > 0)
        selectedRole = roles.head._1
        ajaxSelectObj(roles, Empty, (role: Role.Value) => selectedRole = role, "id" -> "roles")
      } else {
        <span id="roles"></span>
      }
    }

    def rights: Elem = {
      <table id="rights" class="randi2Table">
        <thead>
          <tr>
            <th>{S.?("menu.user")}</th>
            <th>{S.?("role")}</th>
            <th></th>
          </tr>
        </thead>{if (!actualRights.isEmpty) {
        <tfoot></tfoot>
      } else {
        <tfoot>
          <tr>
            <td rowspan="3">{S.?("trial.noUsersAvailable")}</td>
           </tr>
        </tfoot>
      }}<tbody>
        {actualRights.toList.sortWith((elem1, elem2) => (elem1._1.username + elem1._2.role.toString).compareTo((elem2._1.username + elem2._2.role.toString)) < 0).flatMap(entry => {
          <tr>
            <td>
              {entry._1.username}
            </td>
            <td>
              {entry._2.role.toString}
            </td>
            <td>
              {ajaxButton(S.?("remove"), () => {
              actualRights.remove(entry)
              Replace("rights", rights)
            })}
            </td>
          </tr>
        }
        )}
      </tbody>
      </table>
    }

    bind("trial", xhtml,
    "name" -> <span>{trial.name}</span>,
      "userSelect" -> usersSelectField,
      "roleSelect" -> roleField,
      "addRight" -> ajaxButton(S.?("add"), () => {
        actualRights.add((selectedUser, TrialRight(selectedRole, trial).toOption.get))
        Replace("rights", rights)
      }),
      "rights" -> rights,
      "submit" -> submit(S.?("save"), save _, "class" -> "btnSend")
    )
  }

}