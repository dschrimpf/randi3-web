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
import org.randi3.web.util.{CurrentUser, CurrentSelectedUser}
import org.randi3.model._
import collection.mutable.{HashSet, ListBuffer}

class UserSnippet extends StatefulSnippet {


  private val userService = DependencyFactory.userService
  private val trialSiteService = DependencyFactory.trialSiteService

  private var username = ""
  private var password = ""
  private var passwordCheck = ""
  private var email = ""
  private var firstName = ""
  private var lastName = ""
  private var phoneNumber = ""
  private var isAdministrator = false
  private var canCreateTrial = false
  private var isActive = true

  private var trialSites: List[(TrialSite, String)] = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO error handling

  private var actualTrialSite: TrialSite = trialSites.head._1


  private var trials: List[(Trial, String)] = Nil
  private var selectedTrial: Trial = null
  private var selectedRole = Role.investigator
  private val actualRights = new HashSet[TrialRight]()

  def dispatch = {
    case "info" => redirectTo("/user/list")
    case "users" => users _
    case "create" => create _
    case "edit" => edit _
    case "show" => show _
  }


  /**
   * Get the XHTML containing a list of users
   */
  private def users(xhtml: NodeSeq): NodeSeq = {
    val currentUser = CurrentUser.get.get
    userService.getAll.either match {
      case Left(x) => <tr>
        <td colspan="8">
          {x}
        </td>
      </tr>
      case Right(users) => users.flatMap(user => <tr>
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
          {user.phoneNumber}
        </td>
        <td>
          {user.site.name}
        </td>
        <td>
          {user.isActive}
        </td>
        <td>
          {if (currentUser.administrator) link("/user/show", () => CurrentSelectedUser.set(Some(user)), Text("Show"))}
        </td>
        <td>
          {if (currentUser.administrator) link("/user/edit", () => CurrentSelectedUser.set(Some(user)), Text("Edit"))}
        </td>
      </tr>)
    }
  }


  /**
   * Add a user
   */
  private def create(xhtml: NodeSeq): NodeSeq = {

    def save() {
      //TODO validate
      User(username = username, password = password, email = email, firstName = firstName, lastName = lastName, phoneNumber = phoneNumber, site = actualTrialSite, rights = actualRights.toSet, administrator = isAdministrator, canCreateTrial = canCreateTrial).either match {
        case Left(x) => S.error(x.toString())
        case Right(user) => userService.create(user).either match {
          case Left(x) => S.error("userMsg", x)
          case Right(b) => {
            clearFields()
            S.notice("Saved!")
            S.redirectTo("/user/list")
          }
        }
      }
    }
    generateFrom(xhtml, save)

  }

  /**
   * Edit a user
   */
  private def edit(xhtml: NodeSeq): NodeSeq = {
    if (CurrentSelectedUser.get.isDefined) {
      val user = CurrentSelectedUser.get.get

      setFields(user)

      def update() {
        User(id = user.id, version = user.version, username = username, password = password, email = email, firstName = firstName, lastName = lastName, phoneNumber = phoneNumber, site = actualTrialSite, rights = actualRights.toSet, administrator = isAdministrator, canCreateTrial = canCreateTrial, isActive = isActive).either match {
          case Left(x) => S.error("userMsg", x.toString())
          case Right(actUser) => userService.update(actUser).either match {
            case Left(x) => S.error("userMsg", x)
            case Right(upUser) => {
              clearFields()
              S.redirectTo("/user/list")
              S.notice("Saved!")
            }
          }
        }
      }

      generateFrom(xhtml, update, true)

    } else S.redirectTo("/user/list")
  }

  private def generateFrom(xhtml: NodeSeq, code: => Unit, editForm: Boolean = false): NodeSeq = {
    def usernameField(failure: Boolean = false): Elem = {
      val id = "username"
      generateEntry(id, failure, {
        if (!editForm) {
          ajaxText(username, v => {
            username = v
            User.check(username = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", usernameField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", usernameField(false))
            }
          }, "id" -> id)
        } else <span>
          {username}
        </span>
      }
      )
    }

    def passwordField(failure: Boolean = false): Elem = {
      val id = "password"
      generateEntry(id, failure, {
        ajaxText(password, v => {
          password = v
          User.check(password = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", passwordField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", passwordField(false))
          }
        }, "id" -> id, "type" -> "password")
      }
      )
    }

    def passwordCheckField(failure: Boolean = false): Elem = {
      val id = "passwordCheck"
      generateEntry(id, failure, {
        ajaxText(passwordCheck, v => {
          passwordCheck = v
          if (password == passwordCheck) {
            clearErrorMessage(id)
            Replace(id + "Li", passwordCheckField(false))
          } else {
            S.error(id + "Msg", "<- passwords does not match")
            Replace(id + "Li", passwordCheckField(true))
          }
        }, "id" -> id, "type" -> "password")
      }
      )
    }

    def firstNameField(failure: Boolean = false): Elem = {
      val id = "firstName"
      generateEntry(id, failure, {
        ajaxText(firstName, v => {
          firstName = v
          User.check(firstName = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", firstNameField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", firstNameField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def lastNameField(failure: Boolean = false): Elem = {
      val id = "lastName"
      generateEntry(id, failure, {
        ajaxText(lastName, v => {
          lastName = v
          User.check(firstName = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", lastNameField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", lastNameField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def emailField(failure: Boolean = false): Elem = {
      val id = "email"
      generateEntry(id, failure, {
        ajaxText(email, v => {
          email = v
          User.check(firstName = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", emailField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", emailField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def phoneNumberField(failure: Boolean = false): Elem = {
      val id = "phoneNumber"
      generateEntry(id, failure, {
        ajaxText(phoneNumber, v => {
          phoneNumber = v
          User.check(firstName = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", phoneNumberField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", phoneNumberField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def trialSiteField: Elem = {
      val id = "trialSite"
      generateEntry(id, false, {
        ajaxSelectObj(trialSites, Empty, (trialSite: TrialSite) => {
          actualTrialSite = trialSite
          Replace("trialSiteInfo", trialSiteInfo)
        })
      })
    }

    def trialSiteInfo: Elem = {
      if (actualTrialSite != null) {
        <div id="trialSiteInfo">
          <div>
            <span class="elementLeft">Country:</span>
            <span class="elementRight">
              {actualTrialSite.country}
            </span>
          </div>
          <div>
            <span class="elementLeft">City:</span>
            <span class="elementRight">
              {actualTrialSite.city}
            </span>
          </div>
          <div>
            <span class="elementLeft">PostCode:</span>
            <span class="elementRight">
              {actualTrialSite.postCode}
            </span>
          </div>
          <div>
            <span class="elementLeft">Street:</span>
            <span class="elementRight">
              {actualTrialSite.street}
            </span>
          </div>
        </div>
      } else <div id="trialSiteInfo"></div>

    }

    def administratorField: Elem = {
      val id = "administrator"
      generateEntry(id, false, {
        <div>
          <span>Is the user an administrator?</span>{ajaxCheckbox(isAdministrator, status => {
          isAdministrator = status
        })}
        </div>
      })
    }

    def canCreateTrialsField: Elem = {
      val id = "canCreateTrials"
      generateEntry(id, false, {
        <div>
          <span>Can the user create Trials?</span>{ajaxCheckbox(canCreateTrial, status => {
          canCreateTrial = status
        })}
        </div>
      })
    }

    def isActiveField: Elem = {
      val id = "isActive"
      generateEntry(id, false, {
        <div>
          <span>Is user active?</span>{ajaxCheckbox(isActive, status => {
          isActive = status
        })}
        </div>
      })
    }

    def rights: Elem = {
      <table id="rights" class="randi2Table">
        <thead>
          <tr>
            <th>Trial</th>
            <th>Role</th>
            <th></th>
          </tr>
        </thead>{if (!actualRights.isEmpty) {
        <tfoot></tfoot>
      } else {
        <tfoot>
          <tr>
            <td colspan="2"></td>
            No rights defined</tr>
        </tfoot>
      }}<tbody>
        {actualRights.flatMap(right => {
          <tr>
            <td>
              {right.trial.name}
            </td>
            <td>
              {right.role.toString}
            </td>
            <td>
              {ajaxButton("remove", () => {
              actualRights.remove(right)
              Replace("rights", rights)
            })}
            </td>
          </tr>
        }
        )}
      </tbody>
      </table>

    }

    def trialsSelectField: Elem = {
      generatePossibleTrials
      if (!trials.isEmpty) {
        ajaxSelectObj(trials, Empty, (trial: Trial) => {
          selectedTrial = trial
          Replace("roles", roleField)
        }, "id" -> "possibleTrials")
      } else {
        <span id="possibleTrials">Not the right to add rights</span>
      }
    }

    def roleField: Elem = {
      if (!trials.isEmpty) {
        val roles = Role.values.map(role => (role, role.toString)).toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) > 0)
        selectedRole = roles.head._1
        ajaxSelectObj(roles, Empty, (role: Role.Value) => selectedRole = role, "id" -> "roles")
      } else {
        <span id="roles"></span>
      }
    }

    bind("user", xhtml,
      "username" -> usernameField(),
      "password" -> passwordField(),
      "passwordCheck" -> passwordCheckField(),
      "firstName" -> firstNameField(),
      "lastName" -> lastNameField(),
      "email" -> emailField(),
      "phoneNumber" -> phoneNumberField(),
      "trialSite" -> trialSiteField,
      "trialSiteInfo" -> trialSiteInfo,
      "administrator" -> administratorField,
      "canCreateTrials" -> canCreateTrialsField,
      "isActive" -> isActiveField,
      "trialsSelect" -> trialsSelectField,
      "roleSelect" -> roleField,
      "addRight" -> ajaxButton("add", () => {
        actualRights.add(TrialRight(selectedRole, selectedTrial).toOption.get)
        Replace("rights", rights)
      }),
      "rights" -> rights,
      "submit" -> submit("save", code _)
    )

  }


  private def show(xhtml: NodeSeq): NodeSeq = {
    val user = CurrentSelectedUser.get.get

    def trialSiteInfo: Elem = {

      <div id="trialSiteInfo">
        <div>
          <span class="elementLeft">Name:</span>
          <span class="elementRight">
            {user.site.name}
          </span>
        </div>
        <div>
          <span class="elementLeft">Country:</span>
          <span class="elementRight">
            {user.site.country}
          </span>
        </div>
        <div>
          <span class="elementLeft">City:</span>
          <span class="elementRight">
            {user.site.city}
          </span>
        </div>
        <div>
          <span class="elementLeft">PostCode:</span>
          <span class="elementRight">
            {user.site.postCode}
          </span>
        </div>
        <div>
          <span class="elementLeft">Street:</span>
          <span class="elementRight">
            {user.site.street}
          </span>
        </div>
      </div>
    }

    def rights: Elem = {
      <table id="rights" class="randi2Table">
        <thead>
          <tr>
            <th>Trial</th>
            <th>Role</th>
          </tr>
        </thead>{if (!user.rights.isEmpty) {
        <tfoot></tfoot>
      } else {
        <tfoot>
          <tr>
            <td></td>
            No rights defined</tr>
        </tfoot>
      }}<tbody>
        {user.rights.toList.sortWith((a, b) => a.trial.name.compareToIgnoreCase(b.trial.name) < 0).flatMap(right => {
          <tr>
            <td>
              {right.trial.name}
            </td>
            <td>
              {right.role.toString}
            </td>
          </tr>
        }
        )}
      </tbody>
      </table>

    }

    bind("user", xhtml,
      "username" -> generateEntry("username", false, {
        <span>
          {user.username}
        </span>
      }),
      "firstName" -> generateEntry("firstName", false, {
        <span>
          {user.firstName}
        </span>
      }),
      "lastName" -> generateEntry("lastName", false, {
        <span>
          {user.lastName}
        </span>
      }),
      "email" -> generateEntry("email", false, {
        <span>
          {user.email}
        </span>
      }),
      "phoneNumber" -> generateEntry("phoneNumber", false, {
        <span>
          {user.phoneNumber}
        </span>
      }),
      "trialSite" -> generateEntry("username7", false, {
        <span>
          {user.username}
        </span>
      }),
      "trialSiteInfo" -> trialSiteInfo,
      "isActive" -> generateEntry("isActive", false, {
        <span>
          {user.isActive}
        </span>
      }),
      "administrator" -> generateEntry("administrator", false, {
        <span>
          {user.administrator}
        </span>
      }),
      "canCreateTrials" -> generateEntry("canCreateTrials", false, {
        <span>
          {user.canCreateTrial}
        </span>
      }),
      "rights" -> rights
    )
  }


  private def generatePossibleTrials() {
    if (CurrentUser.get.isDefined) {
      val rights = CurrentUser.get.get.rights.toList

      trials = rights.filter(right => (right.role == Role.principleInvestigator || right.role == Role.trialAdministrator)).map(right => (right.trial, right.trial.name)).toSet.toList

      selectedTrial = if (!trials.isEmpty) trials.head._1 else null
    } else {
      trials = Nil
      selectedTrial = null
    }
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


  private def clearFields() {
    CurrentSelectedUser.set(None)
    username = ""
    password = ""
    passwordCheck = ""
    email = ""
    firstName = ""
    lastName = ""
    phoneNumber = ""
    trialSites = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO error handling
    actualTrialSite = trialSites.head._1
    actualRights.clear()
  }

  private def setFields(user: User) {
    username = user.username
    password = user.password
    passwordCheck = ""
    email = user.email
    firstName = user.firstName
    lastName = user.lastName
    phoneNumber = user.phoneNumber
    actualTrialSite = user.site
    isAdministrator = user.administrator
    canCreateTrial = user.canCreateTrial
    trialSites = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO error handling
    actualRights.clear()
    user.rights.foreach(right => actualRights.add(right))
  }


}

