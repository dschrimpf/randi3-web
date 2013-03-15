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
import org.randi3.web.util.{CurrentLoggedInUser, CurrentUser}
import org.randi3.model._
import collection.mutable.{HashSet, ListBuffer}

class UserCreateUpdateSnippet extends StatefulSnippet with GeneralFormSnippet{


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

  private val locales = Locale.getAvailableLocales.toList
    .sortBy(locale =>  if(!locale.getCountry.isEmpty) {locale.getDisplayLanguage +" ("+ locale.getDisplayCountry +")"} else {locale.getDisplayLanguage})
    .map(locale => (locale, if(!locale.getCountry.isEmpty) {locale.getDisplayLanguage +" ("+ locale.getDisplayCountry +")"} else {locale.getDisplayLanguage})).toSeq

  private var locale: Locale = Locale.ENGLISH

  def dispatch = {
    case "create" => create _
    case "edit" => edit _
  }


  /**
   * Add a user
   */
  private def create(xhtml: NodeSeq): NodeSeq = {

    def save() {
      //TODO validate
      User(username = username, password = password, email = email, firstName = firstName, lastName = lastName, phoneNumber = phoneNumber, site = actualTrialSite, rights = actualRights.toSet, administrator = isAdministrator, canCreateTrial = canCreateTrial, locale = locale).either match {
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
    if (CurrentUser.get.isDefined) {
      val user = CurrentUser.get.get

      setFields(user)

      def update() {
        User(id = user.id, version = user.version, username = username, password = password, email = email, firstName = firstName, lastName = lastName, phoneNumber = phoneNumber, site = actualTrialSite, rights = actualRights.toSet, administrator = isAdministrator, canCreateTrial = canCreateTrial, isActive = isActive, locale = locale).either match {
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
            <span class="elementLeft">{S.?("street")}:</span>
            <span class="elementRight">
              {actualTrialSite.street}
            </span>
          </div>
          <div>
            <span class="elementLeft">{S.?("postCode")}:</span>
            <span class="elementRight">
              {actualTrialSite.postCode}
            </span>
          </div>
          <div>
            <span class="elementLeft">{S.?("city")}:</span>
            <span class="elementRight">
              {actualTrialSite.city}
            </span>
          </div>
          <div>
            <span class="elementLeft">{S.?("country")}:</span>
            <span class="elementRight">
              {actualTrialSite.country}
            </span>
          </div>
        </div>
      } else <div id="trialSiteInfo"></div>

    }

    def administratorField: Elem = {
      val id = "administrator"
      generateEntry(id, false, {
        <div>
          <span>{S.?("user.isUserAdministrator")}</span>{ajaxCheckbox(isAdministrator, status => {
          isAdministrator = status
        })}
        </div>
      })
    }

    def canCreateTrialsField: Elem = {
      val id = "canCreateTrials"
      generateEntry(id, false, {
        <div>
          <span>{S.?("user.canUserCreateTrials")}</span>{ajaxCheckbox(canCreateTrial, status => {
          canCreateTrial = status
        })}
        </div>
      })
    }

    def isActiveField: Elem = {
      val id = "isActive"
      generateEntry(id, false, {
        <div>
          <span>{S.?("user.isActive")}</span>{ajaxCheckbox(isActive, status => {
          isActive = status
        })}
        </div>
      })
    }

    def rights: Elem = {
      <table id="rights" class="randi2Table">
        <thead>
          <tr>
            <th>{S.?("trial")}</th>
            <th>{S.?("role")}</th>
            <th></th>
          </tr>
        </thead>{if (!actualRights.isEmpty) {
        <tfoot></tfoot>
      } else {
        <tfoot>
          <tr>
            <td colspan="2"></td>
            {S.?("user.noRights")}</tr>
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
              {ajaxButton(S.?("remove"), () => {
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
        <span id="possibleTrials">{S.?("user.cantAddRights")}</span>
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
      "info" -> <span>{username}</span>,
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
      "addRight" -> ajaxButton(S.?("add"), () => {
        actualRights.add(TrialRight(selectedRole, selectedTrial).toOption.get)
        Replace("rights", rights)
      }),
      "rights" -> rights,
      "numberOfFailedLogins" -> generateEntry("numberOfFailedLogins", false, {
        <span>
          {if(CurrentUser.isDefined) CurrentUser.get.get.numberOfFailedLogins}
        </span>
      }),
      "lockedUntil" -> generateEntry("lockedUntil", false, {
        <span>
          {if(CurrentUser.isDefined && CurrentUser.get.get.lockedUntil.isDefined) CurrentUser.get.get.lockedUntil.get else <span>---</span>}
        </span>
      }),
      "resetLock" -> button(S.?("resetLock"), () => {
        val actUser = CurrentUser.get.get
        val dbUser = userService.get(actUser.id).toOption.get.get
        val changedUser = dbUser.copy(numberOfFailedLogins = 0, lockedUntil=None)
        val updatedUser = userService.update(changedUser).either match {
          case Left(failure) => S.error(failure)
          case Right(user) => CurrentUser.set(Some(user))
        }

       S.redirectTo("/user/edit")
      }
      ),
    "locale" -> selectObj(locales, Full(locale), (loc:Locale) => locale = loc),
      "submit" -> submit(S.?("save"), code _)
    )

  }



  private def generatePossibleTrials() {
    if (CurrentLoggedInUser.get.isDefined) {
      val rights = CurrentLoggedInUser.get.get.rights.toList

      trials = rights.filter(right => (right.role == Role.principleInvestigator || right.role == Role.trialAdministrator)).map(right => (right.trial, right.trial.name)).toSet.toList

      selectedTrial = if (!trials.isEmpty) trials.head._1 else null
    } else {
      trials = Nil
      selectedTrial = null
    }
  }





  private def clearFields() {
    CurrentUser.set(None)
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
    locale = Locale.ENGLISH
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
    isActive = user.isActive
    trialSites = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO error handling
    actualRights.clear()
    user.rights.foreach(right => actualRights.add(right))
    locale = user.locale
  }


}

