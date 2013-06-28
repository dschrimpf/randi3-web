package org.randi3.web.snippet

import org.randi3.model.{User, TrialSite}
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import org.randi3.web.lib.DependencyFactory
import org.randi3.web.util.CurrentLoggedInUser
import net.liftweb.http.js.JsCmd
import net.liftweb.common.{Full, Empty}
import net.liftweb.http.js.jquery.JqJsCmds.DisplayMessage
import net.liftweb.http.js.JE.ValById
import com.sun.jmx.mbeanserver.MXBeanProxy.GetHandler
import scalaz.NonEmptyList
import xml._
import net.liftweb.http.{LiftScreen, StatefulSnippet, S, SHtml}
import java.util.Locale


class RegisterForm extends StatefulSnippet with GeneralFormSnippet{

  private val userService = DependencyFactory.get.userService
  private val trialSiteService = DependencyFactory.get.trialSiteService


  def dispatch = {case _ => render}

  private var username = ""
  private var password = ""
  private var passwordCheck = ""
  private var email = ""
  private var firstName = ""
  private var lastName = ""
  private var phoneNumber = ""
  private var actualTrialSite: TrialSite = null
  private var trialSitePassword = ""
  private val locales = Locale.getAvailableLocales.toList
    .sortBy(locale =>  if(!locale.getCountry.isEmpty) {locale.getDisplayLanguage +" ("+ locale.getDisplayCountry +")"} else {locale.getDisplayLanguage})
    .map(locale => (locale, if(!locale.getCountry.isEmpty) {locale.getDisplayLanguage +" ("+ locale.getDisplayCountry +")"} else {locale.getDisplayLanguage})).toSeq

  private var locale: Locale = Locale.ENGLISH

  private var trialSites = trialSiteService.getAllActive.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO error handling

  /**
   * Add a user
   */
  private def render(xhtml: NodeSeq): NodeSeq = {

    def usernameField(failure: Boolean = false): Elem = {
      val id = "username"
      generateEntry(id, failure, {
        ajaxText(username, v => {
          username = v
          User.check(username = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", usernameField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", usernameField(false))
          }
        }, "id" -> id)
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
        selectObj(trialSites, Empty, (trialSite:TrialSite) => actualTrialSite = trialSite)
      })
    }

    def trialSitPasswordeField: Elem = {
      val id = "trialSitePassword"
      generateEntry(id, false, {
        text(trialSitePassword, v => trialSitePassword = v, "type" -> "password")
      })
    }


    def localeField: Elem = {
      val id = "locale"
      generateEntry(id, false, {
        selectObj(locales, Full(locale), (loc:Locale) => locale = loc)
      })
    }


    def register() {
      User(username = username, password = password, email = email, firstName = firstName, lastName = lastName, phoneNumber = phoneNumber, site = actualTrialSite, rights = Set(), locale = locale).either match {
        case Left(x) => S.error("registerMsg",x.toString)   //TODO set field failure
        case Right(user) =>userService.register(user, trialSitePassword).either match {
          case Left(x) => S.error("registerMsg",x)
          case Right(b) => {
            clearFields
            S.notice("Thanks user \"" + username + "\" saved!")
            S.redirectTo("login")
          }
        }
      }
    }

    bind("register", xhtml,
      "username" -> usernameField(),
      "password" -> passwordField(),
      "passwordCheck" -> passwordCheckField(),
      "firstName" -> firstNameField(),
      "lastName" -> lastNameField(),
      "email" -> emailField(),
      "phoneNumber" -> phoneNumberField(),
      "trialSite" -> trialSiteField,
      "trialSitePassword" -> trialSitPasswordeField,
      "locale" -> localeField,
    "cancel" -> submit("cancel", () => {
      clearFields()
      redirectTo("/login")
    }, "class" -> "btnCancel"),
      "submit" -> submit("register", register _ , "class" -> "btnSend")
    )

  }

  private def clearFields() {
      username = ""
      password = ""
      passwordCheck = ""
      email = ""
      firstName = ""
      lastName = ""
      phoneNumber = ""
      actualTrialSite = null
      trialSitePassword = ""
     locale = Locale.ENGLISH
   }
}

