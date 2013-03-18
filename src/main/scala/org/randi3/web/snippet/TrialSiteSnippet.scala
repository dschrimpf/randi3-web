package org.randi3.web.snippet


import scala.xml._
import scala.xml.Group
import scala.xml.NodeSeq

import org.randi3.web.lib.DependencyFactory

import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http._
import js.JsCmds._

import net.liftweb.util.Helpers._
import org.randi3.model.TrialSite
import scala.Right
import scalaz._
import org.randi3.web.util.CurrentTrialSite

class TrialSiteSnippet extends StatefulSnippet with GeneralFormSnippet{


  private var name = ""
  private var country = ""
  private var postCode = ""
  private var city = ""
  private var street = ""
  private var password = ""
  private var passwordCheck = ""
  private var active = true


  def dispatch = {
    case "info" => redirectTo("/trialSite/list")
    case "create" => create _
    case "edit" => edit _
    case "activate" => activate _
    case "deactivate" => deactivate _
  }

  private val trialSiteService = DependencyFactory.get.trialSiteService


  /**
   * Confirm deleting a user
   */
  private def confirmDelete(xhtml: Group): NodeSeq = {
    if (CurrentTrialSite.isDefined) {
      val trialSite = CurrentTrialSite.get.get
      def deleteTrialSite() {
        notice("Trial site " + (trialSite.name) + " deleted")
        trialSiteService.delete(trialSite)
        redirectTo("/trialSite/list.html")
      }

      // bind the incoming XHTML to a "delete" button.
      // when the delete button is pressed, call the "deleteUser"
      // function (which is a closure and bound the "user" object
      // in the current content)
      bind("xmp", xhtml, "name" -> (trialSite.name),
        "delete" -> submit("Delete", deleteTrialSite _))

      // if the was no ID or the user couldn't be found,
      // display an error and redirect
    } else {
      error("Trial site not found");
      redirectTo("/trialSite/list.html")
    }
  }


  private def create(xhtml: NodeSeq): NodeSeq = {

    def save() {
      TrialSite(name = name, street = street, postCode = postCode, city = city, country = country, password = password, isActive = active).either match {
        case Left(x) => S.error("trialSiterMsg", x.toString) //TODO set field failure
        case Right(site) => trialSiteService.create(site).either match {
          case Left(x) => S.error("trialSiteMsg", x)
          case Right(b) => {
            clearFields()
            S.notice("Thanks \"" + name + "\" saved!")
            S.redirectTo("/trialSite/list")
          }
        }
      }
    }

    generateForm(xhtml, save)
  }


  private def edit(xhtml: NodeSeq): NodeSeq = {
    if (CurrentTrialSite.isDefined) {
      val selectedTrialSite = CurrentTrialSite.get.get
      setFields(selectedTrialSite)

      def update() {
        TrialSite(id = selectedTrialSite.id, version = selectedTrialSite.version, name = name, street = street, postCode = postCode, city = city, country = country, password = password, isActive = selectedTrialSite.isActive).either match {
          case Left(x) => S.error("trialSiterMsg", x.toString) //TODO set field failure
          case Right(site) => trialSiteService.update(site).either match {
            case Left(x) => S.error("trialSiteMsg", x)
            case Right(b) => {
              clearFields()
              S.notice("Thanks \"" + name + "\" saved!")
              S.redirectTo("/trialSite/list")
            }
          }
        }
      }

      generateForm(xhtml, update)
    } else S.redirectTo("/trialSite/list")

  }


  private def activate(xhtml: NodeSeq): NodeSeq = {
    if (CurrentTrialSite.isDefined) {

      def activate() {
        trialSiteService.activate(CurrentTrialSite.get.get).either match {
          case Left(x) => S.error(x)
          case Right(b) => {
            clearFields()
            S.notice("Thanks \"" + name + "\" saved!")
            S.redirectTo("/trialSite/list")
          }
        }
      }

      bind("trialSite", xhtml,
        "submit" -> submit("activate " + CurrentTrialSite.get.get.name, activate _)
      )
    } else S.redirectTo("/trialSite/list")

  }

  private def deactivate(xhtml: NodeSeq): NodeSeq = {
    if (CurrentTrialSite.isDefined) {

      def deactivate() {
        trialSiteService.deactivate(CurrentTrialSite.get.get).either match {
          case Left(x) => S.error(x)
          case Right(b) => {
            clearFields()
            S.notice("Thanks \"" + name + "\" saved!")
            S.redirectTo("/trialSite/list")
          }
        }
      }

      bind("trialSite", xhtml,
        "submit" -> submit("deactivate " + CurrentTrialSite.get.get.name, deactivate _)
      )
    } else S.redirectTo("/trialSite/list")

  }


  private def generateForm(xhtml: NodeSeq, code: => Unit): NodeSeq = {

    def nameField(failure: Boolean = false): Elem = {
      val id = "name"
      generateEntry(id, failure, {
        ajaxText(name, v => {
          name = v
          TrialSite.check(name = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", nameField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", nameField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def streetField(failure: Boolean = false): Elem = {
      val id = "street"
      generateEntry(id, failure, {
        ajaxText(street, v => {
          street = v
          TrialSite.check(street = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", streetField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", streetField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def postCodeField(failure: Boolean = false): Elem = {
      val id = "postCode"
      generateEntry(id, failure, {
        ajaxText(postCode, v => {
          postCode = v
          TrialSite.check(postCode = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", postCodeField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", postCodeField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def cityField(failure: Boolean = false): Elem = {
      val id = "city"
      generateEntry(id, failure, {
        ajaxText(city, v => {
          city = v
          TrialSite.check(city = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", cityField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", cityField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def countryField(failure: Boolean = false): Elem = {
      val id = "country"
      generateEntry(id, failure, {
        ajaxText(country, v => {
          country = v
          TrialSite.check(country = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", countryField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", countryField(false))
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
          TrialSite.check(password = v).either match {
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

    bind("trialSite", xhtml,
    "info" -> <span>{name}</span>,
      "name" -> nameField(),
      "street" -> streetField(),
      "postCode" -> postCodeField(),
      "city" -> cityField(),
      "country" -> countryField(),
      "password" -> passwordField(),
      "passwordCheck" -> passwordCheckField(),
      "submit" -> submit(S.?("save"), code _)
    )
  }



  private def clearFields() {
    name = ""
    country = ""
    postCode = ""
    city = ""
    street = ""
    password = ""
    active = true
  }

  private def setFields(trialSite: TrialSite) {
    name = trialSite.name
    country = trialSite.country
    postCode = trialSite.postCode
    city = trialSite.city
    street = trialSite.street
    password = trialSite.password
    active = trialSite.isActive
  }

}


