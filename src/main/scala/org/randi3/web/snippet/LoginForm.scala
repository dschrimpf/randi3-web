package org.randi3.web.snippet

import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb._

import scala.xml._
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.Text
import org.randi3.web.lib.DependencyFactory
import org.randi3.web.util.CurrentLoggedInUser


class LoginForm {

  val userService = DependencyFactory.get.userService

  def login(xhtml: NodeSeq): NodeSeq = {
    var username = ""
    var password = ""
    def authentificate() {
      userService.login(username, password).either match {
        case Left(x) => {
          if (x != null) S.error(x)
          redirectTo("/index")
        }
        case Right(user) => {
          CurrentLoggedInUser(Some(user))
          redirectTo("/index")
        }
      }
    }
    bind("login", xhtml,
      "username" -> SHtml.text(username, username = _),
      "password" -> SHtml.password(password, password = _),
      "submit" -> SHtml.submit(S.?("login"), authentificate _))
  }
}
