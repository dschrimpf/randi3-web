package org.randi3.web.snippet

import org.randi3.web.lib.DependencyFactory
import xml.{Text, NodeSeq}
import net.liftweb.http.{Templates, SHtml, S}
import net.liftweb.http.S._
import scala.Left
import scala.Right
import scala.Some
import net.liftweb.util.Helpers._
import scala.Left
import scala.Right
import scala.Some
import net.liftweb.http.js.jquery.JqJsCmds.ModalDialog
import net.liftweb.http.js.JsCmds.Alert


class SupportSnippet {


  val userService = DependencyFactory.get.userService

  def supportUsers(xhtml: NodeSeq): NodeSeq = {
    userService.getAllAdministrators.either match {
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
      </tr>)
    }
  }

  def dialog(xhtml: NodeSeq): NodeSeq = {
    SHtml.a(() => try {
      S.runTemplate(List("_jsdialog_confirm")).
        map(ns => ModalDialog(ns)) openOr
        Alert("Couldn't find _jsdialog_confirm template")
    } catch {
      case error: Exception => Alert("exception: "+error.getMessage)
    }, Text("show dialog"))
  }


}
