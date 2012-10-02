package org.randi3.web.snippet

import org.randi3.web.util.CurrentUser
import net.liftweb.http.S
import org.randi3.web.lib.DependencyFactory
import xml.Elem
import scalaz.NonEmptyList

/**
 * Created with IntelliJ IDEA.
 * User: schrimpf
 * Date: 02.10.12
 * Time: 10:48
 * To change this template use File | Settings | File Templates.
 */
trait HelperSnippet {


  private val userService = DependencyFactory.userService

  protected def updateCurrentUser = {
    userService.get(CurrentUser.get.get.id).either match {
      case Left(x) => S.error(x)
      case Right(user) => {
        CurrentUser(user)
      }
    }
  }


  protected def generateEntry(id: String, failure: Boolean, element: Elem): Elem = {
    <li id={id + "Li"} class={if (failure) "errorHint" else ""}>
      <label for={id}>
        {id}
      </label>{element}<lift:msg id={id + "Msg"} errorClass="err"/>
    </li>
  }

  protected def generateEntryWithInfo(id: String, failure: Boolean, info: String, element: Elem): Elem = {
    <li id={id + "Li"} class={if (failure) "errorHint" else ""}>
      <label for={id}>
        <span>
          {id}
        </span>
        <span class="tooltip">
          <img src="/images/icons/help16.png" alt={info} title={info}/> <span class="info">
          {info}
        </span>
        </span>
      </label>{element}<lift:msg id={id + "Msg"} errorClass="err"/>
    </li>
  }


  protected def showErrorMessage(id: String, errors: NonEmptyList[String]) {
    S.error(id + "Msg", "<-" + errors.list.reduce((acc, el) => acc + ", " + el))
  }

  protected def clearErrorMessage(id: String) {
    S.error(id + "Msg", "")
  }
}
