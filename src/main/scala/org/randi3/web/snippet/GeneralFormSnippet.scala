package org.randi3.web.snippet

import xml.Elem
import scalaz.NonEmptyList
import net.liftweb.http.S
import org.randi3.web.lib.DependencyFactory
import org.randi3.web.util.CurrentLoggedInUser


trait GeneralFormSnippet {


  protected def generateEntry(id: String, failure: Boolean, element: Elem): Elem = {
    <li id={id + "Li"} class={if (failure) "errorHint" else ""}>
      <label for={id}>
        {S.?(id)}
      </label>{element}<lift:msg id={id + "Msg"} errorClass="err"/>
    </li>
  }

  protected def showErrorMessage(id: String, errors: NonEmptyList[String]) {
    S.error(id + "Msg", "<-" + errors.list.reduce((acc, el) => acc + ", " + el))
  }

  protected def clearErrorMessage(id: String) {
    S.error(id + "Msg", "")
  }

  private val userService = DependencyFactory.userService

  protected def updateCurrentUser = {
    userService.get(CurrentLoggedInUser.get.get.id).either match {
      case Left(x) => S.error(x)
      case Right(user) => {
        CurrentLoggedInUser(user)
      }
    }
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

}
