package org.randi3.web.snippet

import xml.Elem
import scalaz.NonEmptyList
import net.liftweb.http.S


trait GeneralFormSnippet {


  protected def generateEntry(id: String, failure: Boolean, element: Elem): Elem = {
    <li id={id + "Li"} class={if (failure) "errorHint" else ""}>
      <label for={id}>
        {id}
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
