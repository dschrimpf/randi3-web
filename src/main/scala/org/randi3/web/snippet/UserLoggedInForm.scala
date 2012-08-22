package org.randi3.web.snippet


import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq, Group}

import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http._
import org.randi3.web.util.{CurrentSelectedUser, CurrentTrial, CurrentUser}


class UserLoggedInForm {

  def render(xhtml: Group): NodeSeq = {
    if (CurrentUser.get.isDefined) {
      bind("user", xhtml,
        "username" -> {
            <img src="/images/icons/user32.png" alt=" "/> ++ link("/user/show", () => CurrentSelectedUser.set(CurrentUser.get), Text(CurrentUser.get.get.lastName + ", " + CurrentUser.get.get.firstName) ++ <img src="/images/icons/about16.png" alt=" "/>)
        },
        "action" -> {
          link("/user/edit", () => CurrentSelectedUser.set(CurrentUser.get), <img src="/images/icons/change16.png" alt=" "/>) ++ <br/>
        },
        "logInOut" -> link("/login", () => {
          CurrentUser(None)
          CurrentTrial(None)
          CurrentSelectedUser(None)
        }, Text("logout") ++ <img src="/images/arrowleft16.png" alt=" "/>)
      )
    } else {
      bind("user", xhtml,
        "username" -> "",
        "action" -> "",
        "logInOut" -> ""
      )
    }

  }
}
