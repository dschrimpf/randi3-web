package org.randi3.web.snippet


import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq, Group}

import net.liftweb.http.SHtml._
import net.liftweb.http._
import org.randi3.web.util.{CurrentTrialSite, CurrentUser, CurrentTrial, CurrentLoggedInUser}


class UserLoggedInForm {

  def render(xhtml: Group): NodeSeq = {
    if (CurrentLoggedInUser.get.isDefined) {
      bind("user", xhtml,
        "username" -> {
            <img src="/images/icons/user32.png" alt=" "/> ++ link("/user/show", () => CurrentUser.set(CurrentLoggedInUser.get), Text(CurrentLoggedInUser.get.get.lastName + ", " + CurrentLoggedInUser.get.get.firstName) ++ <img src="/images/icons/about16.png" alt=" "/>)
        },
        "action" -> {
          link("/user/edit", () => CurrentUser.set(CurrentLoggedInUser.get), <img src="/images/icons/change16.png" alt=" "/>) ++ <br/>
        },
        "logInOut" -> link("/login", () => {
          CurrentLoggedInUser(None)
          CurrentTrial(None)
          CurrentTrialSite(None)
          CurrentUser(None)
        }, Text(S.?("logout")) ++ <img src="/images/arrowleft16.png" alt=" "/>)
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
