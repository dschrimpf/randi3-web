package org.randi3.web.snippet

import scala.xml.NodeSeq
import org.randi3.web.util.CurrentLoggedInUser
import net.liftweb.http.S._

class IndexSnippet {
 def loggedIn(html: NodeSeq) =
   if (CurrentLoggedInUser.get.isDefined) html else NodeSeq.Empty

 def loggedOut(html: NodeSeq) =
   if (CurrentLoggedInUser.get.isEmpty) redirectTo("login") else NodeSeq.Empty
}