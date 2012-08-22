package org.randi3.web.snippet

import scala.xml.NodeSeq
import org.randi3.web.util.CurrentUser
import net.liftweb.http.S._

class IndexSnippet {
 def loggedIn(html: NodeSeq) =
   if (CurrentUser.get.isDefined) html else NodeSeq.Empty

 def loggedOut(html: NodeSeq) =
   if (CurrentUser.get.isEmpty) redirectTo("login") else NodeSeq.Empty
}