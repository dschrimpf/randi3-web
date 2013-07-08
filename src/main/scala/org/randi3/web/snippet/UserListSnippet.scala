package org.randi3.web.snippet

import xml.{Text, NodeSeq}
import org.randi3.web.util.{CurrentUser, CurrentLoggedInUser}
import org.randi3.web.lib.DependencyFactory
import net.liftweb.http.SHtml._
import net.liftweb.http.S


class UserListSnippet {


  private val userService = DependencyFactory.get.userService

  def list(nodeSeq: NodeSeq): NodeSeq  = {
      val currentUser = CurrentLoggedInUser.get.get
      userService.getAll.toEither match {
        case Left(x) => <tr>
          <td colspan="9">
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
          <td>
            {user.isActive}
          </td>
          <td>
            {if (currentUser.administrator) link("/user/show", () => CurrentUser.set(Some(user)), Text(S.?("show")))}
          </td>
          <td>
            {if (currentUser.administrator) link("/user/edit", () => CurrentUser.set(Some(user)), Text(S.?("edit")))}
          </td>
        </tr>)
      }
  }
}
