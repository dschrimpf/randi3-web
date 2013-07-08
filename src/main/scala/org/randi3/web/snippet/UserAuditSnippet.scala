package org.randi3.web.snippet


import scala.xml.NodeSeq

import org.randi3.web.lib.DependencyFactory

import net.liftweb.http.S._


import org.randi3.web.util.{CurrentUser, CurrentTrial}
import org.joda.time.format.DateTimeFormat

class UserAuditSnippet {


  def render: NodeSeq = {
    val user = CurrentUser.get.getOrElse {
      redirectTo("/user/list")
    }

    DependencyFactory.get.auditService.getAudit(user.username).toEither match {
      case Left(x) => <div>
        {x}
      </div>
      case Right(entries) => {
        <table class="randi2Table">
          <thead>
            <tr>
              <th>Time</th>
              <th>User</th>
              <th>Action</th>
              <th>Type</th>
              <th>Identifier</th>
              <th>Text</th>
            </tr>
          </thead>{if (entries.isEmpty) {
          <tfoot>
            <tr>
              <td colspan="2">no audit available</td>
            </tr>
          </tfoot>
        } else {
          <tfoot></tfoot>
        }}<tbody>
          {entries.flatMap(entry => {
            <tr>
              <td>
                {entry.time.toString(DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"))}
              </td>
              <td>
                {entry.username}
              </td>
              <td>
                {entry.action.toString}
              </td>
              <td>
                {entry.clazz.getSimpleName}
              </td>
              <td>
                {entry.identifier}
              </td>
              <td>
                {entry.text}
              </td>
            </tr>
          })}
        </tbody>
        </table>
      }
    }

  }

}
