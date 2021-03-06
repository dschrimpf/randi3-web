package org.randi3.web.snippet

import xml.{Elem, NodeSeq}
import org.randi3.web.util.CurrentUser
import net.liftweb.util.Helpers._
import net.liftweb.http.S._
import net.liftweb.http.S
import net.liftweb.http.SHtml._

class UserShowSnippet extends GeneralFormSnippet{


  def show(xhtml: NodeSeq): NodeSeq = {
    val user = CurrentUser.get.getOrElse(redirectTo("/user/list"))

    def trialSiteInfo: Elem = {

      <div id="trialSiteInfo">
        <div>
          <span class="elementLeft">{S.?("name")}:</span>
          <span class="elementRight">
            {user.site.name}
          </span>
        </div>
        <div>
          <span class="elementLeft">{S.?("street")}:</span>
          <span class="elementRight">
            {user.site.street}
          </span>
        </div>
        <div>
          <span class="elementLeft">{S.?("postCode")}:</span>
          <span class="elementRight">
            {user.site.postCode}
          </span>
        </div>
        <div>
          <span class="elementLeft">{S.?("city")}:</span>
          <span class="elementRight">
            {user.site.city}
          </span>
        </div>
        <div>
          <span class="elementLeft">{S.?("country")}:</span>
          <span class="elementRight">
            {user.site.country}
          </span>
        </div>
      </div>
    }

    def rights: Elem = {
      <table id="rights" class="randi2Table">
        <thead>
          <tr>
            <th>{S.?("trial")}</th>
            <th>{S.?("role")}</th>
          </tr>
        </thead>{if (!user.rights.isEmpty) {
        <tfoot></tfoot>
      } else {
        <tfoot>
          <tr>
            <td></td>
            No rights defined</tr>
        </tfoot>
      }}<tbody>
        {user.rights.toList.sortWith((a, b) => a.trial.name.compareToIgnoreCase(b.trial.name) < 0).flatMap(right => {
          <tr>
            <td>
              {right.trial.name}
            </td>
            <td>
              {right.role.toString}
            </td>
          </tr>
        }
        )}
      </tbody>
      </table>

    }

    bind("user", xhtml,
      "info" -> <span>{user.username}</span> ,
      "username" -> generateEntry("username", false, {
        <span>
          {user.username}
        </span>
      }),
      "firstName" -> generateEntry("firstName", false, {
        <span>
          {user.firstName}
        </span>
      }),
      "lastName" -> generateEntry("lastName", false, {
        <span>
          {user.lastName}
        </span>
      }),
      "email" -> generateEntry("email", false, {
        <span>
          {user.email}
        </span>
      }),
      "phoneNumber" -> generateEntry("phoneNumber", false, {
        <span>
          {user.phoneNumber}
        </span>
      }),
      "locale" -> <span>{
        if (user.locale.getDisplayCountry.isEmpty)
        {
          user.locale.getDisplayLanguage
        }
        else
        {
          user.locale.getDisplayLanguage+ " ("+ user.locale.getDisplayCountry +")"
        }
        }</span>,
      "trialSite" -> generateEntry("username7", false, {
        <span>
          {user.username}
        </span>
      }),
      "trialSiteInfo" -> trialSiteInfo,
      "administrator" -> generateEntry("user.isUserAdministrator", false, {
        <span>{checkbox(user.administrator, () => _ , "id" -> "user.isUserAdministrator", "disabled" -> "disabled")}</span>
      }),
      "canCreateTrials" -> generateEntry("canCreateTrials", false, {
        <span>{checkbox(user.canCreateTrial, () => _ , "id" -> "canCreateTrial", "disabled" -> "disabled")}</span>
      }),
      "rights" -> rights,
      "isActive" -> generateEntry("isActive", false, {
        <span>
          {user.isActive}
        </span>
      }),
      "numberOfFailedLogins" -> generateEntry("numberOfFailedLogins", false, {
        <span>
          {user.numberOfFailedLogins}
        </span>
      }),
    "edit" -> submit(S.?("user.edit"), () => S.redirectTo("/user/edit"), "class" -> "btnSend") ,
      "lockedUntil" -> generateEntry("lockedUntil", false, {
        <span>
          {if(user.lockedUntil.isDefined) user.lockedUntil.get else <span>---</span>}
        </span>
      })
    )
  }
}
