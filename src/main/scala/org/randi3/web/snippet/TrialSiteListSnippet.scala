package org.randi3.web.snippet

import xml.{Text, NodeSeq}
import org.randi3.web.util.{CurrentTrialSite, CurrentLoggedInUser}

import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import org.randi3.web.lib.DependencyFactory
import net.liftweb.http.S


class TrialSiteListSnippet {

  private val trialSiteService = DependencyFactory.get.trialSiteService


  def list(xhtml: NodeSeq): NodeSeq = {
    val currentUser = CurrentLoggedInUser.getOrElse(
      redirectTo("/index")
    )
    trialSiteService.getAll.either match {
      case Left(x) => <tr>
        <td colspan="7">
          {x}
        </td>
      </tr> //TODO show Failure
      case Right(trialSites) => trialSites.flatMap(trialSite => <tr>
        <td>
          {trialSite.name}
        </td>
        <td>
          {trialSite.street}
        </td>
        <td>
          {trialSite.postCode}
        </td>
        <td>
          {trialSite.city}
        </td>
        <td>
          {trialSite.country}
        </td>
        <td>
          {trialSite.isActive}
        </td>
        <td>
          {if (currentUser.administrator) {
          link("/trialSite/edit", () => CurrentTrialSite.set(Some(trialSite)), Text(S.?("edit")))
        }}
        </td>
      </tr>)
    }
  }
}
