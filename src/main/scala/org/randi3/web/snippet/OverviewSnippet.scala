package org.randi3.web.snippet

import org.randi3.web.lib.DependencyFactory
import xml.{Text, NodeSeq}
import net.liftweb.http.{SHtml, S}
import S._
import SHtml._
import org.randi3.web.util.{CurrentTrial, CurrentLoggedInUser}
import org.randi3.model.{TrialStatus, Role}


class OverviewSnippet {

  val userService = DependencyFactory.get.userService
  val trialService = DependencyFactory.get.trialService

  val user = CurrentLoggedInUser.get.get

  def overview(xhtml: NodeSeq): NodeSeq = {
    <div>
      <form>
        {createObjectOptions}{randomizeOptions}{trialOverviewOptions}

      </form>
    </div>
  }


  def createObjectOptions: NodeSeq = {
    if (user.administrator || user.canCreateTrial) {
      <fieldset>
        <legend>{S.?("overview.createObjects")}</legend>
        <table class="randi2Table">
          <thead>
            <tr>
              <th>{S.?("overview.object")}</th>
            </tr>
          </thead>
          <tfoot>

          </tfoot>
          <tbody>{if (user.administrator) {
          <tr>
            <td>
              {link("/user/add", () => {}, Text(S.?("overview.addNewUser")))}
            </td>
          </tr>
            <tr>
              <td>
                {link("/trialSite/add", () => {}, Text(S.?("overview.addNewTrialSite")))}
              </td>
            </tr>
        }}{if (user.canCreateTrial) {
          <tr>
            <td>
              {link("/trial/add", () => {}, Text(S.?("overview.addNewTrial")))}
            </td>
          </tr>
        }}
        </tbody>
        </table>
      </fieldset>
    } else <div></div>

  }

  def randomizeOptions: NodeSeq = {
    val trials = trialService.getAll.toOption.get
    val trialsWithRandomizeRight = trials.filter(trial => trial.status == TrialStatus.ACTIVE && user.rights.filter(right => right.trial.id == trial.id).map(right => right.role).contains(Role.investigator))
    if (!trialsWithRandomizeRight.isEmpty) {
    <fieldset>
      <legend>{S.?("overview.randomizePatient")}</legend>
      <table class="randi2Table">
        <thead>
          <tr>
            <th>{S.?("trial.abbreviation")}</th>
            <th>{S.?("trial.name")}</th>
            <th></th>
          </tr>
        </thead>
        <tfoot></tfoot>
        <tbody>
          {trialsWithRandomizeRight.flatMap(trial => {
          <tr>
            <td>{trial.abbreviation}</td>
            <td>{trial.name}</td>
            <td>{link("/trialSubject/randomize", () => CurrentTrial.set(Some(trialService.get(trial.id).toOption.get.get)), Text(S.?("randomize")))}</td>
          </tr>
        })}
        </tbody>
      </table>
    </fieldset>
    }else <div></div>
  }

  def trialOverviewOptions: NodeSeq = {
    val trials = trialService.getAll.toOption.get
    val trialsWithRandomizeRight = trials.filter(trial => !user.rights.filter(right => right.trial.id == trial.id).map(right => right.role).isEmpty)
    if (!trialsWithRandomizeRight.isEmpty) {
      <fieldset>
        <legend>{S.?("overview.viewTrialInformation")}</legend>
        <table class="randi2Table">
          <thead>
            <tr>
              <th>{S.?("trial.abbreviation")}</th>
              <th>{S.?("trial.name")}</th>
              <th>{S.?("trial.status")}</th>
              <th></th>
            </tr>
          </thead>
          <tfoot></tfoot>
          <tbody>
            {trialsWithRandomizeRight.flatMap(trial => {
            <tr>
              <td>{trial.abbreviation}</td>
              <td>{trial.name}</td>
              <td>{trial.status.toString}</td>
              <td>{link("/trial/generalInformation", () => CurrentTrial.set(Some(trialService.get(trial.id).toOption.get.get)), Text(S.?("show")))}</td>
            </tr>
          })}
          </tbody>
        </table>
      </fieldset>
    }else <div></div>
  }
}
