package org.randi3.web.snippet

import xml.{NodeSeq, Text}
import org.randi3.web.lib.DependencyFactory
import org.joda.time.format.DateTimeFormat


import net.liftweb._

import http.S
import http.SHtml._
import org.randi3.web.util.CurrentTrial

class TrialListSnippet {

  private val trialService = DependencyFactory.trialService

  def show(in: NodeSeq): NodeSeq = {
    trialService.getAll.either match {
      case Left(x) => <tr>
        <td colspan="8">
          {x}
        </td>
      </tr>
      case Right(trials) => {
        trials.flatMap(trial => {
          <tr>
            <td>
              {trial.abbreviation}
            </td>
            <td>
              {trial.name}
            </td>
            <td>
              {trial.status}
            </td>
            <td>
              {trial.startDate.toString(DateTimeFormat.forPattern("yyyy-MM-dd"))}
            </td>
            <td>
              {trial.endDate.toString(DateTimeFormat.forPattern("yyyy-MM-dd"))}
            </td>
            <td>
              {trial.description}
            </td>
            <td>
              {link("/trial/generalInformation", () => {
              CurrentTrial.set(Some(trialService.get(trial.id).toOption.get.get))
            }, Text(S.?("select"))) /*TODO error handling*/}
            </td>
          </tr>
        })
      }
    }
  }
}
