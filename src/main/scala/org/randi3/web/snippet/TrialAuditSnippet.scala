package org.randi3.web.snippet

import java.util.Date

import java.util.Locale
import net.liftweb.http.StatefulSnippet
import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import scala.xml._
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.Text

import org.randi3.web.lib.DependencyFactory

import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._
import js.JE

import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb._

import org.randi3.randomization._
import org.apache.commons.math3.random.MersenneTwister
import org.randi3.web.util.CurrentTrial
import org.joda.time.format.DateTimeFormat

class TrialAuditSnippet {


  def render: NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }

    DependencyFactory.auditService.getAudit(trial).either match {
      case Left(x) => <div>
        {x}
      </div>
      case Right(entries) => {
        <table class="randi2Table">
          <thead>
            <tr>
              <th>{S.?("time")}</th>
              <th>{S.?("username")}</th>
              <th>{S.?("action")}</th>
              <th>{S.?("type")}</th>
              <th>{S.?("identifier")}</th>
              <th>{S.?("description")}</th>
            </tr>
          </thead>{if (entries.isEmpty) {
          <tfoot>
            <tr>
              <td colspan="2">{S.?("trial.noAuditAvailable")}</td>
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
