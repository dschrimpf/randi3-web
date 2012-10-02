package org.randi3.web.snippet

import org.randi3.web.util.CurrentTrial
import scala.xml._
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.Text
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq}
import net.liftweb.util._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._
import js.JE

import org.joda.time.format.DateTimeFormat
import org.randi3.web.lib.DependencyFactory
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint


object TrialGeneralInformationSnippet {


  def show(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }
    def trialSites: Elem = {
      <table class="randi2Table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Country</th>
          </tr>
        </thead>{if (trial.participatingSites.isEmpty) {
        <tfoot>
          <tr>
            <td colspan="2">no site defined</td>
          </tr>
        </tfoot>
      } else {
        <tfoot></tfoot>
      }}<tbody>
        {trial.participatingSites.flatMap(site => {
          <tr>
            <td>
              {site.name}
            </td>
            <td>
              {site.country}
            </td>
          </tr>
        })}
      </tbody>
      </table>
    }

    def treatmentArms: Elem = {
      <table class="randi2Table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Description</th>
            <th>Planned subject size</th>
          </tr>
        </thead>{if (trial.treatmentArms.isEmpty) {
        <tfoot>
          <tr>
            <td colspan="3">no arms defined</td>
          </tr>
        </tfoot>
      } else {
        <tfoot></tfoot>
      }}<tbody>
        {trial.treatmentArms.flatMap(arm => {
          <tr>
            <td>
              {arm.name}
            </td>
            <td>
              {arm.description}
            </td>
            <td>
              {arm.plannedSize}
            </td>
          </tr>
        })}
      </tbody>
      </table>
    }

    def properties: Elem = {
      getPropertiesTable(trial.criterions)
    }

    def stages: Elem = {
      <div>
        {trial.stages.flatMap(element => {
        <div>

          <h4>
            {element._1}
          </h4>{getPropertiesTable(element._2)}
        </div>

      })}
      </div>
    }

    bind("trial", in,
      "abbreviation" -> trial.abbreviation,
      "status" -> trial.status.toString,
      "name" -> trial.name,
      "description" -> trial.description,
      "startDate" -> trial.startDate.toString(DateTimeFormat.forPattern("yyyy-MM-dd")),
      "endDate" -> trial.endDate.toString(DateTimeFormat.forPattern("yyyy-MM-dd")),
      "sites" -> trialSites,
      "treatmentArms" -> treatmentArms,
      "properties" -> properties,
      "stages" -> stages,
      "algorithm" -> trial.randomizationMethod.toString
    )
  }


  private def getPropertiesTable(criterions: List[Criterion[_, Constraint[_]]]): Elem = {
    <table class="randi2Table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Description</th>
          <th>Inclusion Constraint</th>
        </tr>
      </thead>{if (criterions.isEmpty) {
      <tfoot>
        <tr>
          <td colspan="2">no criterions defined</td>
        </tr>
      </tfoot>
    } else {
      <tfoot></tfoot>
    }}<tbody>
      {criterions.flatMap(criterion => {
        <tr>
          <td>
            {criterion.name}
          </td>
          <td>
            {criterion.getClass.getSimpleName}
          </td>
          <td>
            {criterion.description}
          </td>
          <td>
            {criterion.inclusionConstraint.toString}
          </td>
        </tr>
      })}
    </tbody>
    </table>
  }
}
