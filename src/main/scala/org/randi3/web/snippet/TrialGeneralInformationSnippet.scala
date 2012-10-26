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
import org.randi3.model.StratifiedTrialSite


object TrialGeneralInformationSnippet {

  private val randomizationPluginManager = DependencyFactory.randomizationPluginManager

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

    def algorithm: Elem = {
      <div>
        {
        if(trial.randomizationMethod.isEmpty){
          <span>No algorithm defined</span>
          }else {
         val method = trial.randomizationMethod.get
         val plugin = randomizationPluginManager.getPluginForMethod(method).get
         val configuration = plugin.getRandomizationConfigurations(method.id)
          <div>
          <div>
            <h4>{plugin.i18nName}</h4>
            {plugin.description}
          </div>
            <br />
            <h5>Configuration:</h5>
            {configuration.flatMap(conf =>{
            val info = conf.configurationType.description
            <div>
              {conf.configurationType.name}
              <span class="tooltip">
                <img src="/images/icons/help16.png" alt={info} title={info}/>
                <span class="info">
                {info}
                </span>
              </span>
              = {conf.value}
            </div>
          }
            )}
            <br />
            <h5>Stratification:</h5>
            {
            <div>
              Trial site stratification = {trial.stratifyTrialSite.toString}
            <br />
            <h6>Strata</h6>
            {trial.criterions.flatMap(criterion =>
            {if(!criterion.strata.isEmpty)
            <div>
              <h6>{criterion.name}:</h6>
              {criterion.strata.flatMap(constraint =>
               <div>{constraint.configurations.toString}</div>
               )}
            </div>
              else <span></span>
            }
            )}
            </div>

            }
          </div>
         }
        }
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
      "algorithm" -> algorithm
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
