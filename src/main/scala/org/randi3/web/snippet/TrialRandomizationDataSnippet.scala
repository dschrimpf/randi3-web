package org.randi3.web.snippet

import org.randi3.web.util.{ CurrentLoggedInUser, CurrentTrial }
import net.liftweb.http.S._
import xml.{ Elem, NodeSeq }
import org.randi3.model.criterion.constraint.Constraint
import net.liftweb.util.Helpers._
import org.joda.time.format.DateTimeFormat
import org.randi3.model._
import org.randi3.model.criterion.{ DateCriterion, Criterion }
import org.joda.time.{ DateTime, LocalDate }
import net.liftweb.common.{ Box, Full }
import net.liftweb.http.js.JE.JsObj
import net.liftmodules.widgets.flot._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._
import js.JE
import java.util.{ ArrayList, Date }
import collection.mutable.{ ListBuffer, HashMap }
import collection.mutable
import rest.RestHelper
import net.liftweb.http.StreamingResponse
import net.liftweb.common.Full
import scala.Some
import org.randi3.web.util.CurrentEDCTrial
import org.randi3.web.util.CurrentLocalEDCTrial

class TrialRandomizationDataSnippet extends TrialViewHelper {

  def show(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }

    bind("trial", in,
      "randomizationDataTable" -> randomizationDataTable(trial),
      "treatmentArmChart" -> treatmentArmChart(in, trial),
      "treatmentArmTable" -> treatmentArmTable(trial),
      "trialSitesChart" -> trialSitesChart(in, trial),
      "trialSitesTable" -> trialSitesTable(trial),
      "recruitmentChart" -> recruitmentChart(in, trial))
  }

  def showInvestigator(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }

    bind("trial", in,
      "randomizationDataTable" -> randomizationDataTable(trial),
      "randomizationDataFile" -> <a href="/downloadRandomizationData">Download</a>)
  }

  def treatmentArmTable(trial: Trial): Elem = {
    <table class="randi2Table">
      <thead>
        <tr>
          <th>{ S.?("treatmentArms") }</th>
          <th>{ S.?("currentSize") }</th>
          <th>{ S.?("trial.plannedSubjectSize") }</th>
          <th>{ S.?("perCent") }</th>
        </tr>
      </thead>{
        if (trial.treatmentArms.isEmpty)
          <tfoot>{ S.?("trial.noArmsDefined") }</tfoot>
      }<tbody>
         {
           trial.treatmentArms.flatMap(arm => {
             <tr>
               <td>
                 { arm.name }
               </td>
               <td>
                 { arm.subjects.size }
               </td>
               <td>
                 { arm.plannedSize }
               </td>
               <td>
                 { ((arm.subjects.size * 1.0 / arm.plannedSize) * 100) + "%" }
               </td>
             </tr>
           })
         }
       </tbody>
    </table>
  }

}

object DownloadRandomizationData extends RestHelper {

  def randomizationDataFile(edcTrial: Boolean = false): LiftResponse = {
    val user = CurrentLoggedInUser.get.getOrElse(redirectTo("/trial/list"))
    val trial = if(edcTrial)
                 CurrentLocalEDCTrial.get.getOrElse(redirectTo("")).trial.getOrElse(redirectTo(""))
                else  
                  CurrentTrial.get.getOrElse (redirectTo("/trial/list"))
                  
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    val roles = rightList.map(right => right.role)
    val result = new mutable.StringBuilder()
    //no or not the necessary rights
    if (rightList.isEmpty || !(roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor) || roles.contains(Role.investigator))) {
      result.append("No right to download the data")
    } else {

      val criterions: List[Criterion[_, Constraint[_]]] =
        if (trial.criterions.isEmpty) Nil
        else if (trial.criterions.size == 1) trial.criterions
        else {
          trial.criterions.sortWith((elem1, elem2) => elem1.name.compareTo(elem2.name) < 0)
        }

      //CSV Header
      result.append("Created At; Identifier; Treatment; Site; Investigator; ")
      result.append(
        if (criterions.size > 1) {
          criterions.map(criterion => criterion.name).reduce((acc, elem) => acc + "; " + elem.name) + "; "
        } else if (criterions.size == 1) {
          criterions(0).name + "; "
        } else "")

      trial.stages.map(stage => stage).toList.sortBy(_._1).foreach(stage => {
        stage._2.foreach(criterion => {
          result.append(stage._1 + "_" + criterion.name + "; ")
        })
      })
      result.append("\n")

      val subjectList = new ListBuffer[(TreatmentArm, TrialSubject)]()
      trial.treatmentArms.foreach(arm =>
        arm.subjects.foreach(subject => subjectList.append((arm, subject))))
      subjectList.toList.sortWith((subject1, subject2) => subject1._2.createdAt.isBefore(subject2._2.createdAt)).foreach(element => {
        result.append(element._2.createdAt.toString(DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")) + "; ")
        result.append(element._2.identifier + "; ")
        result.append(element._1.name + "; ")
        result.append(element._2.trialSite.name + "; ")
        result.append(element._2.investigatorUserName + "; ")
        result.append(propertiesEntryCSV(element._2.properties, criterions))
        result.append(stagePropertiesEntryCSV(trial.stages, element._2))
        result.append("\n")

      })

    }
    val data = result.toString().getBytes

    val headers =
      ("Content-type" -> "text/csv") :: ("Content-length" -> data.length.toString) :: Nil
    StreamingResponse(
      new java.io.ByteArrayInputStream(data),
      () => {},
      data.length,
      headers, Nil, 200)

  }

  private def propertiesEntryCSV(properties: List[SubjectProperty[_]], criterions: List[Criterion[_, Constraint[_]]]): String = {
    val result = new mutable.StringBuilder()
    criterions.foreach(criterion => {
      val prop = properties.find(property => property.criterion.name == criterion.name)
      if (prop.isDefined) {

        if (prop.get.value.isInstanceOf[Date]) {
          val value = new DateTime(prop.get.value.asInstanceOf[Date].getTime)
          result.append(value.toString(DateTimeFormat.forPattern("yyyy-MM-dd")))
        } else {
          result.append(prop.get.value)
        }

      } else {
        result.append("NA")
      }
      result.append("; ")
    })

    result.toString()
  }

  private def stagePropertiesEntryCSV(stages: Map[String, List[Criterion[_, Constraint[_]]]], trialSubject: TrialSubject): String = {
    val resultLine = new mutable.StringBuilder()

    stages.map(stage => stage).toList.sortBy(_._1).foreach(stage => {
      stage._2.foreach(criterion => {
        resultLine.append({
          val subjectStage = trialSubject.stages.get(stage._1)

          val properties = subjectStage.getOrElse(List())

          val prop = properties.find(property => property.criterion.name == criterion.name)
          if (prop.isDefined) {

            if (prop.get.value.isInstanceOf[Date]) {
              val value = new DateTime(prop.get.value.asInstanceOf[Date].getTime)
              value.toString(DateTimeFormat.forPattern("yyyy-MM-dd"))
            } else {
              prop.get.value
            }

          } else {
            "NA"
          }
        } + "; ")

      })
    })
    resultLine.toString()
  }

  serve {
    case Req("downloadRandomizationData" :: Nil, _, _) =>
      randomizationDataFile()
    case Req("downloadRandomizationDataEDCTrial" :: Nil, _, _) =>
      randomizationDataFile(true)
  }
}

