package org.randi3.web.snippet

import org.randi3.web.util.{CurrentUser, CurrentTrial}
import net.liftweb.http.S._
import xml.{Elem, NodeSeq}
import org.randi3.model.criterion.constraint.Constraint
import net.liftweb.util.Helpers._
import org.joda.time.format.DateTimeFormat
import org.randi3.model.{Role, SubjectProperty, Trial}
import org.randi3.model.criterion.{DateCriterion, Criterion}
import org.joda.time.{DateTime, LocalDate}
import net.liftweb.common.{Box, Full}
import net.liftweb.http.js.JE.JsObj
import net.liftweb.widgets.flot._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._
import js.JE
import java.util.{ArrayList, Date}
import collection.mutable.{ListBuffer, HashMap}
import collection.mutable
import rest.RestHelper


class TrialRandomizationDataSnippet {


  def show(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }



    bind("trial", in,
      "randomizationDataTable" -> randomizationDataTable(trial),
      "randomizationDataFile" -> <a href="/downloadRandomizationData">Download</a>,
      "treatmentArmChart" -> treatmentArmChart(in, trial),
      "treatmentArmTable" -> treatmentArmTable(trial),
      "trialSitesChart" -> trialSitesChart(in, trial),
      "trialSitesTable" -> trialSitesTable(trial),
      "recruitmentChart" -> recruitmentChart(in, trial)
    )
  }

  def showInvestigator(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }

    bind("trial", in,
      "randomizationDataTable" -> randomizationDataTable(trial),
      "randomizationDataFile" -> <a href="/downloadRandomizationData">Download</a>
    )
  }

  def treatmentArmTable(trial: Trial): Elem = {
    <table class="randi2Table">
      <thead>
        <tr>
          <th>Arm</th>
          <th>Current size</th>
          <th>Planned Size</th>
          <th>Fill level</th>
        </tr>
      </thead>{if (trial.treatmentArms.isEmpty)
      <tfoot>no treatment arms available</tfoot>}<tbody>
      {trial.treatmentArms.flatMap(arm => {
        <tr>
          <td>
            {arm.name}
          </td>
          <td>
            {arm.subjects.size}
          </td>
          <td>
            {arm.plannedSize}
          </td>
          <td>
            {((arm.subjects.size * 1.0 / arm.plannedSize) * 100) + "%"}
          </td>
        </tr>
      })}
    </tbody>
    </table>
  }

  def treatmentArmChart(xhtml: NodeSeq, trial: Trial): NodeSeq = {

    val data = trial.treatmentArms.map(arm => arm.subjects.size)
    val bar_labels = trial.treatmentArms.map(arm => arm.name)

    val data_to_plot = for ((y, x) <- data zipWithIndex) yield new FlotSerie() {
      override val data: List[(Double, Double)] = (x.toDouble, y.toDouble) :: Nil
      override val label = Full(bar_labels(x))
      override val bars = Full(new FlotBarsOptions() {
        override val show = Full(true)
        override val barWidth = Full(1)
      })

    }

    val options: FlotOptions = new FlotOptions() {
      override val xaxis = Full(new FlotAxisOptions() {
        override def min = Some(0d)

        override def max = Some(data.length * 1d)

        override def tickDecimals = Some(-1d)
      })

      override val yaxis = Full(new FlotAxisOptions() {
        override def min = Some(0d)
      })


      override def grid = Full(new FlotGridOptions() {
        override def hoverable = Full(true)
      })

    }

    Flot.render("treatmentArmChart_area", data_to_plot, options, Flot.script(xhtml))
  }


  def trialSitesTable(trial: Trial): Elem = {
    <table class="randi2Table">
      <thead>
        <tr>
          <th>Trial site</th>
          <th>Trial subject count</th>
        </tr>
      </thead>{if (trial.treatmentArms.isEmpty)
      <tfoot>no treatment arms available</tfoot>}<tbody>
      {trial.participatingSites.flatMap(site => {
        <tr>
          <td>
            {site.name}
          </td>
          <td>
            {trial.getSubjects.filter(subject => subject.trialSite == site).size}
          </td>
        </tr>
      })}
    </tbody>
    </table>
  }


  def trialSitesChart(xhtml: NodeSeq, trial: Trial): NodeSeq = {

    val data = trial.participatingSites.map(site => trial.getSubjects.filter(subject => subject.trialSite == site).size)
    val bar_labels = trial.participatingSites.map(site => site.name)

    val data_to_plot = for ((y, x) <- data zipWithIndex) yield new FlotSerie() {
      override val data: List[(Double, Double)] = (x.toDouble, y.toDouble) :: Nil
      override val label = Full(bar_labels(x))
      override val bars = Full(new FlotBarsOptions() {
        override val show = Full(true)
        override val barWidth = Full(1)
      })

    }

    val options: FlotOptions = new FlotOptions() {
      override val xaxis = Full(new FlotAxisOptions() {
        override def min = Some(0d)

        override def max = Some(data.length * 1d)

        override def tickDecimals = Some(-1d)
      })

      override val yaxis = Full(new FlotAxisOptions() {
        override def min = Some(0d)
      })


      override def grid = Full(new FlotGridOptions() {
        override def hoverable = Full(true)
      })

    }

    Flot.render("trialSitesChart_area", data_to_plot, options, Flot.script(xhtml))
  }


  def randomizationDataTable(trial: Trial): Elem = {
    val criterions: List[Criterion[_, Constraint[_]]] =
      if (trial.criterions.isEmpty) Nil
      else if (trial.criterions.size == 1) trial.criterions
      else {
        trial.criterions.sortWith((elem1, elem2) => elem1.name.compareTo(elem2.name) < 0)
      }

    <table class="randi2Table">
      <thead>
        <tr>
          <th>Created At</th>
          <th>Identifier</th>
          <th>Treatment</th>
          <th>Site</th>
          <th>Investigator</th>{propertiesHeader(criterions)}
        </tr>
      </thead>{if (trial.getSubjects.isEmpty)
      <tfoot>no subjects available</tfoot>}<tbody>
      {//sort subjects
      trial.getSubjects.sortWith((subject1, subject2) => subject1.createdAt.isBefore(subject2.createdAt)).map(subject => {
        val arm = trial.treatmentArms.find(arm => arm.subjects.contains(subject)).get
        (arm, subject)
      }).flatMap(pair => {
        <tr>
          <td>
            {pair._2.createdAt.toString(DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"))}
          </td>
          <td>
            {pair._2.identifier}
          </td>
          <td>
            {pair._1.name}
          </td>
          <td>
            {pair._2.trialSite.name}
          </td>
          <td>
            {pair._2.investigatorUserName}
          </td>{propertiesEntry(pair._2.properties, criterions)}
        </tr>
      })}
    </tbody>
    </table>
  }


  def recruitmentChart(xhtml: NodeSeq, trial: Trial): NodeSeq = {


    val recruitments = new HashMap[LocalDate, Int]()
    trial.getSubjects.foreach(subject => {
      val date = subject.createdAt.toLocalDate
      val count = recruitments.get(date).getOrElse(0)
      recruitments.put(date, count + 1)
    })

    val dataSet = recruitments.keySet.toList.sortWith((date1, date2) => date1.toDate.before(date2.toDate)).map(
      key =>
        (key.toDate.getTime.toDouble, recruitments.get(key).get.toDouble)
    ) ::: List((trial.endDate.toDate.getTime.toDouble, trial.getSubjects.size.toDouble))

    val estimatedDataSet = List((trial.startDate.toDate.getTime.toDouble, 0d), (trial.endDate.toDate.getTime.toDouble, trial.plannedSubjects.toDouble))

    val data_to_plot = List(new FlotSerie() {
      override val data: List[(Double, Double)] = dataSet
      override val label = Full("current recruitment")
      override val lines = Full(new FlotLinesOptions {
        override val show = Full(true)
      })

      override val points = Full(new FlotPointsOptions() {
        override val show = Full(true)
      })

    }, new FlotSerie() {
      override val data: List[(Double, Double)] = estimatedDataSet
      override val label = Full("estimated recruitment")
      override val lines = Full(new FlotLinesOptions {
        override val show = Full(true)
      })
    })


    val options: FlotOptions = new FlotOptions() {
      override val xaxis = Full(new FlotAxisOptions() {
        override def min = Some(trial.startDate.toDate.getTime.toDouble)

        override def mode = Some("time")

      })

      override val yaxis = Full(new FlotAxisOptions() {
        override def min = Some(0d)

        override def max = Some(trial.plannedSubjects.toDouble)
      })


      override def grid = Full(new FlotGridOptions() {
        override def hoverable = Full(true)
      })

    }

    Flot.render("recruitmentChart_area", data_to_plot, options, Flot.script(xhtml))
  }


  private def propertiesHeader(criterions: List[Criterion[_, Constraint[_]]]): NodeSeq = {
    criterions.flatMap(criterion => {
      <th>
        {criterion.name}
      </th>
    })
  }

  private def propertiesEntry(properties: List[SubjectProperty[_]], criterions: List[Criterion[_, Constraint[_]]]): NodeSeq = {
    criterions.flatMap(criterion => {
      <td>
        {val prop = properties.find(property => property.criterion.name == criterion.name)
      if (prop.isDefined) {

        if (prop.get.value.isInstanceOf[Date]) {
          val value = new DateTime(prop.get.value.asInstanceOf[Date].getTime)
          value.toString(DateTimeFormat.forPattern("yyyy-MM-dd"))
        } else {
          prop.get.value
        }

      } else {
        "-"
      }}
      </td>
    })
  }


}

object DownloadRandomizationData extends RestHelper {

  def randomizationDataFile(): LiftResponse = {
    val user = CurrentUser.get.getOrElse(redirectTo("/trial/list"))
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }

    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    val roles = rightList.map(right => right.role)
    val result = new mutable.StringBuilder()
    //no or not the necessary rights
    if (rightList.isEmpty || !(roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor) || roles.contains(Role.investigator))) {
      result.append("No right to dowload the data")
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

      result.append("\n")

      trial.treatmentArms.foreach(arm => {
        arm.subjects.foreach(subject => {
          result.append(subject.createdAt.toString(DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")) + "; ")
          result.append(subject.identifier + "; ")
          result.append(arm.name + "; ")
          result.append(subject.trialSite.name + "; ")
          result.append(subject.investigatorUserName + "; ")
          result.append(propertiesEntryCSV(subject.properties, criterions))
          result.append("\n")

        })
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
        result.append("-")
      }
      result.append("; ")
    })

    result.toString()
  }

  serve {
    case Req("downloadRandomizationData" :: Nil, _, _) =>
      randomizationDataFile()
  }
}

