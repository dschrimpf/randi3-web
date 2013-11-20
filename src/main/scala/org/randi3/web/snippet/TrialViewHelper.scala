package org.randi3.web.snippet

import scala.xml.{Elem, NodeSeq}
import org.randi3.model.{TrialSubject, SubjectProperty, Trial}
import net.liftmodules.widgets.flot._
import net.liftweb.common.Full
import net.liftweb.common.Full
import scala.Some
import net.liftweb.common.Full
import scala.Some
import net.liftweb.http.S
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.joda.time.format.DateTimeFormat
import scala.collection.mutable.HashMap
import org.joda.time.{DateTime, LocalDate}
import net.liftweb.common.Full
import scala.Some
import net.liftweb.common.Full
import scala.Some
import java.util.Date

trait TrialViewHelper {
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
    val trialSites = trial.getSubjects.map(subject => subject.trialSite).toSet.toList

    <table class="randi2Table">
      <thead>
        <tr>
          <th>
            {S.?("trialSite")}
          </th>
          <th>
            {S.?("currentSize")}
          </th>
        </tr>
      </thead>{if (trial.treatmentArms.isEmpty)
      <tfoot>
        {S.?("trial.noSiteDefined")}
      </tfoot>}<tbody>
      {trialSites.flatMap(site => {
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

    val trialSites = trial.getSubjects.map(subject => subject.trialSite).toSet.toList

    val data = trialSites.map(site => trial.getSubjects.filter(subject => subject.trialSite == site).size)
    val bar_labels = trialSites.map(site => site.name)

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
          <th>
            {S.?("createdAt")}
          </th>
          <th>
            {S.?("identifier")}
          </th>
          <th>
            {S.?("treatmentArm")}
          </th>
          <th>
            {S.?("trialSite")}
          </th>
          <th>
            {S.?("investigator")}
          </th>{propertiesHeader(criterions)}{stagesHeader(trial.stages)}
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
          </td>{propertiesEntry(pair._2.properties, criterions)}{stagesEntry(trial.stages, pair._2)}
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
      override val label = Full(S.?("currentRecruitment"))
      override val lines = Full(new FlotLinesOptions {
        override val show = Full(true)
      })

      override val points = Full(new FlotPointsOptions() {
        override val show = Full(true)
      })

    }, new FlotSerie() {
      override val data: List[(Double, Double)] = estimatedDataSet
      override val label = Full(S.?("estimatedRecruitment"))
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

  private def stagesHeader(stages: Map[String, List[Criterion[_, Constraint[_]]]]): NodeSeq = {
    stages.map(stage => stage).toList.sortBy(_._1).flatMap(stage => {
      stage._2.flatMap(criterion => {
        <th>
          {stage._1 + "_" + criterion.name}
        </th>
      })
    })
  }

  private def stagesEntry(stages: Map[String, List[Criterion[_, Constraint[_]]]], trialSubject: TrialSubject): NodeSeq = {
    stages.map(stage => stage).toList.sortBy(_._1).flatMap(stage => {
      stage._2.flatMap(criterion => {
        <td>
          {val subjectStage = trialSubject.stages.get(stage._1)

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
        }}
        </td>
      })
    })

  }

}
