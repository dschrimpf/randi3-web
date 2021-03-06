package org.randi3.web.snippet

import net.liftweb.http.{S, StatefulSnippet}
import collection.mutable.ListBuffer
import scala.xml.{Elem, Node, NodeSeq}
import org.randi3.web.util.{RandomizationResult, CurrentLoggedInUser, Utility, CurrentTrial}
import net.liftweb.http.S._
import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint.Constraint
import net.liftweb.http.SHtml._
import net.liftweb.common.{Full, Empty}
import org.randi3.model._
import net.liftweb.util.Helpers._
import scala.Left
import scala.Right
import scala.Some
import org.randi3.web.model.SubjectDataTmp
import org.randi3.web.lib.DependencyFactory
import org.joda.time.LocalDate
import scala.Some
import org.randi3.web.model.SubjectDataTmp
import scala.Some
import org.randi3.web.model.SubjectDataTmp
import net.liftweb.http.js.JsCmds.Replace


class ResponseSnippet extends StatefulSnippet with GeneralFormSnippet{


  private val trialService = DependencyFactory.get.trialService


  def dispatch = {
    case "addResponse" => addResponse _
  }



  private def addResponse(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      error("Trial not found")
      redirectTo("/trial/list")
    }

    var trialSubject: TrialSubject = null
    var stageName = ""
    var stageCriterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]] = List()
    var subjectDataList = new ListBuffer[SubjectDataTmp]()



    def saveStage() {
      val properties: List[SubjectProperty[Any]] = subjectDataList.toList.map(subjectData => SubjectProperty(criterion = subjectData.criterion, value = subjectData.value).toEither match {
        case Left(x) => null
        case Right(prop) => prop
      })

      if(!stageName.isEmpty && !properties.isEmpty && !properties.contains(null) && trialSubject != null) {

      trialService.addStage(trial, trialSubject, stageName, properties).toEither match {
        case Left(failure) => S.error("responseMsg", failure)
        case Right(trialSubject) => {
          val user = CurrentLoggedInUser.get.get

          val dbTrial = trialService.get(trial.id).toOption.get
          CurrentTrial.set(dbTrial)

          val rightList = user.rights.filter(right => right.trial.id == dbTrial.get.id)
          val roles = rightList.map(right => right.role)

          if (roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor)) {
            S.redirectTo("/trial/randomizationData")
          } else {
            S.redirectTo("/trial/randomizationDataInvestigator")
          }
      }
      }
     } else {
        S.error("responseMsg", "Add stage failed, some data is missing. Please repeat the data entry or contact an administrator.")
      }
    }


    def subjectField(failure: Boolean = false): Elem = {
      val id = "subjectIdentifier"
      trialSubject = trial.getSubjects.head
      generateEntry(id, failure, {
        ajaxSelectObj(trial.getSubjects.map(subject => (subject, subject.identifier)).toSeq, Full(trialSubject), (subject: TrialSubject) => {
          trialSubject = subject
          Replace("stageAndDataField", stageAndDataField())
        })
      }
      )
    }

    def stageAndDataField (failure: Boolean = false): Elem = {
      <div id="stageAndDataField">
        {stageField()}
        <li>
        <fieldset>
          <legend>Subject properties</legend>
          <ul>
            {dataField}
          </ul>
        </fieldset>
        </li>
      </div>
    }

    def stageField(failure: Boolean = false): Elem = {
      val id = "stageField"
      val stages = trial.stages.map(stage => (stage, stage._1)).toSeq
      if(!stages.isEmpty && !stages.head._1._2.isEmpty)   {
        stageName = stages.head._1._1
        stageCriterions = stages.head._1._2
        Replace("dataField", dataField)
      } else{
        stageName = ""
        stageCriterions = List()
        Replace("dataField", dataField)
      }
      <li id={id}>
        <label for={id}>{S.?(id)}</label>
        {
        ajaxSelectObj(stages, Empty, (stage: (String, List[Criterion[_ <: Any, Constraint[_ <: Any]]])) => {
          stageName = stage._1
          stageCriterions = stage._2
          Replace("dataField", dataField)
        })
        } </li>
    }


    def dataField: Elem = {
      subjectDataList.clear()
      if(stageCriterions.isEmpty){
        <div id="dataField">
          No properties available
          </div>
      } else {

     <div id="dataField">
       {
        stageCriterions.foreach(criterion => subjectDataList +=  {
           if(criterion.getClass == classOf[FreeTextCriterion])
             new SubjectDataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], "")
           else
             new SubjectDataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], null)
         })

       val subjectDataNodeSeq = new ListBuffer[Node]()

       for (subjectData <- subjectDataList) {
         subjectDataNodeSeq +=  <li> {
           // generateEntryWithInfo(subjectData.criterion.name, false, subjectData.criterion.description,
           <label for={subjectData.criterion.name + subjectData.criterion.id}>
             <span>
               {subjectData.criterion.name}
             </span>
             <span class="tooltip">
               <img src="/images/icons/help16.png" alt={subjectData.criterion.description} title={subjectData.criterion.description}/> <span class="info">
               {subjectData.criterion.description}
             </span>
             </span>
           </label>
             <div id={subjectData.criterion.name + subjectData.criterion.id}>
               {if (subjectData.criterion.getClass == classOf[DateCriterion]) {
               {
                 ajaxText("", (y: String) => {
                   if (!y.isEmpty) {
                     try {
                       val value = new LocalDate(Utility.slashDate.parse(y).getTime)
                       subjectData.value = value
                       if (!subjectData.criterion.isValueCorrect(value))
                         S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
                     } catch {
                       case _: Throwable  => S.error("randomizeMsg", subjectData.criterion.name + ": unknown failure")
                     }
                   } else {
                     S.error("randomizeMsg", subjectData.criterion.name + ": value not set")
                   }
                 })
               }
             } else if (subjectData.criterion.getClass == classOf[DoubleCriterion]) {
               {
                 ajaxText(if (subjectData.value == null) "" else subjectData.value.toString, (y: String) => {
                   if (!y.isEmpty) {
                     try {
                       val value = y.toDouble
                       subjectData.value = value
                       if (!subjectData.criterion.isValueCorrect(value))
                         S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
                     } catch {
                       case nfe: NumberFormatException => S.error("randomizeMsg", subjectData.criterion.name + ": not a number")
                       case _ : Throwable => S.error("randomizeMsg", "unknown failure")
                     }
                   } else {
                     S.error("randomizeMsg", subjectData.criterion.name + ": value not set")
                   }
                 })
               }
             } else if (subjectData.criterion.getClass == classOf[IntegerCriterion]) {
               {
                 ajaxText(if (subjectData.value == null) "" else subjectData.value.toString, (y: String) => {
                   if (!y.isEmpty) {
                     try {
                       val value = y.toInt
                       subjectData.value = value
                       if (!subjectData.criterion.isValueCorrect(value))
                         S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
                     } catch {
                       case nfe: NumberFormatException => S.error("randomizeMsg", subjectData.criterion.name + ": not a number")
                       case _ : Throwable => S.error("randomizeMsg", "unknown failure")
                     }
                   } else {
                     S.error("randomizeMsg", subjectData.criterion.name + ": value not set")
                   }
                 })
               }
             } else if (subjectData.criterion.getClass == classOf[FreeTextCriterion]) {
               {
                 ajaxTextarea(if (subjectData.value == null) "" else subjectData.value.toString, (y: String) => {
                   subjectData.value = y
                   if (subjectData.value == null)
                     S.error("randomizeMsg", subjectData.criterion.name + ": Element not set")
                   else if (!subjectData.criterion.isValueCorrect(y))
                     S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
                 })
               }
             } else if (subjectData.criterion.getClass == classOf[OrdinalCriterion]) {
               {
                 <div class="radio">{
                   ajaxRadio(subjectData.criterion.asInstanceOf[OrdinalCriterion].values.toSeq, Empty, (y: String) => {
                     subjectData.value = y
                     if (!subjectData.criterion.isValueCorrect(y))
                       S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
                   }).toForm
                   }</div>
               }
             } else {
               <span>?</span>
             }}
             </div>
           //)
           }
         </li>
       }

         NodeSeq fromSeq subjectDataNodeSeq
       }
     </div>
      }
    }

    bind("form", in,
      "identifier" -> subjectField(),
      "stageAndData" -> stageAndDataField(),
      "cancel" ->       submit(S.?("cancel"), () => {
        val user = CurrentLoggedInUser.get.get
        val rightList = user.rights.filter(right => right.trial.id == trial.id)
        val roles = rightList.map(right => right.role)
        if (roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor)) {
          redirectTo("/trial/randomizationData")
        } else {
          redirectTo("/trial/randomizationDataInvestigator")
        }
        }, "class" -> "btnCancel"),
      "submit" -> submit("Save", saveStage _, "class" -> "btnSend"))
  }


}
