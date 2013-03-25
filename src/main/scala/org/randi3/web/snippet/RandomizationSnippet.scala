package org.randi3.web.snippet

import net.liftweb.http.{S, StatefulSnippet}
import collection.mutable.ListBuffer
import xml.{Node, NodeSeq}
import org.randi3.web.util.{RandomizationResult, CurrentLoggedInUser, Utility, CurrentTrial}
import net.liftweb.http.S._
import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint.Constraint
import net.liftweb.http.SHtml._
import net.liftweb.common.Empty
import org.randi3.model.{Role, TrialSubject, TrialSubjectIdentificationCreationType, SubjectProperty}
import net.liftweb.util.Helpers._
import scala.Left
import scala.Right
import scala.Some
import org.randi3.web.model.SubjectDataTmp
import org.randi3.web.lib.DependencyFactory
import org.joda.time.LocalDate


class RandomizationSnippet extends StatefulSnippet with GeneralFormSnippet{


  private val trialService = DependencyFactory.get.trialService


  def dispatch = {
    case "randomize" => randomize _
    case "randomizationResult" => showRandomizationResult _
  }


  private var subjectIdentifier = ""
  private var subjectDataList = new ListBuffer[SubjectDataTmp]()

  private def randomize(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      error("Trial not found")
      redirectTo("/trial/list")
    }

    if (subjectDataList.isEmpty) {
      trial.criterions.foreach(criterion => subjectDataList +=  {
        if(criterion.getClass == classOf[FreeTextCriterion])
          new SubjectDataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], "")
        else
          new SubjectDataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], null)
      })
    }

    val subjectDataNodeSeq = new ListBuffer[Node]()

    for (subjectData <- subjectDataList) {
      subjectDataNodeSeq +=  <fieldset> {
        // generateEntryWithInfo(subjectData.criterion.name, false, subjectData.criterion.description,
        <legend>
          <span>
            {subjectData.criterion.name}
          </span>
          <span class="tooltip">
            <img src="/images/icons/help16.png" alt={subjectData.criterion.description} title={subjectData.criterion.description}/> <span class="info">
            {subjectData.criterion.description}
          </span>
          </span>
        </legend>
          <div>
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
                    case _ => S.error("randomizeMsg", subjectData.criterion.name + ": unknown failure")
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
                    case _ => S.error("randomizeMsg", "unknown failure")
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
                    case _ => S.error("randomizeMsg", "unknown failure")
                  }
                } else {
                  S.error("randomizeMsg", subjectData.criterion.name + ": value not set")
                }
              })
            }
          } else if (subjectData.criterion.getClass == classOf[FreeTextCriterion]) {
            {
              ajaxText(if (subjectData.value == null) "" else subjectData.value.toString, (y: String) => {
                subjectData.value = y
                if (subjectData.value == null)
                  S.error("randomizeMsg", subjectData.criterion.name + ": Element not set")
                else if (!subjectData.criterion.isValueCorrect(y))
                  S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
              })
            }
          } else if (subjectData.criterion.getClass == classOf[OrdinalCriterion]) {
            {
              <div>
                {ajaxRadio(subjectData.criterion.asInstanceOf[OrdinalCriterion].values.toSeq, Empty, (y: String) => {
                subjectData.value = y
                if (!subjectData.criterion.isValueCorrect(y))
                  S.error("randomizeMsg", subjectData.criterion.name + ": inclusion constraint not fulfilled")
              }).toForm}
              </div>
            }
          } else {
            <span>?</span>
          }}
          </div>
        //)
        }
      </fieldset>
    }


    def randomizeSubject() {
      val properties: List[SubjectProperty[Any]] = subjectDataList.toList.map(subjectData => SubjectProperty(criterion = subjectData.criterion, value = subjectData.value).either match {
        case Left(x) => null
        case Right(prop) => prop
      })
      if (trial.criterions.isEmpty || subjectDataList.toList.map(subjectData => subjectData.criterion.isValueCorrect(subjectData.value)).reduce((acc, elem) => acc && elem)) {
        if (trial.identificationCreationType != TrialSubjectIdentificationCreationType.EXTERNAL) subjectIdentifier = "system"
        TrialSubject(identifier = subjectIdentifier, investigatorUserName = CurrentLoggedInUser.get.get.username, trialSite = CurrentLoggedInUser.get.get.site, properties = properties).either match {
          case Left(x) => S.error("randomizeMsg", x.toString())
          case Right(subject) => {
            trialService.randomize(trial, subject).either match {
              case Left(x) => S.error("randomizeMsg", x)
              case Right(result) => {
                RandomizationResult.set(Some((result._1, result._2, subject)))

                CurrentTrial.set(Some(trialService.get(trial.id).toOption.get.get))
                subjectDataList.clear()
                subjectIdentifier = ""
                S.notice("Thanks patient (" + result._2 + ") randomized to treatment arm: " + result._1.name + "!")
                S.redirectTo("/trialSubject/randomizationResult")
              }
            }
          }
        }
      } else S.error("randomizeMsg", "Inclusion constraints not fulfilled")
    }

    bind("form", in,
      "identifier" -> {
        if (trial.identificationCreationType == TrialSubjectIdentificationCreationType.EXTERNAL)
          ajaxText(subjectIdentifier, s => subjectIdentifier = s)
        else <span>System generated identifier</span>
      },
      "data" -> {
        NodeSeq fromSeq subjectDataNodeSeq
      },
      "cancel" -> <a href={val user = CurrentLoggedInUser.get.get
      val rightList = user.rights.filter(right => right.trial.id == trial.id)
      val roles = rightList.map(right => right.role)
      if (roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor)) {
        "/trial/randomizationData"
      } else {
        "/trial/randomizationDataInvestigator"
      }}>Cancel</a>,
      "submit" -> button("Randomize", randomizeSubject _))
  }


  private def showRandomizationResult(in: NodeSeq): NodeSeq = {
    if (RandomizationResult.isDefined && RandomizationResult.get.isDefined) {

      val result = RandomizationResult.get.get
      bind("trialSubject", in,
        "treatmentArm" -> <span style="font-weight:bold"> {result._1.name}</span>,
        "identifier" -> <span style="font-weight:bold">{result._2}</span> ,
        "ok" -> button("Ok", () => {
          RandomizationResult.set(None)
          val user = CurrentLoggedInUser.get.get
          val trial = CurrentTrial.get.get
          val rightList = user.rights.filter(right => right.trial.id == trial.id)
          val roles = rightList.map(right => right.role)
          if (roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor)) {
            S.redirectTo("/trial/randomizationData")
          } else {
            S.redirectTo("/trial/randomizationDataInvestigator")
          }
        })
      )
    } else S.redirectTo("/trial/list")
  }

}
