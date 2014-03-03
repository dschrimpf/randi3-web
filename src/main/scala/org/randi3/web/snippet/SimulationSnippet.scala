package org.randi3.web.snippet

import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import scala.xml._
import scala.xml.NodeSeq
import scala.collection.mutable.HashMap

import org.randi3.web.lib.DependencyFactory

import net.liftweb.common._
import net.liftweb.http.S._

import net.liftweb.http._

import js.JsCmds.{ Replace, SetHtml }

import net.liftweb.util.Helpers._
import net.liftweb._

import common.Full
import http.SHtml._
import org.randi3.randomization._
import org.randi3.randomization.configuration._
import org.apache.commons.math3.random.MersenneTwister

import org.randi3.model._
import org.randi3.simulation.model._
import org.randi3.simulation.service._
import org.randi3.simulation.distributions._
import scalaz.{ Node => _, _ }
import Scalaz._
import org.randi3.web.util.{ Utility, CurrentTrial }
import org.joda.time.LocalDate
import collection.mutable.{ HashMap, ListBuffer, HashSet }
import org.joda.time.format.DateTimeFormat

import scala.Some
import xml.Text
import scala.Left

import scala.Right
import collection.mutable

import org.randi3.web.model._
import org.randi3.web.util.CurrentSimulationResult

class SimulationSnippet extends StatefulSnippet with GeneralFormSnippet {

  private val trialSiteService = DependencyFactory.get.trialSiteService
  private val userService = DependencyFactory.get.userService
  private val trialService = DependencyFactory.get.trialService
  private val randomizationPluginManager = DependencyFactory.get.randomizationPluginManager

  def dispatch = {
    case "simulate" => simulate _
    case "results" => results _
  }

  //TODO enumeration & text
  private val criterionTypes = Seq( /*("DateCriterion", "DateCriterion"),
    ("IntegerCriterion", "IntegerCriterion"),
    ("DoubleCriterion", "DoubleCriterion"),
    ("FreeTextCriterion", "FreeTextCriterion"),
    */
    ("OrdinalCriterion", "OrdinalCriterion"))

  private val distributionTypes = Seq(("EqualDistributed", "EqualDistributed"), ("FixedRatio", "FixedRatio"))

  private var name = ""
  private var stageCount = 1
  private var description = ""
  private var startDate = new LocalDate
  private var endDate = new LocalDate

  private var trialSiteCount = 1

  private var seed = System.currentTimeMillis()

  private var nameNewTreatmentArm = ""
  private var descriptionNewTreatmentArm = ""
  private var plannedSubjectSizeNewTreatmentArm = 0

  private val armsTmp = getEmptyArmsTmpList

  private def getEmptyArmsTmpList: ListBuffer[TreatmentArmTmp] = {
    val result = new ListBuffer[TreatmentArmTmp]()
    result += new TreatmentArmTmp(Int.MinValue, 0, "", "", 0)
    result
  }

  private var criterionTypeTmp = "OrdinalCriterion"

  private val criterionsTmp = new ListBuffer[CriterionDistributionTmp]()

  private var stageName = ""

  private val stages = new HashMap[String, ListBuffer[CriterionTmp]]()

  private val randomizationMethods = randomizationPluginManager.getPluginNamesWithI18N

  private var randomizationMethodTmp = generateEmptyRandomizationMethodConfig(randomizationMethods.head._1)

  private var isStratifiedByTrialSite = false

  private var simulationRuns = 100

  private var trialSites: HashMap[TrialSite, Int] = new HashMap()

  private def results(xhtml: NodeSeq): NodeSeq = {
    val simulationResult = CurrentSimulationResult.get.getOrElse(S.redirectTo("/simulation/start"))
    val resultMethod = simulationResult.results.head
    
    val details = {
      simulationResult.results.map(method => {
      <div>
     <h3>{method.method}</h3>
      <table border="1">
  <tr>
    <th>Arm</th>
    <th>Min</th>
    <th>Max</th>
    <th>Mean</th>
    <th>Median</th>
  </tr>
    {
     NodeSeq fromSeq  method.simResultsArms.sortBy(_.armName).map( arm => {
          <tr>
    <td>{arm.armName}</td>
    <td>{arm.min}</td>
    <td>{arm.max}</td>
    <td>{arm.mean}</td>
    <td>{arm.median}</td>
  </tr>
      })
    }
</table>
 </div>
    })
      }
    
    val algortihms = {
        <table border="1">
  <tr>
    <th>Algorithm</th>
    <th>Duration</th>
    <th>Min marginal balance</th>
    <th>Mean marginal balance</th>
    <th>Max marginal balance</th>
  </tr>
    {
     NodeSeq fromSeq  simulationResult.results.map( method => {
          <tr>
     <td>{method.method}</td>
    <td>{method.duration / 1000} s</td>
    <td>{method.marginalBalanceMin}</td>
    <td>{method.marginalBalanceMean}</td>
    <td>{method.marginalBalanceMax}</td>
  </tr>
      })
    }
</table>
    }
    
      bind("simulation", xhtml,
      "trialName" -> <span>{simulationResult.scenario.trial.name}</span>,
      "runs" ->  <span>{simulationResult.runs}</span>,
      "duration" -> <span>{resultMethod.duration / 1000} s</span>,
      "algorithms" ->  algortihms,
      "details" -> details
      )
  }

  private def simulate(xhtml: NodeSeq): NodeSeq = {

    def simulate() {
      val trialSitesDistribution = TrialSiteDistribution(new MersenneTwister(seed).nextInt(), trialSites.toMap)
      val criterions = createCriterionsList(criterionsTmp)
      Trial(
        name = name,
        abbreviation = name,
        description = description,
        startDate = startDate,
        endDate = endDate,
        status = TrialStatus.ACTIVE,
        treatmentArms = createTreatmentArms(armsTmp),
        criterions = criterions.map(crit => crit.criterion),
        participatingSites = trialSites.keys.toList,
        randomizationMethod = None,
        stages = Map(),
        identificationCreationType = TrialSubjectIdentificationCreationType.CONTINUOUS_COUNTER,
        isTrialOpen = true,
        isStratifiedByTrialSite = isStratifiedByTrialSite).toEither match {
          case Left(x) => S.error("trialMsg", x.toString)
          case Right(trial) => {
            //TODO Random Config
            randomizationPluginManager.getPlugin(randomizationMethodTmp.name).get.randomizationMethod(new MersenneTwister(seed), trial, randomizationMethodTmp.getConfigurationProperties).toEither match {
              case Left(failure) => S.error("trialMsg", failure)
              case Right(randomMethod) => {
                val trialWithMethod = trial.copy(randomizationMethod = Some(randomMethod))
                val simulationScenario = new SimulationScenario {
                  def trial: Trial = trialWithMethod

                  def criterionProbability: List[CriterionDistribution[Any]] = criterions

                  def stageProbabilities = List()

                  def siteRatio: TrialSiteDistribution = trialSitesDistribution

                  def randomizationMethods: List[(String, RandomizationMethod)] = List((randomizationMethodTmp.name, randomMethod))
                }

                CurrentSimulationResult(Some(SimulationUtil.simulate(simulationScenario, simulationRuns, seed)))
                S.redirectTo("/simulation/result")
              }
            }
          }
        }

    }

    generateForm(xhtml, simulate())

  }

  private def generateForm(xhtml: NodeSeq, code: => Unit): NodeSeq = {
    def nameField(failure: Boolean = false): Elem = {
      val id = "name"
      generateEntry(id, failure, {
        ajaxText(name, v => {
          name = v
          Trial.check(name = v).toEither match {
            case Left(x) =>
              showErrorMessage(id, x); Replace(id + "Li", nameField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", nameField(false))
          }
        }, "id" -> id)
      })
    }

    def descriptionField(failure: Boolean = false): Elem = {
      val id = "description"
      generateEntry(id, failure, {
        ajaxTextarea(description, v => {
          description = v
          Trial.check(description = v).toEither match {
            case Left(x) =>
              showErrorMessage(id, x); Replace(id + "Li", descriptionField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", descriptionField(false))
          }
        }, "id" -> id)
      })
    }

    def startDateField(failure: Boolean = false): Elem = {
      val id = "startDate"
      generateEntry(id, failure, {

        text(Utility.slashDate.format(startDate.toDate).toString, v => {
          startDate = new LocalDate(Utility.slashDate.parse(v).getTime)
          Trial.check(startDate = startDate).toEither match {
            case Left(x) => showErrorMessage(id, x)
            case Right(_) => clearErrorMessage(id)
          }
        }, "id" -> id)
      })
    }

    def endDateField(failure: Boolean = false): Elem = {
      val id = "endDate"
      generateEntry(id, failure, {
        text(Utility.slashDate.format(endDate.toDate).toString, v => {
          endDate = new LocalDate(Utility.slashDate.parse(v).getTime)
          Trial.check(endDate = endDate).toEither match {
            case Left(x) => showErrorMessage(id, x)
            case Right(_) => clearErrorMessage(id)
          }
        }, "id" -> id)
      })
    }

    def trialSitesCountField(failure: Boolean = false): Elem = {
      val id = "trialSiteCount"
      generateEntry(id, failure, {
        ajaxText(trialSiteCount.toString, v => {
          if (v.parseInt.isSuccess) {
            trialSiteCount = v.parseInt.toOption.get
            clearErrorMessage(id); Replace(id + "Li", trialSitesCountField(false))
          } else {
            showErrorMessage(id, NonEmptyList("Not a number")); Replace(id + "Li", trialSitesCountField(true))
          }
        }, "id" -> id)
      })
    }

    def seedField(failure: Boolean = false): Elem = {
      val id = "seed"
      generateEntry(id, failure, {
        ajaxText(seed.toString, v => {
          if (v.parseInt.isSuccess) {
            seed = v.parseInt.toOption.get
            clearErrorMessage(id); Replace(id + "Li", seedField(false))
          } else {
            showErrorMessage(id, NonEmptyList("Not a number")); Replace(id + "Li", seedField(true))
          }
        }, "id" -> id)
      })
    }

    def randomizationMethodSelectField: NodeSeq = {
      ajaxSelect(randomizationMethods.toSeq, Empty, v => {
        randomizationMethodTmp = generateEmptyRandomizationMethodConfig(v)
        Replace("randomizationConfig", generateRandomizationConfigField)
      })
    }

    def simulationRunsField(failure: Boolean = false): Elem = {
      val id = "simulationRuns"
      generateEntry(id, failure, {
        ajaxText(simulationRuns.toString, v => {
          if (v.parseInt.isSuccess) {
            simulationRuns = v.parseInt.toOption.get
            clearErrorMessage(id); Replace(id + "Li", simulationRunsField(false))
          } else {
            showErrorMessage(id, NonEmptyList("Not a number")); Replace(id + "Li", simulationRunsField(true))
          }
        }, "id" -> id)
      })
    }

    bind("trial", xhtml,
      "info" -> <span>{ name }</span>,
      "name" -> nameField(),
      "description" -> descriptionField(),
      "startDate" -> startDateField(),
      "endDate" -> endDateField(),
      "trialSiteCount" -> trialSitesCountField(),
      "treatmentArmName" -> ajaxText(nameNewTreatmentArm, nameNewTreatmentArm = _, "id" -> "nameNewTreatmentArm"),
      "treatmentArmDescription" -> ajaxText(descriptionNewTreatmentArm, descriptionNewTreatmentArm = _),
      "treatmentArmPlannedSubjects" -> ajaxText(plannedSubjectSizeNewTreatmentArm.toString, size => plannedSubjectSizeNewTreatmentArm = size.toInt),
      "addTreatmentArm" -> ajaxButton(S.?("add"), () => {
        armsTmp += new TreatmentArmTmp(Int.MinValue, 0, "", "", 0)
        Replace("treatmentArms", generateTreatmentArms(xhtml))
      }, "class" -> "btnNormal"),
      "treatmentArms" -> generateTreatmentArms(xhtml),
      //TODO selectElem
      "criterionSelect" -> ajaxSelect(criterionTypes, Empty, criterionTypeTmp = _),
      "addSelectedCriterion" -> ajaxButton(S.?("add"), () => {
        addSelectedCriterion(criterionTypeTmp, criterionsTmp)
        Replace("criterions", generateCriterions(xhtml))
      }),
      "criterions" -> generateCriterions(xhtml),
      //      "stageName" -> ajaxText(stageName, stageName = _),
      //      "addStage" -> ajaxButton("add", () => {
      //        stages.put(stageName, new ListBuffer())
      //        Replace("stagesTabs", generateStages(xhtml))
      //      }),
      //      "stages" -> generateStages(xhtml),
      "randomizationMethodSelect" -> randomizationMethodSelectField,
      "randomizationConfig" -> generateRandomizationConfigField,
      "simulationRuns" -> simulationRunsField(),
      "seed" -> seedField(),
      "trialSiteDistributionConfig" -> trialSiteDistributionConfigField,
        "propertyDistributionConfig" -> generatePropertyDistributionConfigField,
      "cancel" -> submit("cancel", () => {
        cleanVariables()
        redirectTo("/trial/list")
      }, "class" -> "btnCancel"),
      "submit" -> submit(S.?("simulate"), code _, "class" -> "btnSend"))
  }

  private def createTreatmentArms(arms: ListBuffer[TreatmentArmTmp]): List[TreatmentArm] = {
    val result = ListBuffer[TreatmentArm]()

    arms.foreach(arm =>
      TreatmentArm(id = arm.id, version = arm.version, name = arm.name, description = arm.description, plannedSize = arm.plannedSize).toEither match {
        case Left(x) => S.error(x.toString()) //TODO error handling
        case Right(treatmentArm) => result += treatmentArm
      })

    result.toList
  }

  private def generateEmptyRandomizationMethodConfig(randomizationMethodName: String): RandomizationMethodConfigTmp = {
    val plugin = randomizationPluginManager.getPlugin(randomizationMethodName).get
    val stages = plugin.randomizationConfigurationOptions()._2
    val configurations = plugin.randomizationConfigurationOptions()._1
    val methodConfigsTmp = configurations.map(config => {
      if (config.getClass == classOf[BooleanConfigurationType]) {
        new RandomizationMethodConfigEntryTmp(config.asInstanceOf[BooleanConfigurationType], true)
      } else if (config.getClass == classOf[DoubleConfigurationType]) {
        new RandomizationMethodConfigEntryTmp(config.asInstanceOf[DoubleConfigurationType], 0.0)
      } else if (config.getClass == classOf[IntegerConfigurationType]) {
        new RandomizationMethodConfigEntryTmp(config.asInstanceOf[IntegerConfigurationType], 0)
      } else if (config.getClass == classOf[OrdinalConfigurationType]) {
        new RandomizationMethodConfigEntryTmp(config.asInstanceOf[OrdinalConfigurationType], config.asInstanceOf[OrdinalConfigurationType].options.head)
      }

    })
    new RandomizationMethodConfigTmp(name = plugin.name, i18nName = plugin.i18nName, description = plugin.description, canBeUsedWithStratification = plugin.canBeUsedWithStratification, configurationEntries = methodConfigsTmp.asInstanceOf[List[RandomizationMethodConfigEntryTmp[Any]]], stages = stages)
  }

  private def generateRandomizationConfigField: Elem = {
    <div id="randomizationConfig" class="clearfix">
      <fieldset>
        <legend>{ S.?("generalInformation") }</legend>
        <ul>
          <li>
            <label for="randomizationMethodName">
              { S.?("name") }
              :
            </label>
            <span id="randomizationMethodName">
              { randomizationMethodTmp.i18nName }
            </span>
          </li>
          <li>
            <label for="randomizationMethodDescription">
              { S.?("description") }
              :
            </label>
            <span id="randomizationMethodDescription">
              { randomizationMethodTmp.description }
            </span>
          </li>
          {
            if (!randomizationMethodTmp.stages.isEmpty) {
              <li>
                <label for="randomizationMethodStages">{ S.?("stages") }:</label>
                <span id="randomizationMethodStages">
                  <ul class="stagesList">
                    {
                      randomizationMethodTmp.stages.flatMap(stage => {
                        <li>
                          <span>{ stage._1 }</span>
                          <ul>
                            {
                              stage._2.flatMap(crit => {
                                <li>
                                  { crit.name + ": " + crit.description }
                                </li>
                              })
                            }
                          </ul>
                        </li>
                      })
                    }
                  </ul>
                </span>
              </li>
            }
          }
        </ul>
      </fieldset>{
        if (!randomizationMethodTmp.configurationEntries.isEmpty) {
          <fieldset>
            <legend>{ S.?("trial.randomizationConfiguration") }</legend>
            <ul>
              {
                randomizationMethodTmp.configurationEntries.flatMap(configuration => {
                  <li>
                    <label for={ configuration.configurationType.name }>
                      { configuration.configurationType.name }
                      :
                      <span class="tooltip">
                        <img src="/images/icons/help16.png" alt={ configuration.configurationType.description } title={ configuration.configurationType.description }/><span class="info">
                                                                                                                                                                         { configuration.configurationType.description }
                                                                                                                                                                       </span>
                      </span>
                    </label>{
                      ajaxText(configuration.value.toString, v => {
                        //TODO check value
                        configuration.value = v
                      }, "id" -> configuration.configurationType.name)
                    }
                  </li>
                })
              }
            </ul>
          </fieldset>
        } else <div></div>
      }{
        if (randomizationMethodTmp.canBeUsedWithStratification) {
          val criterionList = criterionsTmp
          <fieldset>
            <legend>{ S.?("trial.stratification") }</legend>
            <ul>
              <li>
                <label for="trialSiteStratification">{ S.?("trial.trialSiteStratification") }:</label>
                { checkbox(isStratifiedByTrialSite, value => isStratifiedByTrialSite = value, "id" -> "trialSiteStratification") }
              </li>
              {
                val result = new ListBuffer[Node]()
                for (i <- criterionList.indices) {
                  val criterion = criterionList(i)
                  result += generateStratumConfig("stratum-" + criterion.name.replace(' ', '_'), criterion)
                }
                NodeSeq fromSeq result
              }
            </ul>
          </fieldset>

        } else <div></div>
      }
    </div>
  }

  private def trialSiteDistributionConfigField: Elem = {
    <div id="trialSiteDistributionConfig" class="clearfix">
      {
        val criterionList = criterionsTmp
        <fieldset>
          <legend>{ S.?("simulation.trialSiteDistributions") }{
            ajaxButton(S.?("refresh"), () => {
              Replace("trialSiteDistributionConfig", trialSiteDistributionConfigField)
            })
          }</legend>
          <ul>
            {
              trialSites.clear()
              (1 to trialSiteCount).foreach(i => {
                trialSites.put(TrialSite(i, 0, "Site " + i, "Country", "Street", "Post code", "city", "password", true).toOption.get, 1)
              })

              NodeSeq fromSeq trialSites.toList.sortBy(_._1.name).map(entry => {
                <li>
                  <label for={ entry._1.name + "label" }> { entry._1.name }:</label>{
                    text(entry._2.toString, v => {
                      if (v.parseInt.isSuccess) {
                        trialSites.remove(entry._1)
                        trialSites.put(entry._1, v.parseInt.toOption.get)
                      }
                    }, "id" -> (entry._1 + "label"))
                  }
                </li>
              })
            }
          </ul>
        </fieldset>
      }
    </div>
  }

  private def generatePropertyDistributionConfigField: Elem = {
    <div id="propertyDistributionConfig" class="clearfix">
      {
        val criterionList = criterionsTmp
        <fieldset>
          <legend>{ S.?("simulation.propertyDistributions") }{
            ajaxButton(S.?("refresh"), () => {
              Replace("propertyDistributionConfig", generatePropertyDistributionConfigField)
            })
          }</legend>
          <ul>
            {
              NodeSeq fromSeq criterionsTmp.filter(_.typ == "OrdinalCriterion").map(criterion => {
                createDistributionConfig(criterion)
              })
            }
          </ul>
        </fieldset>
      }
    </div>
  }

  private def createDistributionConfig(criterion: CriterionDistributionTmp): Elem = {
    val id = "distributionConfig" + criterion.name
    val idValue = id + "Value"
    <div id={ id }>
      <li>
        <label for={ id + "distribution" }>{ criterion.name }</label>{
          ajaxSelect(distributionTypes, Empty, (value => {
            criterion.distibutionName = value
            Replace(idValue, createDistributionValues(idValue, value, criterion))
          }), "id" -> (id + "distribution"))
        }
      </li>
      { createDistributionValues(idValue, distributionTypes.head._1, criterion) }
    </div>
  }

  private def createDistributionValues(id: String, value: String, criterion: CriterionDistributionTmp): Elem = {
    <div id={ id }>
      {
        value match {
          case "EqualDistributed" => {
            criterion.ratio.clear
          }
          case "FixedRatio" => {
            criterion.ratio.clear
            criterion.values.get.foreach(ordinalValue => {
              criterion.ratio.put(ordinalValue, 1)
            })
            NodeSeq fromSeq criterion.ratio.toList.sortBy(_._1).map(entry => {
              <li>
                <label for={ entry._1 + "label" }> { entry._1 }:</label>{
                  text(entry._2.toString, v => {
                    if (v.parseInt.isSuccess) {
                      criterion.ratio.remove(entry._1)
                      criterion.ratio.put(entry._1, v.parseInt.toOption.get)
                    }
                  }, "id" -> (entry._1 + "label"))
                }
              </li>
            })
          }
        }
      }
    </div>
  }

  private def generateTreatmentArms(xhtml: NodeSeq): NodeSeq = {
    <div id="treatmentArms">
      {
        val result = new ListBuffer[Node]()
        for (i <- armsTmp.indices) {
          val arm = armsTmp(i)
          result += <div class="singleField">
                      <fieldset>
                        <legend>
                          { S.?("treatmentArm") }
                          {
                            ajaxButton(<img alt="remove" src="/images/icons/error16.png"/>, () => {
                              armsTmp.remove(i)
                              Replace("treatmentArms", generateTreatmentArms(xhtml))
                            })
                          }
                        </legend>
                        <ul>
                          <li>
                            <label for={ "armName" + i }>{ S.?("name") }</label>{ ajaxText(arm.name, arm.name = _, "id" -> ("armName" + i)) }
                          </li>
                          <li>
                            <label for={ "armDescription" + i }>{ S.?("description") }</label>{ ajaxTextarea(arm.description, arm.description = _, "id" -> ("armDescription" + i)) }
                          </li>
                          <li>
                            <label for={ "armPlannedSize" + i }>{ S.?("trial.plannedSubjectSize") }</label>{ ajaxText(arm.plannedSize.toString, (v) => arm.plannedSize = v.toInt, "id" -> ("armPlannedSize" + i)) /*TODO check toInt */ }
                          </li>
                        </ul>
                      </fieldset>
                    </div>
        }
        NodeSeq fromSeq result
      }
    </div>
  }

  private def generateGeneralCriterions(xhtml: NodeSeq, id: String, criterionList: ListBuffer[CriterionDistributionTmp]): NodeSeq = {
    <div id={ id }>
      {
        val result = new ListBuffer[Node]()
        for (i <- criterionList.indices) {
          val criterion = criterionList(i)
          result += <div class="singleField">
                      <fieldset>
                        <legend>
                          { criterion.typ }{
                            ajaxButton(<img alt="remove" src="/images/icons/error16.png"/>, () => {
                              criterionList.remove(i)
                              Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
                            })
                          }
                        </legend>
                        <ul>
                          <li>
                            <label for={ id + "Name" + i }>{ S.?("name") }</label>{ ajaxText(criterion.name, criterion.name = _, "id" -> (id + "Name" + i)) }
                          </li>
                          <li>
                            <label for={ id + "Description" + +i }>{ S.?("description") }</label>{ ajaxTextarea(criterion.description, criterion.description = _, "id" -> (id + "Description" + +i)) }
                          </li>{ createValues(criterion, xhtml, id, criterionList) }
                        </ul>
                      </fieldset>
                    </div>
        }
        NodeSeq fromSeq result
      }
    </div>
  }

  private def generateCriterions(xhtml: NodeSeq): NodeSeq = {
    generateGeneralCriterions(xhtml, "criterions", criterionsTmp)
  }

  private def createValues(criterion: CriterionDistributionTmp, xhtml: NodeSeq, id: String, criterionList: ListBuffer[CriterionDistributionTmp]): NodeSeq = {
    criterion.values match {
      case None => <span></span>
      case Some(x) => {
        //TODO implement specific replacement
        <li>
          <fieldset class="criterionValues">
            <legend>
              { S.?("values") }
              {
                ajaxButton(S.?("add"), () => {
                  x += ""
                  criterion.inclusionConstraint = None
                  Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
                })
              }
            </legend>
            <ul>
              {
                val result = new ListBuffer[Node]()
                for (i <- x.indices) result += <li>
                                                 {
                                                   ajaxText(x(i), v => {
                                                     x(i) = v
                                                     criterion.inclusionConstraint = None
                                                     Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
                                                   })
                                                 }{
                                                   ajaxButton(S.?("remove"), () => {
                                                     x.remove(i)
                                                     criterion.inclusionConstraint = None
                                                     Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
                                                   })
                                                 }
                                               </li>
                NodeSeq fromSeq result
              }
            </ul>
          </fieldset>
        </li>
      }
    }
  }

  private def generateStratumConfig(id: String, criterion: CriterionDistributionTmp): Elem = {
    if (criterion.typ != "FreeTextCriterion") {
      <li>
        <fieldset id={ id } class="criterionForStrata">
          <legend>
            { criterion.typ }
          </legend>
          <ul>
            <li>
              <label>{ S.?("name") }</label>{ criterion.name }
            </li>
            <li>
              <label>{ S.?("description") }</label>{ criterion.description }
            </li>
          </ul>
          <div>
            {
              ajaxButton(S.?("trial.addStratum"), () => {
                val constraint = new ConstraintTmp()
                if (criterion.typ == "OrdinalCriterion") {
                  constraint.ordinalValues.clear()
                  criterion.values.get.foreach(value => {
                    constraint.ordinalValues.add((false, value))
                  })
                }
                criterion.strata.append(constraint)
                Replace(id, generateStratumConfig(id, criterion))
              })
            }
            {
              ajaxButton(S.?("remove"), () => {
                criterion.strata.remove(criterion.strata.size - 1)
                Replace(id, generateStratumConfig(id, criterion))
              })
            }
          </div>{
            val result = new ListBuffer[Node]()
            for (i <- criterion.strata.indices) {
              val constraint = criterion.strata(i)
              result +=
                { generateStratumElement(id + i, criterion, constraint) }

            }
            NodeSeq fromSeq result
          }
        </fieldset>
      </li>
    } else <div></div>
  }

  private def generateStratumElement(id: String, criterion: CriterionDistributionTmp, constraint: ConstraintTmp): Elem = {
    <fieldset id={ id } class="stratum">
      <legend>{ S.?("group") }</legend>{
        if (criterion.typ != "OrdinalCriterion") {
          <ul>
            <li>
              <label for={ "groupLowerBoundary" + id }>
                { S.?("lowerBoundary") }
                {
                  ajaxCheckbox(constraint.minValue.isDefined, v => {
                    if (!v) {
                      constraint.minValue = None
                    } else {
                      constraint.minValue = Some("")
                    }
                    Replace(id, generateStratumElement(id, criterion, constraint))
                  }, "id" -> ("groupLowerBoundary" + id))
                }
              </label>
              {
                if (constraint.minValue.isDefined) {
                  ajaxText(constraint.minValue.get, v => {
                    constraint.minValue = Some(v)
                  })
                }
              }
            </li>
            <li>
              <label for={ "upperBoundary" + id }>
                { S.?("upperBoundary") }
                {
                  ajaxCheckbox(constraint.maxValue.isDefined, v => {
                    if (!v) {
                      constraint.maxValue = None
                    } else {
                      constraint.maxValue = Some("")
                    }
                    Replace(id, generateStratumElement(id, criterion, constraint))
                  }, "id" -> ("upperBoundary" + id))
                }
              </label>
              {
                if (constraint.maxValue.isDefined) {
                  ajaxText(constraint.maxValue.get, v => {
                    constraint.maxValue = Some(v)
                  })
                }
              }
            </li>
          </ul>
        } else {
          val ordinalValues = constraint.ordinalValues
          <ul>{
            ordinalValues.toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) < 0).flatMap(value => {
              <li>
                <label>{ value._2 }</label>
                {
                  ajaxCheckbox(value._1, v => {
                    ordinalValues.remove(value)
                    ordinalValues.add((v, value._2))
                    Replace(id, generateStratumElement(id, criterion, constraint))
                  })
                }
              </li>
            })
          }</ul>
        }
      }
    </fieldset>
  }

  private def addSelectedCriterion(criterionType: String, criterionList: ListBuffer[CriterionDistributionTmp]) {
    def emptyValues = {
      val list = new ListBuffer[String]()
      list += ""
      list += ""
      Some(list)
    }
    criterionType match {
      case "OrdinalCriterion" => criterionList += new CriterionDistributionTmp(Int.MinValue, 0, "OrdinalCriterion", "", "", emptyValues, None, new ListBuffer(), new HashMap())
      //  case x => criterionList += new CriterionTmp(Int.MinValue, 0, x, "", "", None, None)
    }
  }

  private def createCriterionsList(criterions: ListBuffer[CriterionDistributionTmp]): List[CriterionDistribution[Any]] = {
    val result = ListBuffer[CriterionDistribution[Any]]()
    val random = new MersenneTwister(seed)
    var i = 0
    criterions.sortBy(_.name).foreach(criterionTmp => {
       i = i +1
      (criterionTmp.typ match {
      //      case "DateCriterion" => DateCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      //      case "IntegerCriterion" => IntegerCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      //      case "DoubleCriterion" => DoubleCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      //      case "FreeTextCriterion" => FreeTextCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      case "OrdinalCriterion" => OrdinalCriterion(id = i, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, values = criterionTmp.values.get.toSet, inclusionConstraint = None, strata = createStrata(criterionTmp))
    }).toEither match {
      case Left(x) => S.error(x.toString()) //TODO error handling
      case Right(criterion) => {
        criterionTmp.distibutionName match {
          case "FixedRatio" => result += (new OrdinalCriterionFixedRatio(criterion, random.nextLong(), criterionTmp.ratio.toMap)).asInstanceOf[CriterionDistribution[Any]]
          case _ => result += (new OrdinalCriterionEqualDistributed(criterion, random.nextLong())).asInstanceOf[CriterionDistribution[Any]]
        }
      }
    }})

    result.toList
  }

  private def createStrata[T <: Constraint[Any]](criterionTmp: CriterionDistributionTmp): List[T] = {
    var id = 0
    val list: List[T] = criterionTmp.strata.toList.
      map(constraintTmp =>{
       id = id +1
      createConstraint(criterionTmp, constraintTmp, id).asInstanceOf[Option[T]] }).
      filter(elem => elem.isDefined).map(elem => elem.get)
    list
  }

  private def createConstraint[T](criterionTmp: CriterionDistributionTmp, constraint: ConstraintTmp, id: Int): Option[T] = {
    criterionTmp.typ match {
      case "DateCriterion" => {
        val min = if (constraint.minValue.isDefined) Some(new LocalDate(Utility.slashDate.parse(constraint.minValue.get).getTime)) else None
        val max = if (constraint.maxValue.isDefined) Some(new LocalDate(Utility.slashDate.parse(constraint.maxValue.get).getTime)) else None
        Some(DateConstraint(id, constraint.version, List(min, max)).toOption.get.asInstanceOf[T])
      }
      case "IntegerCriterion" => {
        val min = if (constraint.minValue.isDefined) Some(constraint.minValue.get.toInt) else None
        val max = if (constraint.maxValue.isDefined) Some(constraint.maxValue.get.toInt) else None
        Some(IntegerConstraint(id, constraint.version, List(min, max)).toOption.get.asInstanceOf[T])
      }
      case "DoubleCriterion" => {
        val min = if (constraint.minValue.isDefined) Some(constraint.minValue.get.toDouble) else None
        val max = if (constraint.maxValue.isDefined) Some(constraint.maxValue.get.toDouble) else None
        Some(DoubleConstraint(id, constraint.version, List(min, max)).toOption.get.asInstanceOf[T])
      }
      case "FreeTextCriterion" => {
        Some(FreeTextConstraintNotEmpty(id, constraint.version).toOption.get.asInstanceOf[T])
      }
      case "OrdinalCriterion" => {
        Some(OrdinalConstraint(id, constraint.version, constraint.ordinalValues.toList.filter(entry => entry._1).map(entry => Some(entry._2))).toOption.get.asInstanceOf[T])
      }
      case _ => None
    }
  }

  private def cleanVariables() {
    name = ""
    stageCount = 1
    description = ""
    startDate = new LocalDate
    endDate = new LocalDate
    trialSiteCount = 1
    //TODO clean participation sites
    armsTmp.clear()
    armsTmp += new TreatmentArmTmp(Int.MinValue, 0, "", "", 0)
    criterionsTmp.clear()
    cleanTreatmentArmVariables()
    isStratifiedByTrialSite = false
  }

  private def cleanTreatmentArmVariables() {
    nameNewTreatmentArm = ""
    descriptionNewTreatmentArm = ""
    plannedSubjectSizeNewTreatmentArm = 0
  }

}