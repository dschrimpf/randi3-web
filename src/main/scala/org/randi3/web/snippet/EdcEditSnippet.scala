package org.randi3.web.snippet


import scala.xml._
import scala.xml.NodeSeq

import org.randi3.web.lib.DependencyFactory

import net.liftweb.http.SHtml._
import net.liftweb.http._

import js.JsCmds.Replace
import net.liftweb.util.Helpers._
import scalaz.NonEmptyList
import org.randi3.model.criterion._
import constraint._
import org.randi3.web.util.{Utility, CurrentLoggedInUser, CurrentEDCTrial}
import collection.mutable.{HashMap, ListBuffer}
import collection.mutable
import net.liftweb.common.Empty
import org.randi3.randomization.RandomizationMethod
import org.randi3.model._
import org.apache.commons.math3.random.MersenneTwister
import org.joda.time.LocalDate
import scalaz.Scalaz._
import scala.Left
import org.randi3.randomization.configuration.OrdinalConfigurationType
import org.randi3.randomization.configuration.IntegerConfigurationType
import org.randi3.randomization.configuration.BooleanConfigurationType
import scala.Some
import xml.Node

import net.liftweb.common.Full
import scala.Right
import org.randi3.randomization.configuration.DoubleConfigurationType

import org.randi3.web.model._



class EdcEditSnippet extends StatefulSnippet {


  def dispatch: EdcEditSnippet#DispatchIt = {
    case "add" => add _
    case "edit" => edit _
  }


  private val armsTmp = new ListBuffer[TreatmentArmTmp]()

 private val randomizationPluginManager = DependencyFactory.get.randomizationPluginManager

 private val randomizationMethods = randomizationPluginManager.getPluginNames

  private val randomizationMethodSelect = {
    randomizationMethods map {
      s => (s, s)
    } toSeq
  }
  private var randomizationMethodTmp = generateEmptyRandomizationMethodConfig(randomizationMethods.head)

  private val criterionsTmp = new ListBuffer[CriterionTmp]()



  private def add(nodeSeq: NodeSeq): NodeSeq = {
    if (CurrentEDCTrial.isEmpty) S.redirectTo("/edcTrial/listRemote")

    val trial = CurrentEDCTrial.get.get

    clearFields

    val selectedCriterions = new mutable.HashSet[Criterion[Any, Constraint[Any]]] ()

    def save() {
      println(selectedCriterions)
      Trial(
        name = trial.identifier,
        abbreviation = trial.identifier,
        description = trial.description,
        startDate = new LocalDate(),
        endDate = new LocalDate(),
        status = TrialStatus.IN_PREPARATION,
        treatmentArms = createTreatmentArms(armsTmp),
        criterions = createCriterionsList(criterionsTmp),
        participatingSites = List(CurrentLoggedInUser.get.get.site),
        randomizationMethod = None,
        stages = Map(),
        identificationCreationType = TrialSubjectIdentificationCreationType.EXTERNAL,
        isTrialOpen = false,
        isStratifiedByTrialSite = false
      ).either match {
        case Left(x) => S.error("trialMsg", x.toString)
        case Right(newTrial) => {
          //TODO Random Config
          val randomMethod = randomizationPluginManager.getPlugin(randomizationMethodTmp.name).get.randomizationMethod(new MersenneTwister(), newTrial, randomizationMethodTmp.getConfigurationProperties).toOption.get
          val trialWithMethod = newTrial.copy(randomizationMethod = Some(randomMethod))

          DependencyFactory.get.openClinicaService.createNewLocalTrial(trial.copy(trial = Some(newTrial)))
        }
      }
      println("save !!!!!!!!!")
    }

    bind("edcTrial", nodeSeq,
      "identifier" -> <div>{trial.identifier}</div>,
      "name" -> <div>{trial.name}</div>,
      "description" -> <div>{trial.description}</div>,
      "treatmentItem" -> generateTreatmentArmsSelect(nodeSeq),
    "treatmentArms" -> generateTreatmentArms(nodeSeq),
      "items" ->  <table class="randi2Table">
        <thead>
          <tr>
            <th></th>
            <th>Name</th>
            <th>Description</th>
            <th>Type</th>
          </tr>
        </thead>
        <tfoot>

        </tfoot>
        <tbody>
          {
          trial.getAllCriteria().flatMap(criterion =>
            <tr>
              <td>{ajaxCheckbox(false, check => {
                if (check){
                  selectedCriterions.add(criterion)
                }else {
                  selectedCriterions.remove(criterion)
                }
                clearAndGenerateCriterionTmp(selectedCriterions.toList)
              })}</td>
              <td>{criterion.name}</td>
              <td>{criterion.description}</td>
              <td>{criterion.getClass.getSimpleName}</td>
              <td>{criterion.toString}</td>
            </tr>
          )
          }
        </tbody>
      </table>
      ,
      "randomizationMethodSelect" -> randomizationMethodSelectField,
      "randomizationConfig" -> generateRandomizationConfigField,
      "save" -> button("save", save _)
    )
  }


  private def generateTreatmentArmsSelect(nodeSeq: NodeSeq): NodeSeq = {
    val trial = CurrentEDCTrial.get.get
    val criterionSeq = trial.getAllCriteria().filter(criterion => criterion.isInstanceOf[OrdinalCriterion]).map(criteria => (criteria, criteria.name))
   <div> {
    ajaxSelectObj(criterionSeq, Empty, (criterion: Criterion[Any, Constraint[Any]]) => {
      if (criterion.isInstanceOf[OrdinalCriterion]){
        armsTmp.clear()
        criterion.asInstanceOf[OrdinalCriterion].values.foreach(
          value => {
            armsTmp.append(new TreatmentArmTmp(id = Int.MinValue, version = 0, name = value, description = "", plannedSize = 0))
          })
      }
      Replace("treatmentArms", generateTreatmentArms(nodeSeq))
    })
     }</div>
  }

  private def generateTreatmentArms(xhtml: NodeSeq): NodeSeq = {
    <div id="treatmentArms">
      {val result = new ListBuffer[Node]()
    for (i <- armsTmp.indices) {
      val arm = armsTmp(i)
      result += <div class="singleField">
        <fieldset>
          <legend>Treatment arm
          </legend>
          <ul>
            <li>
              <label for={"armName" + i}>Name</label>{arm.name}
            </li>
            <li>
              <label for={"armDescription" + i}>Description</label>{arm.description}
            </li>
            <li>
              <label for={"armPlannedSize" + i}>Planned size</label>{ajaxText(arm.plannedSize.toString, (v) => arm.plannedSize = v.toInt, "id" -> ("armPlannedSize" + i)) /*TODO check toInt */}
            </li>
          </ul>
        </fieldset>
      </div>
    }
    NodeSeq fromSeq result}
    </div>
  }

  def randomizationMethodSelectField: NodeSeq = {
    ajaxSelect(randomizationMethodSelect, Empty, v => {
      randomizationMethodTmp = generateEmptyRandomizationMethodConfig(v)
      Replace("randomizationConfig", generateRandomizationConfigField)
    })
  }

  private def edit(nodeSeq: NodeSeq): NodeSeq = {
    <dev>add</dev>
  }

  private def clearFields {

  }


  def clearAndGenerateCriterionTmp(criteria: List[Criterion[Any, Constraint[Any]]]) {
    criterionsTmp.clear()
    criteria.foreach( criterion =>
        if (criterion.isInstanceOf[OrdinalCriterion]) {
          val values = new ListBuffer[String]()
          criterion.asInstanceOf[OrdinalCriterion].values.foreach(s => values += s)
          criterionsTmp += new CriterionTmp(criterion.id, criterion.version, "OrdinalCriterion", criterion.name, criterion.description, Some(values), getInclusionConstraintTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]), getStrataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]))
        } else if (criterion.isInstanceOf[DateCriterion])
          criterionsTmp += new CriterionTmp(criterion.id, criterion.version, "DateCriterion", criterion.name, criterion.description, None, getInclusionConstraintTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]), getStrataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]))
        else if (criterion.isInstanceOf[IntegerCriterion])
          criterionsTmp += new CriterionTmp(criterion.id, criterion.version, "IntegerCriterion", criterion.name, criterion.description, None, getInclusionConstraintTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]), getStrataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]))
        else if (criterion.isInstanceOf[DoubleCriterion])
          criterionsTmp += new CriterionTmp(criterion.id, criterion.version, "DoubleCriterion", criterion.name, criterion.description, None, getInclusionConstraintTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]), getStrataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]))
        else if (criterion.isInstanceOf[FreeTextCriterion])
          criterionsTmp += new CriterionTmp(criterion.id, criterion.version, "FreeTextCriterion", criterion.name, criterion.description, None, getInclusionConstraintTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]), getStrataTmp(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]))


  )
  }


  private def generateEmptyRandomizationMethodConfig(randomizationMethodName: String): RandomizationMethodConfigTmp = {
    val plugin = randomizationPluginManager.getPlugin(randomizationMethodName).get
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
    new RandomizationMethodConfigTmp(name = plugin.name, i18nName = plugin.i18nName, description = plugin.description, canBeUsedWithStratification = plugin.canBeUsedWithStratification, configurationEntries = methodConfigsTmp.asInstanceOf[List[RandomizationMethodConfigEntryTmp[Any]]])
  }

  private def generateRandomizationMethodConfig(randomizationMethod: Option[RandomizationMethod]): RandomizationMethodConfigTmp = {
    if (randomizationMethod.isEmpty) generateEmptyRandomizationMethodConfig(randomizationMethods.head)
    else {
      val method = randomizationMethod.get

      val plugin = randomizationPluginManager.getPluginForMethod(method).get
      val configurations = plugin.getRandomizationConfigurations(method.id)
      val methodConfigsTmp = configurations.map(configProp => {
        val config = configProp.configurationType
        if (config.getClass == classOf[BooleanConfigurationType]) {
          new RandomizationMethodConfigEntryTmp(config.asInstanceOf[BooleanConfigurationType], configProp.value)
        } else if (config.getClass == classOf[DoubleConfigurationType]) {
          new RandomizationMethodConfigEntryTmp(config.asInstanceOf[DoubleConfigurationType], configProp.value)
        } else if (config.getClass == classOf[IntegerConfigurationType]) {
          new RandomizationMethodConfigEntryTmp(config.asInstanceOf[IntegerConfigurationType], configProp.value)
        } else if (config.getClass == classOf[OrdinalConfigurationType]) {
          new RandomizationMethodConfigEntryTmp(config.asInstanceOf[OrdinalConfigurationType], configProp.value)
        }

      })
      new RandomizationMethodConfigTmp(name = plugin.name, i18nName = plugin.i18nName, description = plugin.description, canBeUsedWithStratification = plugin.canBeUsedWithStratification, configurationEntries = methodConfigsTmp.asInstanceOf[List[RandomizationMethodConfigEntryTmp[Any]]])
    }
  }

  private def generateRandomizationConfigField: Elem = {
    <div id="randomizationConfig">
      <fieldset>
        <legend>General informations</legend>
        <ul>
          <li>
            <label for="randomizationMethodName">Name:
            </label>
            <span id="randomizationMethodName">
              {randomizationMethodTmp.name}
            </span>
          </li>
          <li>
            <label for="randomizationMethodDescription">Description:
            </label>
            <span id="randomizationMethodDescription">
              {randomizationMethodTmp.description}
            </span>
          </li>
        </ul>
      </fieldset>{if (!randomizationMethodTmp.configurationEntries.isEmpty) {
      <fieldset>
        <legend>Configurations</legend>
        <ul>
          {randomizationMethodTmp.configurationEntries.flatMap(configuration => {
          <li>
            <label for={configuration.configurationType.name}>
              {configuration.configurationType.name}
              :
              <span class="tooltip">
                <img src="/images/icons/help16.png" alt={configuration.configurationType.description} title={configuration.configurationType.description}/> <span class="info">
                {configuration.configurationType.description}
              </span>
              </span>
            </label>{ajaxText(configuration.value.toString, v => {
            //TODO check value
            configuration.value = v
          }, "id" -> configuration.configurationType.name)}
          </li>
        })}
        </ul>
      </fieldset>
    } else <div></div>}{if (randomizationMethodTmp.canBeUsedWithStratification) {
      val criterionList = criterionsTmp
      <div>
        <h3>Stratification:</h3>
        Trial site stratification: {<span>TODO</span>}
        {val result = new ListBuffer[Node]()
      for (i <- criterionList.indices) {
        val criterion = criterionList(i)
        result += generateStratumConfig("stratum-" + criterion.name, criterion)
      }
      NodeSeq fromSeq result}
      </div>

    } else <div></div>}

    </div>
  }

  private def getInclusionConstraintTmp(crit: Criterion[Any, Constraint[Any]]): Option[ConstraintTmp] = {
    if (crit.inclusionConstraint.isDefined) {

      getConstraintTmp(crit.inclusionConstraint.get, crit)

    } else None
  }

  private def getStrataTmp(crit: Criterion[Any, Constraint[Any]]): ListBuffer[ConstraintTmp] = {
    val result = new ListBuffer[ConstraintTmp]()
    crit.strata.foreach(constraint => {
      val constrTmp = getConstraintTmp(constraint, crit)
      if (constrTmp.isDefined) result.append(constrTmp.get)
    })
    result
  }

  private def getConstraintTmp(constraint: Constraint[Any], crit: Criterion[Any, Constraint[Any]]): Option[ConstraintTmp] = {
    if (constraint.isInstanceOf[OrdinalConstraint]) {
      val actConstraint = constraint.asInstanceOf[OrdinalConstraint]
      val values = new mutable.HashSet[(Boolean, String)]()
      actConstraint.expectedValues.foreach(element => {
        values.add(true, element)
      })
      val actCriterion = crit.asInstanceOf[OrdinalCriterion]
      actCriterion.values.foreach(value => {
        if(!values.map(elem =>elem._2).contains(value))
          values.add(false, value)
      })
      Some(new ConstraintTmp(id = actConstraint.id, version = actConstraint.version, ordinalValues = values))

    } else if (constraint.isInstanceOf[IntegerConstraint]) {
      val actConstraint = constraint.asInstanceOf[IntegerConstraint]
      val firstValue = actConstraint.firstValue match {
        case None => None
        case Some(value) => Some(value.toString)
      }
      val secondValue = actConstraint.secondValue match {
        case None => None
        case Some(value) => Some(value.toString)
      }
      Some(new ConstraintTmp(id = actConstraint.id, version = actConstraint.version, minValue = firstValue, maxValue = secondValue))

    } else if (constraint.isInstanceOf[DoubleConstraint]) {
      val actConstraint = constraint.asInstanceOf[DoubleConstraint]
      val firstValue = actConstraint.firstValue match {
        case None => None
        case Some(value) => Some(value.toString)
      }
      val secondValue = actConstraint.secondValue match {
        case None => None
        case Some(value) => Some(value.toString)
      }
      Some(new ConstraintTmp(id = actConstraint.id, version = actConstraint.version, minValue = firstValue, maxValue = secondValue))

    } else if (constraint.isInstanceOf[DateConstraint]) {
      val actConstraint = constraint.asInstanceOf[DateConstraint]
      val firstValue = actConstraint.firstValue match {
        case None => None
        case Some(value) => Some(value.toString)
      }
      val secondValue = actConstraint.secondValue match {
        case None => None
        case Some(value) => Some(value.toString)
      }
      Some(new ConstraintTmp(id = actConstraint.id, version = actConstraint.version, minValue = firstValue, maxValue = secondValue))

    } else if (constraint.isInstanceOf[FreeTextConstraintNotEmpty]) {
      val actConstraint = constraint.asInstanceOf[FreeTextConstraintNotEmpty]
      Some(new ConstraintTmp(id = actConstraint.id, version = actConstraint.version, minValue = None, maxValue = None))
    } else None
  }

  private def generateStratumConfig(id: String, criterion: CriterionTmp): Elem = {
    if(criterion.typ != "FreeTextCriterion"){
      <div class="singleField" id={id}>
        <fieldset>
          <legend>
            {criterion.typ}
          </legend>
          <ul>
            <li>
              <label>Name</label>{criterion.name}
            </li>
            <li>
              <label>Description</label>{criterion.description}
            </li>
          </ul>
          <div>
            {ajaxButton("add stratum", () => {
            val constraint = new ConstraintTmp()
            if (criterion.typ == "OrdinalCriterion") {
              constraint.ordinalValues.clear()
              criterion.values.get.foreach(value => {
                constraint.ordinalValues.add((false, value))
              })
            }
            criterion.strata.append(constraint)
            Replace(id, generateStratumConfig(id, criterion))
          })}
            {ajaxButton("remove stratum", () => {
            criterion.strata.remove(criterion.strata.size-1)
            Replace(id, generateStratumConfig(id, criterion))
          })}
          </div>{val result = new ListBuffer[Node]()
        for (i <- criterion.strata.indices) {
          val constraint = criterion.strata(i)
          result += <div class="singleField">
            {//TODO stratum configuration
            generateStratumElement(id + i, criterion, constraint)}
          </div>
        }
        NodeSeq fromSeq result}
        </fieldset>
      </div>
    }else <div></div>
  }

  private def generateStratumElement(id: String, criterion: CriterionTmp, constraint: ConstraintTmp): Elem = {
    <fieldset id={id} class="inclusionConstraint">
      <legend>Constraint</legend>{if (criterion.typ != "OrdinalCriterion") {
      <ul>
        <li>
          {ajaxCheckbox(constraint.minValue.isDefined, v => {
          if (!v) {
            constraint.minValue = None
          } else {
            constraint.minValue = Some("")
          }
          Replace(id, generateStratumElement(id, criterion, constraint))
        }, "style" -> "width: 20px;")}
          lower boundary?
          {if (constraint.minValue.isDefined) {
          ajaxText(constraint.minValue.get, v => {
            constraint.minValue = Some(v)
          })
        }}
        </li>
        <li>
          {ajaxCheckbox(constraint.maxValue.isDefined, v => {
          if (!v) {
            constraint.maxValue = None
          } else {
            constraint.maxValue = Some("")
          }
          Replace(id, generateStratumElement(id, criterion, constraint))
        }, "style" -> "width: 20px;")}
          upper boundary?
          {if (constraint.maxValue.isDefined) {
          ajaxText(constraint.maxValue.get, v => {
            constraint.maxValue = Some(v)
          })
        }}
        </li>
      </ul>
    } else {
      val ordinalValues = constraint.ordinalValues
      ordinalValues.toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) < 0).flatMap(value => {
        <div>
          {ajaxCheckbox(value._1, v => {
          ordinalValues.remove(value)
          ordinalValues.add((v, value._2))
          Replace(id, generateStratumElement(id, criterion, constraint))
        })}<span>
          {value._2}
        </span>
        </div>
      })
    }}
    </fieldset>
  }


  private def generateEntry(id: String, failure: Boolean, element: Elem): Elem = {
    <li id={id + "Li"} class={if (failure) "errorHint" else ""}>
      <label for={id}>
        {id}
      </label>{element}<lift:msg id={id + "Msg"} errorClass="err"/>
    </li>
  }


  private def showErrorMessage(id: String, errors: NonEmptyList[String]) {
    S.error(id + "Msg", "<-" + errors.list.reduce((acc, el) => acc + ", " + el))
  }

  private def clearErrorMessage(id: String) {
    S.error(id + "Msg", "")
  }


  //TODO Refactor duplicated code
  private def createTreatmentArms(arms: ListBuffer[TreatmentArmTmp]): List[TreatmentArm] = {
    val result = ListBuffer[TreatmentArm]()

    arms.foreach(arm =>
      TreatmentArm(id = arm.id, version = arm.version, name = arm.name, description = arm.description, plannedSize = arm.plannedSize).either match {
        case Left(x) => S.error(x.toString()) //TODO error handling
        case Right(treatmentArm) => result += treatmentArm
      }
    )

    result.toList
  }


  private def createStages(actStages: HashMap[String, ListBuffer[CriterionTmp]]): Map[String, List[Criterion[Any, Constraint[Any]]]] = {
    val result = new HashMap[String, List[Criterion[Any, Constraint[Any]]]]()

    actStages.foreach(entry => result.put(entry._1, createCriterionsList(entry._2)))

    result.toMap
  }

  private def createCriterionsList(criterions: ListBuffer[CriterionTmp]): List[Criterion[Any, Constraint[Any]]] = {
    val result = ListBuffer[Criterion[Any, Constraint[Any]]]()

    criterions.foreach(criterionTmp => (criterionTmp.typ match {
      case "DateCriterion" => DateCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      case "IntegerCriterion" => IntegerCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      case "DoubleCriterion" => DoubleCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      case "FreeTextCriterion" => FreeTextCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
      case "OrdinalCriterion" => OrdinalCriterion(id = criterionTmp.id, version = criterionTmp.version, name = criterionTmp.name, description = criterionTmp.description, values = criterionTmp.values.get.toSet, inclusionConstraint = createInclusionConstraint(criterionTmp), strata = createStrata(criterionTmp))
    }).asInstanceOf[ValidationNEL[String, Criterion[Any, Constraint[Any]]]].either match {
      case Left(x) => S.error(x.toString()) //TODO error handling
      case Right(criterion) => result += criterion
    }
    )

    result.toList
  }


  private def createInclusionConstraint[T](criterionTmp: CriterionTmp): Option[T] = {
    if (criterionTmp.inclusionConstraint.isDefined) {
      createConstraint(criterionTmp, criterionTmp.inclusionConstraint.get)
    } else {
      None
    }
  }

  private def createStrata[T <: Constraint[Any]](criterionTmp: CriterionTmp): List[T] = {
    val list: List[T] = criterionTmp.strata.toList.
      map(constraintTmp => createConstraint(criterionTmp, constraintTmp).asInstanceOf[Option[T]]).
      filter(elem => elem.isDefined).map(elem => elem.get)
    list
  }


  private def createConstraint[T](criterionTmp: CriterionTmp, constraint: ConstraintTmp): Option[T] = {
    criterionTmp.typ match {
      case "DateCriterion" => {
        val min = if (constraint.minValue.isDefined) Some(new LocalDate(Utility.slashDate.parse(constraint.minValue.get).getTime)) else None
        val max = if (constraint.maxValue.isDefined) Some(new LocalDate(Utility.slashDate.parse(constraint.maxValue.get).getTime)) else None
        Some(DateConstraint(constraint.id, constraint.version, List(min, max)).toOption.get.asInstanceOf[T])
      }
      case "IntegerCriterion" => {
        val min = if (constraint.minValue.isDefined) Some(constraint.minValue.get.toInt) else None
        val max = if (constraint.maxValue.isDefined) Some(constraint.maxValue.get.toInt) else None
        Some(IntegerConstraint(constraint.id, constraint.version, List(min, max)).toOption.get.asInstanceOf[T])
      }
      case "DoubleCriterion" => {
        val min = if (constraint.minValue.isDefined) Some(constraint.minValue.get.toDouble) else None
        val max = if (constraint.maxValue.isDefined) Some(constraint.maxValue.get.toDouble) else None
        Some(DoubleConstraint(constraint.id, constraint.version, List(min, max)).toOption.get.asInstanceOf[T])
      }
      case "FreeTextCriterion" => {
        Some(FreeTextConstraintNotEmpty(constraint.id, constraint.version).toOption.get.asInstanceOf[T])
      }
      case "OrdinalCriterion" => {
        Some(OrdinalConstraint(constraint.id, constraint.version, constraint.ordinalValues.toList.filter(entry => entry._1).map(entry => Some(entry._2))).toOption.get.asInstanceOf[T])
      }
      case _ => None
    }
  }

}
