package org.randi3.web.snippet


import org.randi3.model.criterion._
import org.randi3.model.criterion.constraint._
import scala.xml._
import scala.xml.NodeSeq

import org.randi3.web.lib.DependencyFactory

import net.liftweb.common._
import net.liftweb.http.S._

import net.liftweb.http._

import js.JsCmds.{Replace, SetHtml}

import net.liftweb.util.Helpers._
import net.liftweb._

import common.Full
import http.SHtml._
import org.randi3.randomization._
import org.randi3.randomization.configuration._
import org.apache.commons.math3.random.MersenneTwister


import org.randi3.model._

import scalaz.{Empty => _, Node => _, _}
import Scalaz._
import org.randi3.web.util.{Utility, CurrentTrial}
import org.joda.time.LocalDate
import collection.mutable.{HashMap, ListBuffer, HashSet}
import org.joda.time.format.DateTimeFormat

import scala.Some
import xml.Text
import scala.Left

import scala.Right
import collection.mutable

import org.randi3.web.model._


class TrialSnippet extends StatefulSnippet with GeneralFormSnippet{

  private val trialSiteService = DependencyFactory.get.trialSiteService
  private val userService = DependencyFactory.get.userService
  private val trialService = DependencyFactory.get.trialService
  private val randomizationPluginManager = DependencyFactory.get.randomizationPluginManager


  def dispatch = {
    case "info" => redirectTo("/trial/list")
    case "show" => redirectTo("/trial/generalInformation")
    case "create" => create _
    case "editView" => redirectTo("/trial/editUsers")
    case "edit" => edit _
    case "editStatus" => editStatus _
    case "editParticipatingTrialSites" => editParticipatingTrialSites _
    case "editUsers" => editUsers _
    case "confirmDelete" => confirmDelete _
  }

  //TODO enumeration & text
  private val criterionTypes = Seq(("DateCriterion", "DateCriterion"),
    ("IntegerCriterion", "IntegerCriterion"),
    ("DoubleCriterion", "DoubleCriterion"),
    ("FreeTextCriterion", "FreeTextCriterion"),
    ("OrdinalCriterion", "OrdinalCriterion"))

  private var name = ""
  private var stageCount = 1
  private var abbreviation = ""
  private var description = ""
  private var startDate = new LocalDate
  private var endDate = new LocalDate
  private var trialSites = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name))
  //TODO error handling
  private var actualTrialSite: TrialSite = if (trialSites.isEmpty) null else trialSites.head._1
  private var principleInvestigator: org.randi3.model.User = null

  private var trialStatusTmp = TrialStatus.IN_PREPARATION.toString
  private var nameNewTreatmentArm = ""
  private var descriptionNewTreatmentArm = ""
  private var plannedSubjectSizeNewTreatmentArm = 0

  private var participatingSiteTmp: TrialSite = if (trialSites.isEmpty) null else trialSites.head._1
  private val participatingSites = new HashSet[TrialSite]()

  private val armsTmp = getEmptyArmsTmpList

  private def getEmptyArmsTmpList: ListBuffer[TreatmentArmTmp] = {
    val result = new ListBuffer[TreatmentArmTmp]()
    result += new TreatmentArmTmp(Int.MinValue, 0, "", "", 0)
    result
  }

  private var identificationCreationTypeTmp = TrialSubjectIdentificationCreationType.CONTINUOUS_COUNTER.toString

  private var criterionTypeTmp = "DateCriterion"

  private val criterionsTmp = new ListBuffer[CriterionTmp]()

  private var stageName = ""

  private val stages = new HashMap[String, ListBuffer[CriterionTmp]]()

  private var selectedUser: User = null
  private var selectedRole = Role.investigator
  private val actualRights = new HashSet[(User, TrialRight)]()

  private val randomizationMethods = randomizationPluginManager.getPluginNamesWithI18N


  private var randomizationMethodTmp = generateEmptyRandomizationMethodConfig(randomizationMethods.head._1)

  private var isTrialOpen: Boolean = true

  private var isStratifiedByTrialSite = false

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

  private def create(xhtml: NodeSeq): NodeSeq = {


    def save() {
      //TODO validate
      Trial(
        name = name,
        abbreviation = abbreviation,
        description = description,
        startDate = startDate,
        endDate = endDate,
        status = TrialStatus.IN_PREPARATION,
        treatmentArms = createTreatmentArms(armsTmp),
        criterions = createCriterionsList(criterionsTmp),
        participatingSites = participatingSites.toList,
        randomizationMethod = None,
        stages = createStages(stages),
        identificationCreationType = TrialSubjectIdentificationCreationType.withName(identificationCreationTypeTmp),
        isTrialOpen = isTrialOpen,
        isStratifiedByTrialSite = isStratifiedByTrialSite
      ).either match {
        case Left(x) => S.error("trialMsg", x.toString)
        case Right(trial) => {
          //TODO Random Config
          randomizationPluginManager.getPlugin(randomizationMethodTmp.name).get.randomizationMethod(new MersenneTwister(), trial, randomizationMethodTmp.getConfigurationProperties).either match {
            case Left(failure) =>  S.error("trialMsg", failure)
            case Right(randomMethod) => {
              val trialWithMethod = trial.copy(randomizationMethod = Some(randomMethod))
              trialService.create(trialWithMethod, principleInvestigator).either match {
                case Left(x) => S.error("trialMsg", x)
                case Right(b) => {
                  cleanVariables()
                  updateCurrentUser
                  S.notice("Thanks trial \"" + name + "\" saved!")
                  S.redirectTo("/trial/list")
                }
              }
            }
          }
        }
      }

    }

    generateForm(xhtml, save())

  }


  private def edit(xhtml: NodeSeq): NodeSeq = {

    setFields()

    def save() {
      val trial = CurrentTrial.get.get

      val randomMethod = randomizationPluginManager.getPlugin(randomizationMethodTmp.name).get.randomizationMethod(new MersenneTwister(), trial, randomizationMethodTmp.getConfigurationProperties).toOption.get
      val actTrial = trial.copy(
        name = name,
        abbreviation = abbreviation,
        description = description,
        startDate = startDate,
        endDate = endDate,
        treatmentArms = createTreatmentArms(armsTmp),
        criterions = createCriterionsList(criterionsTmp),
        participatingSites = participatingSites.toList,
        stages = createStages(stages),
        status = TrialStatus.withName(trialStatusTmp),
        identificationCreationType = TrialSubjectIdentificationCreationType.withName(identificationCreationTypeTmp),
        randomizationMethod = Some(randomMethod),
        isTrialOpen = isTrialOpen,
        isStratifiedByTrialSite = isStratifiedByTrialSite
      )
      trialService.update(actTrial)
      redirectTo("/trial/list")
    }

    generateForm(xhtml, save())
  }


  private def editStatus(xhtml: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.get
    setFields()

    def save() {
      val actTrial = trial.copy(status = TrialStatus.withName(trialStatusTmp))
      trialService.update(actTrial)
      redirectTo("/trial/list")
    }

    bind("trial", xhtml,
     "name" -> <span>{trial.name}</span>,
      "status" -> {
        if (trial.status == TrialStatus.ACTIVE) {
          trialStatusTmp = TrialStatus.PAUSED.toString
          ajaxSelect(Seq((TrialStatus.PAUSED.toString, TrialStatus.PAUSED.toString), (TrialStatus.FINISHED.toString, TrialStatus.FINISHED.toString)), Full(trialStatusTmp), trialStatusTmp = _)
        } else if (trial.status == TrialStatus.PAUSED) {
          trialStatusTmp = TrialStatus.ACTIVE.toString
          ajaxSelect(Seq((TrialStatus.ACTIVE.toString, TrialStatus.ACTIVE.toString), (TrialStatus.FINISHED.toString, TrialStatus.FINISHED.toString)), Full(trialStatusTmp), trialStatusTmp = _)
        } else {
          <span>{S.?("trial.changesNotPossible")}</span>
        }
      },
      "submit" -> submit(S.?("save"), save _, "class" -> "btnSend")
    )
  }


  private def editParticipatingTrialSites(xhtml: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.get
    setFields()

    def save() {
      val actTrial = trial.copy(participatingSites = participatingSites.toList)
      trialService.saveParticipatingSites(actTrial).either match {
        case Left(failure) => S.error("trialMsg", failure)
        case Right(trial) => {
          CurrentTrial.set(Some(trial))
          redirectTo("/trial/generalInformation")
        }
      }

    }

    bind("trial", xhtml,
      "name" -> <span>{trial.name}</span>,
      "participatingSiteSelect" -> ajaxSelectObj(trialSites, Empty, (trialSite: TrialSite) => participatingSiteTmp = trialSite, "id" -> "participatingSite"),
      "addParticipatingSite" -> ajaxButton(Text(S.?("add")), () => {
        participatingSites += participatingSiteTmp
        Replace("participatedTrialSiteTable", participatedSitesTable(false))
      }),
      "pSites" -> participatedSitesTable(false),
      "submit" -> submit(S.?("save"), save _, "class" -> "btnSend")
    )
  }


  private def editUsers(xhtml: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      redirectTo("/trial/list")
    }

    val allUsers = userService.getAllPossibleFromTrial(trial).either match {
      case Left(failure) => return <div>
        {failure}
      </div>
      case Right(users) => users

    }

    selectedUser = if (allUsers.isEmpty) null else allUsers.head

    val allUsersTrial = DependencyFactory.get.userService.getAllFromTrial(trial).either match {
      case Left(failure) => return <div>
        {failure}
      </div>
      case Right(users) => users

    }

    actualRights.clear()

    allUsersTrial.foreach(user => {

      val actUser = allUsers.find(actUser => actUser.id == user.id)
     if(actUser.isDefined){
      actUser.get.rights.foreach(right => {
        if (right.trial.id == trial.id)
          actualRights.add((actUser.get, TrialRight(right.role, trial).toOption.get))
      })
    }else {
       S.error("trialMsg", "The following user doesn't belong to one of the participating sites, please remove the user or add the site: " + user.username)
       val dbUser = userService.get(user.id).toOption.get
       dbUser.get.rights.foreach(right => {
         if (right.trial.id == trial.id)
           actualRights.add((dbUser.get, TrialRight(right.role, trial).toOption.get))
       })
    }

    })

    val rightsBefore = actualRights.toList
    def save() {

      val newRights = actualRights.toList.filter(actRight => !rightsBefore.contains(actRight))
      val removedRights = rightsBefore.filter(right => !actualRights.contains(right))

      newRights.foreach(userRight => userService.addTrialRight(userRight._1.id, userRight._2).either match {
        case Left(failure) =>  S.error("trialMsg", "Failure by adding a new right: "+ failure)
        case Right(x) =>
      })
      removedRights.foreach(userRight => userService.removeTrialRight(userRight._1.id, userRight._2).either match {
        case Left(failure) =>  S.error("trialMsg","Failure by removeing right: "+failure)
        case Right(x) =>
      })
      S.notice("Saved!")
    }

    def usersSelectField: Elem = {
      if (!allUsers.isEmpty) {
        ajaxSelectObj(allUsers.map(user => (user, user.username)).toSeq, Empty, (user: User) => {
          selectedUser = user
          Replace("roles", roleField)
        }, "id" -> "possibleUsers")
      } else {
        <span id="possibleUsers">{S.?("trial.noUsersAvailable")}</span>
      }
    }

    def roleField: Elem = {
      if (!allUsers.isEmpty) {
        val roles = Role.values.map(role => (role, role.toString)).toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) > 0)
        selectedRole = roles.head._1
        ajaxSelectObj(roles, Empty, (role: Role.Value) => selectedRole = role, "id" -> "roles")
      } else {
        <span id="roles"></span>
      }
    }

    def rights: Elem = {
      <table id="rights" class="randi2Table">
        <thead>
          <tr>
            <th>{S.?("menu.user")}</th>
            <th>{S.?("role")}</th>
            <th></th>
          </tr>
        </thead>{if (!actualRights.isEmpty) {
        <tfoot></tfoot>
      } else {
        <tfoot>
          <tr>
            <td rowspan="3">{S.?("trial.noUsersAvailable")}</td>
           </tr>
        </tfoot>
      }}<tbody>
        {actualRights.toList.sortWith((elem1, elem2) => (elem1._1.username + elem1._2.role.toString).compareTo((elem2._1.username + elem2._2.role.toString)) < 0).flatMap(entry => {
          <tr>
            <td>
              {entry._1.username}
            </td>
            <td>
              {entry._2.role.toString}
            </td>
            <td>
              {ajaxButton(S.?("remove"), () => {
              actualRights.remove(entry)
              Replace("rights", rights)
            })}
            </td>
          </tr>
        }
        )}
      </tbody>
      </table>
    }

    bind("trial", xhtml,
    "name" -> <span>{trial.name}</span>,
      "userSelect" -> usersSelectField,
      "roleSelect" -> roleField,
      "addRight" -> ajaxButton(S.?("add"), () => {
        actualRights.add((selectedUser, TrialRight(selectedRole, trial).toOption.get))
        Replace("rights", rights)
      }),
      "rights" -> rights,
      "submit" -> submit(S.?("save"), save _, "class" -> "btnSend")
    )
  }


  private def generateForm(xhtml: NodeSeq, code: => Unit): NodeSeq = {
    def nameField(failure: Boolean = false): Elem = {
      val id = "name"
      generateEntry(id, failure, {
        ajaxText(name, v => {
          name = v
          Trial.check(name = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", nameField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", nameField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def abbreviationField(failure: Boolean = false): Elem = {
      val id = "abbreviation"
      generateEntry(id, failure, {
        ajaxText(abbreviation, v => {
          abbreviation = v
          Trial.check(abbreviation = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", abbreviationField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", abbreviationField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def descriptionField(failure: Boolean = false): Elem = {
      val id = "description"
      generateEntry(id, failure, {
        ajaxTextarea(description, v => {
          description = v
          Trial.check(description = v).either match {
            case Left(x) => showErrorMessage(id, x); Replace(id + "Li", descriptionField(true))
            case Right(_) => clearErrorMessage(id); Replace(id + "Li", descriptionField(false))
          }
        }, "id" -> id)
      }
      )
    }

    def startDateField(failure: Boolean = false): Elem = {
      val id = "startDate"
      generateEntry(id, failure, {

        text(Utility.slashDate.format(startDate.toDate).toString, v => {
          startDate = new LocalDate(Utility.slashDate.parse(v).getTime)
          Trial.check(startDate = startDate).either match {
            case Left(x) => showErrorMessage(id, x)
            case Right(_) => clearErrorMessage(id)
          }
        }, "id" -> id)
      }
      )
    }

    def endDateField(failure: Boolean = false): Elem = {
      val id = "endDate"
      generateEntry(id, failure, {
        text(Utility.slashDate.format(endDate.toDate).toString, v => {
          endDate = new LocalDate(Utility.slashDate.parse(v).getTime)
          Trial.check(endDate = endDate).either match {
            case Left(x) => showErrorMessage(id, x)
            case Right(_) => clearErrorMessage(id)
          }
        }, "id" -> id)
      }
      )
    }

    def trialSiteField: Elem = {
      val id = "trialSite"
      generateEntry(id, false, {
        ajaxSelectObj(trialSites, Empty, (trialSite: TrialSite) => {
          actualTrialSite = trialSite
          Replace("principalInvestigatorLi", principalInvestigatorField)
        })
      })
    }

    def principalInvestigatorField: Elem = {
      val principleInvestigators = userService.getAll.either match {
        case Left(x) => S.error(x); return null //TODO error handling
        case Right(list) => if (actualTrialSite != null) list.filter(user => user.site.id == actualTrialSite.id) else Nil
      }
      //TODO Service db
      val id = "principalInvestigator"
      generateEntry(id, false, {
        if (principleInvestigators.isEmpty) {
          <span>No user at this trial site</span>
        } else {
          val first = principleInvestigators.head

          untrustedSelect(principleInvestigators.map(pInvestigator => (pInvestigator.id.toString, pInvestigator.lastName)), Full(first.id.toString), setPrincipleInvestigator(_), "id" -> id)
        }
      })
    }

    def isTrialOpenField: Elem = {
      val id = "isTrialOpen"
      generateEntry(id, false, {
        ajaxCheckbox(isTrialOpen, value => isTrialOpen = value)
      })
    }


    def randomizationMethodSelectField: NodeSeq = {
      ajaxSelect(randomizationMethods.toSeq, Empty, v => {
        randomizationMethodTmp = generateEmptyRandomizationMethodConfig(v)
        Replace("randomizationConfig", generateRandomizationConfigField)
      })
    }

    bind("trial", xhtml,
      "info" -> <span>{name}</span>,
      "name" -> nameField(),
      "abbreviation" -> abbreviationField(),
      "description" -> descriptionField(),
      "startDate" -> startDateField(),
      "endDate" -> endDateField(),
      "trialSite" -> trialSiteField,
      "pInvestigator" -> principalInvestigatorField,
      "status" -> ajaxSelect(TrialStatus.values.map(value => (value.toString, value.toString)).toSeq, Full(trialStatusTmp), trialStatusTmp = _),
      "participatingSiteSelect" -> ajaxSelectObj(trialSites, Empty, (trialSite: TrialSite) => participatingSiteTmp = trialSite, "id" -> "participatingSite"),
      "addParticipatingSite" -> ajaxButton(Text(S.?("add")), () => {
        participatingSites += participatingSiteTmp
        Replace("participatedTrialSiteTable", participatedSitesTable())
      }),
      "pSites" -> participatedSitesTable(),
      "isTrialOpen" -> isTrialOpenField,
      "treatmentArmName" -> ajaxText(nameNewTreatmentArm, nameNewTreatmentArm = _, "id" -> "nameNewTreatmentArm"),
      "treatmentArmDescription" -> ajaxText(descriptionNewTreatmentArm, descriptionNewTreatmentArm = _),
      "treatmentArmPlannedSubjects" -> ajaxText(plannedSubjectSizeNewTreatmentArm.toString, size => plannedSubjectSizeNewTreatmentArm = size.toInt),
      "addTreatmentArm" -> ajaxButton(S.?("add"), () => {
        armsTmp += new TreatmentArmTmp(Int.MinValue, 0, "", "", 0)
        Replace("treatmentArms", generateTreatmentArms(xhtml))
      }, "class" -> "btnNormal"),
      "treatmentArms" -> generateTreatmentArms(xhtml),
      //TODO selectElem
      "identificationCreationTypeSelect" -> ajaxSelect(TrialSubjectIdentificationCreationType.values.map(value => (value.toString, value.toString)).toSeq, Full(identificationCreationTypeTmp), identificationCreationTypeTmp = _),
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
       "cancel" -> submit("cancel", () => {
         cleanVariables()
         redirectTo("/trial/list")
       }, "class" -> "btnCancel"),
      "submit" -> submit(S.?("save"), code _, "class" -> "btnSend")
    )
  }

  private def participatedSitesTable(withRemoveOption: Boolean = true): NodeSeq = {
    <div id="participatedTrialSiteTable">
      <table width="90%">
        <tr>
          <th>{S.?("trialSite.name")}</th>
          <th>{S.?("trialSite.street")}</th>
          <th>{S.?("trialSite.postCode")}</th>
          <th>{S.?("trialSite.city")}</th>
          <th>{S.?("trialSite.country")}</th>
          <th></th>
        </tr>{participatingSites.toList.sortWith((e1, e2) => e1.name.compareTo(e2.name) < 0).flatMap(trialSite => <tr>
        <td>
          {trialSite.name}
        </td>
        <td>
          {trialSite.street}
        </td>
        <td>
          {trialSite.postCode}
        </td>
        <td>
          {trialSite.city}
        </td>
        <td>
          {trialSite.country}
        </td>
        <td>
          {
          if(withRemoveOption)
            ajaxButton(Text(S.?("remove")), () => {
              participatingSites.remove(trialSite)
              Replace("participatedTrialSiteTable", participatedSitesTable())
            })
          }
        </td>
      </tr>)}
      </table>
    </div>

  }

  private def generateRandomizationConfigField: Elem = {
    <div id="randomizationConfig" class="clearfix">
      <fieldset>
        <legend>{S.?("generalInformation")}</legend>
        <ul>
          <li>
            <label for="randomizationMethodName">{S.?("name")}:
            </label>
            <span id="randomizationMethodName">
              {randomizationMethodTmp.i18nName}
            </span>
          </li>
          <li>
            <label for="randomizationMethodDescription">{S.?("description")}:
            </label>
            <span id="randomizationMethodDescription">
              {randomizationMethodTmp.description}
            </span>
          </li>
        </ul>
      </fieldset>{if (!randomizationMethodTmp.configurationEntries.isEmpty) {
      <fieldset>
        <legend>{S.?("trial.randomizationConfiguration")}</legend>
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
      <fieldset>
        <legend>{S.?("trial.stratification")}</legend>
        <ul>
        <li>
         <label for="trialSiteStratification" >{S.?("trial.trialSiteStratification")}:</label>
        { checkbox(isStratifiedByTrialSite, value => isStratifiedByTrialSite = value, "id" -> "trialSiteStratification")}
        </li>
        {val result = new ListBuffer[Node]()
      for (i <- criterionList.indices) {
        val criterion = criterionList(i)
        result += generateStratumConfig("stratum-" + criterion.name, criterion)
      }
      NodeSeq fromSeq result}
        </ul>
      </fieldset>

    } else <div></div>}

    </div>
  }

  private def generateStratumConfig(id: String, criterion: CriterionTmp): Elem = {
   if(criterion.typ != "FreeTextCriterion"){
     <li>
      <fieldset id={id} class ="criterionForStrata">
        <legend>
          {criterion.typ}
        </legend>
        <ul>
          <li>
            <label>{S.?("name")}</label>{criterion.name}
          </li>
          <li>
            <label>{S.?("description")}</label>{criterion.description}
          </li>
        </ul>
        <div>
          {ajaxButton(S.?("trial.addStratum"), () => {
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
          {ajaxButton(S.?("remove"), () => {
          criterion.strata.remove(criterion.strata.size-1)
          Replace(id, generateStratumConfig(id, criterion))
        })}
        </div>{val result = new ListBuffer[Node]()
      for (i <- criterion.strata.indices) {
        val constraint = criterion.strata(i)
        result +=
          { generateStratumElement(id + i, criterion, constraint)}

      }
      NodeSeq fromSeq result}
      </fieldset>
     </li>
   }else <div></div>
  }

  private def generateStratumElement(id: String, criterion: CriterionTmp, constraint: ConstraintTmp): Elem = {
    <fieldset id={id} class="stratum">
      <legend>{S.?("group")}</legend>{if (criterion.typ != "OrdinalCriterion") {
      <ul>
        <li>
          <label for={"groupLowerBoundary" + id}> {S.?("lowerBoundary")}
          {ajaxCheckbox(constraint.minValue.isDefined, v => {
          if (!v) {
            constraint.minValue = None
          } else {
            constraint.minValue = Some("")
          }
          Replace(id, generateStratumElement(id, criterion, constraint))
        }, "id" -> ("groupLowerBoundary" + id))}
          </label>
          {if (constraint.minValue.isDefined) {
          ajaxText(constraint.minValue.get, v => {
            constraint.minValue = Some(v)
          })
        }}
        </li>
        <li>
          <label for={"upperBoundary" + id}> {S.?("upperBoundary")}
          {ajaxCheckbox(constraint.maxValue.isDefined, v => {
          if (!v) {
            constraint.maxValue = None
          } else {
            constraint.maxValue = Some("")
          }
          Replace(id, generateStratumElement(id, criterion, constraint))
        }, "id" -> ("upperBoundary" + id))}
          </label>
          {if (constraint.maxValue.isDefined) {
          ajaxText(constraint.maxValue.get, v => {
            constraint.maxValue = Some(v)
          })
        }}
        </li>
      </ul>
    } else {
      val ordinalValues = constraint.ordinalValues
      <ul>{
      ordinalValues.toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) < 0).flatMap(value => {
        <li>
          <label>{value._2}</label>
          {ajaxCheckbox(value._1, v => {
          ordinalValues.remove(value)
          ordinalValues.add((v, value._2))
          Replace(id, generateStratumElement(id, criterion, constraint))
        })}
        </li>
      })
      }</ul>
    }}
    </fieldset>
  }

  private def addSelectedCriterion(criterionType: String, criterionList: ListBuffer[CriterionTmp]) {
    def emptyValues = {
      val list = new ListBuffer[String]()
      list += ""
      list += ""
      Some(list)
    }
    criterionType match {
      case "OrdinalCriterion" => criterionList += new CriterionTmp(Int.MinValue, 0, "OrdinalCriterion", "", "", emptyValues, None)
      case x => criterionList += new CriterionTmp(Int.MinValue, 0, x, "", "", None, None)
    }
  }


  private def setPrincipleInvestigator(id: String) {
    principleInvestigator = userService.get(id.toInt).either match {
      case Left(failure) => S.error("trialMsg", failure); null
      case Right(user) => user.get
    }

  }

  private def cleanVariables() {
    name = ""
    abbreviation = ""
    stageCount = 1
    description = ""
    startDate = new LocalDate
    endDate = new LocalDate
    actualTrialSite = null
    trialSites = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO Failure handling
    principleInvestigator = null
    participatingSiteTmp = trialSites.sortWith((e1, e2) => e1._2.compareTo(e2._2) > 0).head._1
    //TODO clean participation sites
    armsTmp.clear()
    armsTmp += new TreatmentArmTmp(Int.MinValue, 0, "", "", 0)
    criterionsTmp.clear()
    cleanTreatmentArmVariables()
    isTrialOpen = false
    isStratifiedByTrialSite = false
  }

  private def cleanTreatmentArmVariables() {
    nameNewTreatmentArm = ""
    descriptionNewTreatmentArm = ""
    plannedSubjectSizeNewTreatmentArm = 0
  }

  private def setFields() {
    val trial = CurrentTrial.get.get
    name = trial.name
    abbreviation = trial.abbreviation
    description = trial.description
    startDate = trial.startDate
    endDate = trial.endDate
    actualTrialSite = null
    trialSites = trialSiteService.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name)) //TODO error handling
    principleInvestigator = null
    participatingSiteTmp =
      if (trialSites.size > 1) {
        trialSites.sortWith((e1, e2) => e1._2.compareTo(e2._2) > 0)(0)._1
      } else {
        trialSites(0)._1
      }

    participatingSites.clear()
    trial.participatingSites.foreach(site => participatingSites.add(site))


    armsTmp.clear()
    trial.treatmentArms.foreach(arm => armsTmp.append(new TreatmentArmTmp(arm.id, arm.version, arm.name, arm.description, arm.plannedSize)))


    trialStatusTmp = trial.status.toString

    identificationCreationTypeTmp = trial.identificationCreationType.toString

    isStratifiedByTrialSite = trial.isStratifiedByTrialSite
    isTrialOpen = trial.isTrialOpen

    criterionsTmp.clear()
    trial.criterions.foreach {
      criterion =>
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
    }

    randomizationMethodTmp = generateRandomizationMethodConfig(trial.randomizationMethod)

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

  private def showParticipatedSites(in: NodeSeq): NodeSeq = {
    <div id="participatedTrialSiteTable">
      <table width="90%">
        <tr>
          <th>{S.?("trialSite.name")}</th>
          <th>{S.?("street")}</th>
          <th>{S.?("postCode")}</th>
          <th>{S.?("city")}</th>
          <th>{S.?("country")}</th>
          <th></th>
        </tr>{participatingSites.toList.sortWith((e1, e2) => e1.name.compareTo(e2.name) < 0).flatMap(trialSite => <tr>
        <td>
          {trialSite.name}
        </td>
        <td>
          {trialSite.street}
        </td>
        <td>
          {trialSite.postCode}
        </td>
        <td>
          {trialSite.city}
        </td>
        <td>
          {trialSite.country}
        </td>
        <td>
          {ajaxButton(Text(S.?("remove")), () => {
          participatingSites.remove(trialSite)
          SetHtml("participatedTrialSiteTable", showParticipatedSites(in))
        })}
        </td>
      </tr>)}
      </table>
    </div>

  }

  private def generateTreatmentArms(xhtml: NodeSeq): NodeSeq = {
    <div id="treatmentArms">
      {val result = new ListBuffer[Node]()
    for (i <- armsTmp.indices) {
      val arm = armsTmp(i)
      result += <div class="singleField">
        <fieldset>
          <legend>{S.?("treatmentArm")}
            {ajaxButton(<img alt="remove" src="/images/icons/error16.png"/>, () => {
            armsTmp.remove(i)
            Replace("treatmentArms", generateTreatmentArms(xhtml))
          })}
          </legend>
          <ul>
            <li>
              <label for={"armName" + i}>{S.?("name")}</label>{ajaxText(arm.name, arm.name = _, "id" -> ("armName" + i))}
            </li>
            <li>
              <label for={"armDescription" + i}>{S.?("description")}</label>{ajaxTextarea(arm.description, arm.description = _, "id" -> ("armDescription" + i))}
            </li>
            <li>
              <label for={"armPlannedSize" + i}>{S.?("trial.plannedSubjectSize")}</label>{ajaxText(arm.plannedSize.toString, (v) => arm.plannedSize = v.toInt, "id" -> ("armPlannedSize" + i)) /*TODO check toInt */}
            </li>
          </ul>
        </fieldset>
      </div>
    }
    NodeSeq fromSeq result}
    </div>
  }


  private def generateGeneralCriterions(xhtml: NodeSeq, id: String, criterionList: ListBuffer[CriterionTmp]): NodeSeq = {
    <div id={id}>
      {val result = new ListBuffer[Node]()
    for (i <- criterionList.indices) {
      val criterion = criterionList(i)
      result += <div class="singleField">
        <fieldset>
          <legend>
            {criterion.typ}{ajaxButton(<img alt="remove" src="/images/icons/error16.png"/>, () => {
            criterionList.remove(i)
            Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
          })}
          </legend>
          <ul>
            <li>
              <label for={id + "Name" + i}>{S.?("name")}</label>{ajaxText(criterion.name, criterion.name = _, "id" -> (id + "Name" + i))}
            </li>
            <li>
              <label for={id + "Description" + +i}>{S.?("description")}</label>{ajaxTextarea(criterion.description, criterion.description = _, "id" -> (id + "Description" + +i))}
            </li>{createValues(criterion, xhtml, id, criterionList)}
          </ul>{generateInclusionConstraint(xhtml, "inclusionConstraintFieldset" + i, criterion)}

        </fieldset>
      </div>
    }
    NodeSeq fromSeq result}
    </div>
  }

  private def generateCriterions(xhtml: NodeSeq): NodeSeq = {
    generateGeneralCriterions(xhtml, "criterions", criterionsTmp)
  }

  private def generateInclusionConstraint(xhtml: NodeSeq, id: String, criterion: CriterionTmp): NodeSeq = {

    <fieldset id={id} class="inclusionConstraint">
      <legend>{S.?("trial.inclusionConstraint")}
        {ajaxCheckbox(criterion.inclusionConstraint.isDefined, v => {
        if (!v) criterion.inclusionConstraint = None
        else {
          criterion.inclusionConstraint = Some(new ConstraintTmp())
          if (criterion.typ == "OrdinalCriterion") {
            criterion.inclusionConstraint.get.ordinalValues.clear()
            criterion.values.get.foreach(value => {
              criterion.inclusionConstraint.get.ordinalValues.add((false, value))
            })
          }
        }
        Replace(id, generateInclusionConstraint(xhtml, id, criterion))
      }, "class" ->"inclustionConstraintLegendCheckbox")}
      </legend>{if (criterion.inclusionConstraint.isDefined) {
       if(criterion.typ == "OrdinalCriterion") {
        val ordinalValues = criterion.inclusionConstraint.get.ordinalValues
         <fieldset class="noBorder">
           <ul>{
        ordinalValues.toList.sortWith((elem1, elem2) => elem1._2.compareTo(elem2._2) < 0).flatMap(value => {
          <li>
            <label for={"ordIncValue"+value._2}> {value._2}</label>
            {ajaxCheckbox(value._1, v => {
            ordinalValues.remove(value)
            ordinalValues.add((v, value._2))
            Replace(id, generateInclusionConstraint(xhtml, id, criterion))
          }, "id" ->("ordIncValue"+value._2))}
          </li>
        })
           }
           </ul>
        </fieldset>
      } else if(criterion.typ == "FreeTextCriterion") {
         <fieldset class="noBorder">
          <ul>
           <li>
             <div>{S.?("elementIsNecessary")}</div>
           </li>
         </ul>
           </fieldset>
      } else {
         <fieldset class="noBorder">
         <ul>
           <li>
             <label for =""> {S.?("lowerBoundary")}</label>
             {ajaxCheckbox(criterion.inclusionConstraint.get.minValue.isDefined, v => {
             if (!v) {
               criterion.inclusionConstraint.get.minValue = None
             } else {
               criterion.inclusionConstraint.get.minValue = Some("")
             }
             Replace(id, generateInclusionConstraint(xhtml, id, criterion))
           }, "id" -> ("lowerBoundary"+ id))}

             {if (criterion.inclusionConstraint.get.minValue.isDefined) {
             ajaxText(criterion.inclusionConstraint.get.minValue.get, v => {
               criterion.inclusionConstraint.get.minValue = Some(v)
             })
           }}
           </li>
           <li>
             <label for ="">  {S.?("upperBoundary")}</label>
             {ajaxCheckbox(criterion.inclusionConstraint.get.maxValue.isDefined, v => {
             if (!v) {
               criterion.inclusionConstraint.get.maxValue = None
             } else {
               criterion.inclusionConstraint.get.maxValue = Some("")
             }
             Replace(id, generateInclusionConstraint(xhtml, id, criterion))
           }, "id" -> ("upperBoundary"+ id))}

             {if (criterion.inclusionConstraint.get.maxValue.isDefined) {
             ajaxText(criterion.inclusionConstraint.get.maxValue.get, v => {
               criterion.inclusionConstraint.get.maxValue = Some(v)
             })
           }}
           </li>
         </ul>
         </fieldset>
       }
    } else {
      <span>{S.?("noInclusionConstraint")}</span>
    }}
    </fieldset>
  }


  private def createValues(criterion: CriterionTmp, xhtml: NodeSeq, id: String, criterionList: ListBuffer[CriterionTmp]): NodeSeq = {
    criterion.values match {
      case None => <span></span>
      case Some(x) => {
        //TODO implement specific replacement
        <li>
          <fieldset class ="criterionValues">
            <legend>{S.?("values")}
              {ajaxButton(S.?("add"), () => {
              x += ""
              criterion.inclusionConstraint = None
              Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
            })}</legend>
            <ul>
                {val result = new ListBuffer[Node]()
              for (i <- x.indices) result += <li>
                {ajaxText(x(i), v => {
                  x(i) = v
                  criterion.inclusionConstraint = None
                  Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
                })}{ajaxButton(S.?("remove"), () => {
                  x.remove(i)
                  criterion.inclusionConstraint = None
                  Replace(id, generateGeneralCriterions(xhtml, id, criterionList))
                })}
              </li>
              NodeSeq fromSeq result}
            </ul>
          </fieldset>
        </li>
      }
    }
  }

  private def generateStages(xhtml: NodeSeq): NodeSeq = {
    <div id="stagesTabs">
      {val result = new ListBuffer[Node]()
    for (key <- stages.keySet.toList.sortWith((first, second) => first.compareTo(second) < 0)) {
      val stageElements = stages.get(key).get
      val id = key + "Criterions"
      var criterionType = "DateType"
      result += <div class="singleField">
        <fieldset>
          <legend>
            {key}{ajaxButton(<img alt="remove" src="/images/icons/error16.png"/>, () => {
            stages.remove(key)
            Replace("stagesTabs", generateStages(xhtml))
          })}
          </legend>
          <ul>
            <li>{S.?("pleaseSelect")}:
              {ajaxSelect(criterionTypes, Empty, criterionType = _)}{ajaxButton(S.?("add"), () => {
              addSelectedCriterion(criterionType, stageElements)
              Replace(id, generateGeneralCriterions(xhtml, id, stageElements))
            })}
            </li>
            <li>
              {generateGeneralCriterions(xhtml, id, stageElements)}
            </li>
          </ul>
        </fieldset>
      </div>
    }
    NodeSeq fromSeq result}
    </div>
  }

  private def confirmDelete(in: NodeSeq): NodeSeq = {
    val trial = CurrentTrial.get.getOrElse {
      error("Trial not found")
      redirectTo("/trial/list.html")
    }

    def deleteTrial() {
      notice("Trial " + (trial.name) + " deleted")
      trialService.delete(trial)
      redirectTo("/trial/list.html")
    }

    // bind the incoming XHTML to a "delete" button.
    // when the delete button is pressed, call the "deleteUser"
    // function (which is a closure and bound the "user" object
    // in the current content)
    bind("xmp", in, "name" -> (trial.name),
      "delete" -> submit("Delete", deleteTrial _))

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
    if (randomizationMethod.isEmpty) generateEmptyRandomizationMethodConfig(randomizationMethods.head._1)
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
      new RandomizationMethodConfigTmp(name = plugin.name, i18nName =plugin.i18nName, description = plugin.description, canBeUsedWithStratification = plugin.canBeUsedWithStratification, configurationEntries = methodConfigsTmp.asInstanceOf[List[RandomizationMethodConfigEntryTmp[Any]]])
    }
  }

}