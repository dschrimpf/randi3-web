package bootstrap.liftweb


import net.liftweb.common._
import net.liftweb.http.provider._
import net.liftweb.http._


import net.liftweb.util._

import org.randi3.web.util.{CurrentSelectedUser, CurrentTrial, CurrentUser}
import org.randi3.model.{TrialStatus, Role}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.widgets.flot._
import org.randi3.configuration.{ConfigurationSchema, ConfigurationService, ConfigurationValues}
import org.randi3.web.lib.DependencyFactory
import org.scalaquery.meta.MTable
import org.randi3.web.snippet.DownloadRandomizationData
import org.randi3.schema.DatabaseSchema
import org.randi3.utility.Logging

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Logging {

  def boot {

    checkAndGenerateConfigDatabase()


    if(DependencyFactory.configurationService.isConfigurationComplete)
     checkAndGenerateRandomizationTables()


    // where to search snippet
    LiftRules.addToPackages("org.randi3.web")
    //    Schemifier.schemify(true, Schemifier.infoF _, User, TrialSiteLift)

    Flot.init

    lazy val noGAE = Unless(() => Props.inGAE, "Disabled for GAE")

    val trialSiteMenu = Menu("Trial site") / "trialSiteInfo" submenus(
      Menu(Loc("trialSiteAdd", List("trialSite", "add"), "add", If(() => isAdministrator, ""))),
      Menu(Loc("trialSiteList", List("trialSite", "list"), "list")),
      Menu(Loc("trialSiteEdit", List("trialSite", "edit"), "edit", Hidden, If(() => isAdministrator, ""))),
      Menu(Loc("trialSiteDelete", List("trialSite", "delete"), "delete", Hidden, If(() => isAdministrator, ""))))

    val userMenu = Menu("User") / "userInfo" >> If(() => CurrentUser.isDefined, "") submenus(
      Menu(Loc("userAdd", List("user", "add"), "add", If(() => isAdministrator, ""))),
      Menu(Loc("userList", List("user", "list"), "list")),
      Menu(Loc("userEdit", List("user", "edit"), "edit", If(() => isAdministrator || isOwnUser, ""))),
      Menu(Loc("userShow", List("user", "show"), "show", If(() => isAdministrator || isOwnUser, ""))),
      Menu(Loc("userDelete", List("user", "delete"), "delete", Hidden)))

    val trialMenu = Menu("Trial") / "trialInfo" submenus(
      Menu(Loc("trialAdd", List("trial", "add"), "add", If(() => canCreateTrial, ""))),
      Menu("show") / "trialShow" >> If(() => isTrialSelected, "") submenus(
        Menu(Loc("trialShowGeneral", List("trial", "generalInformation"), "General Information", If(() => isTrialSelected, ""))),
        Menu(Loc("trialShowRadomizationData", List("trial", "randomizationData"), "Randomization Data", If(() => canViewTrialInformation, ""))),
        Menu(Loc("trialShowRadomizationDataInvestigator", List("trial", "randomizationDataInvestigator"), "Own Randomization Data", If(() => (isInvestigator && !canViewTrialInformation), ""))),
        Menu(Loc("trialShowAudit", List("trial", "audit"), "Audit", If(() => canViewTrialInformation, ""))),
        Menu(Loc("trialShowUsers", List("trial", "users"), "Users", If(() => canViewTrialInformation, "")))
        ),
      Menu("edit") / "trialEdit" >> If(() => canViewTrialEdit, "") submenus(
        Menu(Loc("trialEditGeneral", List("trial", "editGeneralData"), "General Data", If(() => canChangeTrial, ""))),
        Menu(Loc("trialEditStatus", List("trial", "editTrialStatus"), "Status", If(() => canChangeTrialStatus, ""))),
        Menu(Loc("trialEditUsers", List("trial", "editUsers"), "Users", If(() => isTrialSelected, "")))
        ),
      Menu(Loc("trialList", List("trial", "list"), "list")),
      Menu(Loc("trialDelete", List("trial", "delete"), "delete", Hidden, If(() => canChangeTrial, "")))
      )

    val trialSubjectMenu = Menu(Loc("trialSubjectRandomize", List("trialSubject", "randomize"), "Randomize", If(() => canRandomize, "")))

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu("Home") / "index", // Simple menu form
      Menu("Login") / "login" >> If(() => CurrentUser.isEmpty && DependencyFactory.configurationService.isConfigurationComplete, ""),
      Menu("Register") / "register" >> If(() => CurrentUser.isEmpty && DependencyFactory.configurationService.isConfigurationComplete, ""),
      trialSiteMenu >> If(() => CurrentUser.isDefined, ""),
      userMenu >> If(() => CurrentUser.isDefined, ""),
      trialMenu >> If(() => CurrentUser.isDefined, ""),
      trialSubjectMenu,
      Menu("Install") / "install" >> If(() => !DependencyFactory.configurationService.isConfigurationComplete, ""),
      // Menu with special Link
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
        "About")))

    LiftRules.setSiteMapFunc(() => sitemap())

    LiftRules.dispatch.append(DownloadRandomizationData)

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart = {
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader-fix").cmd)

    }


    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd = {
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
      Full(() => LiftRules.jsArtifacts.show("ajax-loader-fix").cmd)
    }


    LiftRules.early.append(makeUtf8)

    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }


  private def isAdministrator: Boolean = {
    CurrentUser.getOrElse(return false).administrator
  }

  private def isOwnUser: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val selectedUser = CurrentSelectedUser.getOrElse(return false)
    user.id == selectedUser.id && user.username == selectedUser.username
  }


  private def canCreateTrial: Boolean = {
    CurrentUser.getOrElse(return false).canCreateTrial
  }

  private def isTrialSelected: Boolean = {
    if (CurrentUser.isEmpty) (return false)
    CurrentTrial.isDefined
  }

  private def canViewTrialInformation: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      val roles = rightList.map(right => right.role)
      roles.contains(Role.principleInvestigator) || roles.contains(Role.statistician) || roles.contains(Role.trialAdministrator) || roles.contains(Role.monitor)
    }
  }


  private def canViewTrialEdit: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      val roles = rightList.map(right => right.role)
      roles.contains(Role.principleInvestigator) || roles.contains(Role.trialAdministrator)
    }
  }

  private def isInvestigator: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      rightList.map(right => right.role).contains(Role.investigator)
    }
  }

  private def canRandomize: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    if (trial.status != TrialStatus.ACTIVE) return false
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      rightList.map(right => right.role).contains(Role.investigator)
    }
  }

  private def canChangeTrial: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    if (trial.status != TrialStatus.IN_PREPARATION) return false
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      rightList.map(right => right.role).contains(Role.principleInvestigator) || rightList.map(right => right.role).contains(Role.trialAdministrator)
    }
  }

  private def canChangeTrialStatus: Boolean = {
    val user = CurrentUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    if (trial.status != TrialStatus.ACTIVE && trial.status != TrialStatus.PAUSED) return false
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      rightList.map(right => right.role).contains(Role.principleInvestigator) || rightList.map(right => right.role).contains(Role.trialAdministrator)
    }
  }

  private def checkAndGenerateConfigDatabase() {

    val database = ConfigurationSchema.getDatabase._1
    val tableList = MTable.getTables.list()(database.createSession())

    if (tableList.isEmpty) {
      logger.info("Start of installation ...")
      ConfigurationSchema.createDatabase
    }
  }

  private def checkAndGenerateRandomizationTables() {
    val database =   DependencyFactory.database
    import org.scalaquery.session.Database.threadLocalSession
    import DependencyFactory.driver.Implicit._

    val  pluginManager = DependencyFactory.randomizationPluginManager

    pluginManager.getPluginNames.foreach(pluginName => {
      val plugin = pluginManager.getPlugin(pluginName).get
      if (plugin.databaseTables().isDefined){
        try {
        database withSession {
          plugin.databaseTables().get.create
        }
        }catch {
          case e: Exception =>
        }
      }

    })

  }
}
