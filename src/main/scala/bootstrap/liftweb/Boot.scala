package bootstrap.liftweb


import net.liftweb.common._
import net.liftweb.http.provider._
import net.liftweb.http._


import net.liftweb.util._

import org.randi3.web.util.{CurrentTrialSite, CurrentLoggedInUser, CurrentTrial, CurrentUser}
import org.randi3.model.{TrialStatus, Role}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._

import org.randi3.configuration.{ConfigurationServiceComponent, ConfigurationSchema}

import org.randi3.web.snippet.DownloadRandomizationData

import org.randi3.utility.{Utility, Logging}

import org.randi3.schema.LiquibaseUtil
import java.sql.SQLSyntaxErrorException
import java.util.Locale
import net.liftmodules.widgets.flot.Flot
import scala.slick.jdbc.meta.MTable


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Utility with Logging with ConfigurationServiceComponent {

  val configurationService = new ConfigurationService

  def boot {

    Locale.setDefault(Locale.ENGLISH)


    initializeJDBCDriver()

    checkAndGenerateConfigDatabase()


    if(configurationService.isConfigurationComplete) {
      LiquibaseUtil.updateDatabase(org.randi3.web.lib.DependencyFactory.get.database)
        //TODO use properties from sub project
    //   LiquibaseUtil.updateDatabase(DependencyFactory.get.database, "db/db.changelog-master-edc.xml")
       checkAndGenerateRandomizationTables()
     }

    LiftRules.resourceNames =  "i18n/Messages" :: LiftRules.resourceNames

    LiftRules.localeCalculator =  calcLocale _

    // where to search snippet
    LiftRules.addToPackages("org.randi3.web")
    //    Schemifier.schemify(true, Schemifier.infoF _, User, TrialSiteLift)

    Flot.init()

    lazy val noGAE = Unless(() => Props.inGAE, "Disabled for GAE")

    val trialSiteMenu = Menu(S.?("menu.trialSite")) / "trialSiteInfo" submenus(
      Menu(Loc("trialSiteList", List("trialSite", "list"), S.?("menu.list"))),
      Menu(Loc("trialSiteAdd", List("trialSite", "add"), S.?("menu.add"), If(() => isAdministrator, ""))),
      Menu(Loc("trialSiteEdit", List("trialSite", "edit"), S.?("menu.edit"), If(() => isAdministrator, ""), Hidden)),
      Menu(Loc("trialSiteActive", List("trialSite", "activate"), S.?("menu.trialSiteActivate"), If(() => isAdministrator && CurrentTrialSite.isDefined && !CurrentTrialSite.get.get.isActive, ""), Hidden)),
      Menu(Loc("trialSiteDeactivated", List("trialSite", "deactivate"), S.?("menu.trialSiteDeactivate"), If(() => isAdministrator && CurrentTrialSite.isDefined && CurrentTrialSite.get.get.isActive, ""), Hidden)),
      Menu(Loc("trialSiteDelete", List("trialSite", "delete"), "delete", Hidden, If(() => isAdministrator, ""))))

    val userMenu = Menu(S.?("menu.user")) / "userInfo" >> If(() => CurrentLoggedInUser.isDefined, "") submenus(
      Menu(Loc("userList", List("user", "list"), S.?("menu.list"))),
      Menu(Loc("userAdd", List("user", "add"), S.?("menu.add"), If(() => isAdministrator, ""))),
      Menu(Loc("userShow", List("user", "show"), "show", If(() => isAdministrator || isOwnUser, ""), Hidden)),
      Menu(Loc("userEdit", List("user", "edit"),  S.?("menu.edit"), If(() => isAdministrator || isOwnUser, ""), Hidden)),
      Menu(Loc("userDelete", List("user", "delete"), "delete", Hidden)))

    val trialMenu = Menu(S.?("menu.trial")) / "trialInfo" submenus(
      Menu(Loc("trialList", List("trial", "list"), S.?("menu.list"))),
      Menu(Loc("trialAdd", List("trial", "add"), S.?("menu.add"), If(() => canCreateTrial, ""))),
      Menu(S.?("trial.show")) / "trialShow" >> If(() => isTrialSelected, "") submenus(
        Menu(Loc("trialShowGeneral", List("trial", "generalInformation"), S.?("menu.generalInformation"), If(() => isTrialSelected, ""))),
        Menu(Loc("trialShowRadomizationData", List("trial", "randomizationData"), S.?("menu.randomizationData"), If(() => canViewTrialInformation, ""))),
        Menu(Loc("trialShowRadomizationDataInvestigator", List("trial", "randomizationDataInvestigator"),  S.?("menu.ownRandomizationData"), If(() => (isInvestigator && !canViewTrialInformation), ""))),
        Menu(Loc("trialShowAudit", List("trial", "audit"),  S.?("audit"), If(() => canViewTrialInformation, ""))),
        Menu(Loc("trialShowUsers", List("trial", "users"),  S.?("menu.users"), If(() => canViewTrialInformation, "")))
        ),
      Menu(S.?("menu.edit")) / "trialEdit" >> If(() => canViewTrialEdit, "") submenus(
        Menu(Loc("trialEditGeneral", List("trial", "editGeneralData"), S.?("menu.generalInformation"), If(() => canChangeTrial, ""))),
        Menu(Loc("trialEditStatus", List("trial", "editTrialStatus"), S.?("trial.status"), If(() => canChangeTrialStatus, ""))),
        Menu(Loc("trialEditSites", List("trial", "editParticipatingTrialSites"), S.?("trialSites"), If(() => canChangeParticipatingTrialSites, ""))),
        Menu(Loc("trialEditUsers", List("trial", "editUsers"), S.?("menu.users"), If(() => isTrialSelected, "")))
        ),
      Menu(Loc("trialDelete", List("trial", "delete"), "delete", Hidden, If(() => canChangeTrial, "")))
      )

    val trialSubjectMenu = Menu(Loc("trialSubjectRandomize", List("trialSubject", "randomize"), S.?("Randomize"), If(() => canRandomize, "")))
    val trialSubjectRandomizationResultMenu = Menu(Loc("trialSubjectRandomizationResult", List("trialSubject", "randomizationResult"), "Randomization result", Hidden ,If(() => canRandomize, "")))
    val trialSubjectStageMenu = Menu(Loc("trialSubjectStage", List("trialSubject", "addResponse"), S.?("AddResponse"), If(() => canRandomize, "")))

   val edcMenu = Menu("EDC") / "edcInfo" >> If(() => CurrentLoggedInUser.isDefined, "") submenus(
      Menu(Loc("edcTrialAdd", List("edcTrial", "listRemote"), "list remote EDC trials", If(() => isAdministrator, ""))),
      Menu(Loc("edcTrialEdit", List("edcTrial", "addOrEdit"), "edit EDC trial",  If(() => isAdministrator, ""))),
      Menu(Loc("edcTrialView", List("edcTrial", "viewRemoteDetails"), "view EDC trial",  If(() => isAdministrator, ""))),
      Menu(Loc("edcTrialList", List("edcTrial", "list"), "list EDC trials", If(() => isAdministrator || isOwnUser, ""))))


    // Build SiteMap
    def sitemap() = SiteMap(
      Menu("Home") / "index", // Simple menu form
      Menu(S.?("menu.login")) / "login" >> If(() => CurrentLoggedInUser.isEmpty && configurationService.isConfigurationComplete, ""),
      Menu(S.?("menu.register")) / "register" >> If(() => CurrentLoggedInUser.isEmpty && configurationService.isConfigurationComplete, ""),
      trialSiteMenu >> If(() => CurrentLoggedInUser.isDefined, ""),
      userMenu >> If(() => CurrentLoggedInUser.isDefined, ""),
      trialMenu >> If(() => CurrentLoggedInUser.isDefined, ""),
      trialSubjectMenu,
    //  edcMenu,
      trialSubjectRandomizationResultMenu,
      trialSubjectStageMenu,
      Menu(Loc("installServerURL", List("installation", "serverURL"), "serverURL", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      Menu(Loc("installDatabase", List("installation", "database"), "database", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      Menu(Loc("installMail", List("installation", "mail"), "mail", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      Menu(Loc("installPluginPath", List("installation", "pluginPath"), "pluginPath", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      Menu(Loc("installTrialSite", List("installation", "trialSite"), "trialSite", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      Menu(Loc("installUser", List("installation", "user"), "user", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      Menu(Loc("installComplete", List("installation", "finish"), "complete", If(() => !configurationService.isConfigurationComplete, ""), Hidden)),
      // Menu(S.?("menu.install")) / "install" >> If(() => !configurationService.isConfigurationComplete, ""),
      Menu(S.?("menu.support")) / "support" >> If(() => configurationService.isConfigurationComplete, ""),
      // Menu with special Link
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
        S.?("menu.about"))))

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


  }

  private def calcLocale(in: Box[HTTPRequest]): Locale = {
   val loc = if (CurrentLoggedInUser.isEmpty) Locale.getDefault
    else CurrentLoggedInUser.get.get.locale
    loc
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }


  private def isAdministrator: Boolean = {
    CurrentLoggedInUser.getOrElse(return false).administrator
  }

  private def isOwnUser: Boolean = {
    val user = CurrentLoggedInUser.getOrElse(return false)
    val selectedUser = CurrentUser.getOrElse(return false)
    user.id == selectedUser.id && user.username == selectedUser.username
  }


  private def canCreateTrial: Boolean = {
    CurrentLoggedInUser.getOrElse(return false).canCreateTrial
  }

  private def isTrialSelected: Boolean = {
    if (CurrentLoggedInUser.isEmpty) (return false)
    CurrentTrial.isDefined
  }

  private def canViewTrialInformation: Boolean = {
    val user = CurrentLoggedInUser.getOrElse(return false)
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
    val user = CurrentLoggedInUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if(trial.status == TrialStatus.FINISHED){
      false
    } else if (rightList.isEmpty) {
      false
    } else {
      val roles = rightList.map(right => right.role)
      roles.contains(Role.principleInvestigator) || roles.contains(Role.trialAdministrator)
    }
  }

  private def isInvestigator: Boolean = {
    val user = CurrentLoggedInUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      rightList.map(right => right.role).contains(Role.investigator)
    }
  }

  private def canRandomize: Boolean = {
    val user = CurrentLoggedInUser.getOrElse(return false)
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
    val user = CurrentLoggedInUser.getOrElse(return false)
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
    val user = CurrentLoggedInUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    if (trial.status != TrialStatus.ACTIVE && trial.status != TrialStatus.PAUSED) return false
    val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false
    } else {
      rightList.map(right => right.role).contains(Role.principleInvestigator) || rightList.map(right => right.role).contains(Role.trialAdministrator)
    }
  }

  private def canChangeParticipatingTrialSites: Boolean = {
    val user = CurrentLoggedInUser.getOrElse(return false)
    val trial = CurrentTrial.getOrElse(return false)
    if (!trial.isTrialOpen) return false
    if (trial.status != TrialStatus.ACTIVE && trial.status != TrialStatus.PAUSED) return false
     val rightList = user.rights.filter(right => right.trial.id == trial.id)
    if (rightList.isEmpty) {
      false

    } else {
      rightList.map(right => right.role).contains(Role.principleInvestigator) || rightList.map(right => right.role).contains(Role.trialAdministrator)
    }
  }


  private def initializeJDBCDriver(){
    java.lang.Class.forName("org.hsqldb.jdbc.JDBCDriver")
    java.lang.Class.forName("com.mysql.jdbc.Driver")
    java.lang.Class.forName("org.postgresql.Driver")
  }

  private def checkAndGenerateConfigDatabase() {

    val database = ConfigurationSchema.getDatabase._1
    val tableList = MTable.getTables.list()(database.createSession())

    if (tableList.isEmpty) {
      logger.info("Start of installation ...")
      try {
      ConfigurationSchema.createDatabase
      }catch {
        case e: SQLSyntaxErrorException => //HsqlDB   MTable.getTables doesn't work
      }
    }
  }

  private def checkAndGenerateRandomizationTables() {
    import org.randi3.web.lib.DependencyFactory

    val  pluginManager = DependencyFactory.get.randomizationPluginManager

    pluginManager.getPluginNames.foreach(pluginName => {
      val plugin = pluginManager.getPlugin(pluginName).get
      plugin.updateDatabase()
    })

  }


}

