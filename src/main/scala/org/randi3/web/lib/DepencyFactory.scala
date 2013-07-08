import org.randi3.web.lib.DependencyFactory
package org.randi3.web {

package lib {


import org.randi3.dao._

import org.randi3.service._

import org.randi3.randomization._
import org.randi3.model.User
import org.randi3.web.util.CurrentLoggedInUser
import org.randi3.utility._
import org.randi3.configuration.{ConfigurationService, ConfigurationServiceComponent}
import org.randi3.schema.{SupportedDatabases, DatabaseSchema}
import org.randi3.edc.service.OpenClinicaServiceComponent
import org.randi3.edc.dao.OpenClinicaDaoComponent
import org.randi3.edc.schema.OpenClinicaDatabaseSchema

  import scala.slick.session.Database
import scala.slick.driver.ExtendedProfile


object DependencyFactory {


  private var dependencyFactory: DependencyFactory = null

  def get: DependencyFactory = {
    if (dependencyFactory == null) dependencyFactory = new DependencyFactory
    dependencyFactory
  }

  def reInitializeDependencies={
    dependencyFactory = new DependencyFactory
  }
}


class DependencyFactory extends RandomizationPluginManagerComponent with DaoComponent with AuditDaoComponent with CriterionDaoComponent with TreatmentArmDaoComponent with TrialSubjectDaoComponent with TrialSiteDaoComponent with TrialRightDaoComponent with TrialDaoComponent with UserDaoComponent with SecurityComponent with I18NComponent with RandomizationMethodDaoComponent with TrialSiteServiceComponent with UtilityDBComponent with UtilityMailComponent with MailSenderComponent with TrialServiceComponent with UserServiceComponent with AuditServiceComponent with ConfigurationServiceComponent with OpenClinicaDaoComponent with OpenClinicaServiceComponent {

  import org.randi3.configuration.ConfigurationValues._

  val configurationService = new ConfigurationService


  lazy val dbType = configurationService.getConfigurationEntry(DB_TYPE.toString).toOption.getOrElse(SupportedDatabases.MySQL.toString)
  lazy val dbAddress = configurationService.getConfigurationEntry(DB_ADDRESS.toString).toOption.getOrElse("")
  lazy val dbUser = configurationService.getConfigurationEntry(DB_USER.toString).toOption.getOrElse("")
  lazy val dbPassword = configurationService.getConfigurationEntry(DB_PASSWORD.toString).toOption.getOrElse("")
  lazy val dbName = configurationService.getConfigurationEntry(DB_NAME.toString).toOption.getOrElse("")

  //val databaseTupel: (Database, ExtendedProfile) = DatabaseSchema.createDatabaseMySql //("Randi3TestDatabase")


  lazy val database = Database.forURL(ConfigurationService.generateJDBCURL(dbType, dbAddress, dbUser, dbPassword, dbName))

  lazy val driver: ExtendedProfile = if (dbType == SupportedDatabases.MySQL.toString){
    scala.slick.driver.MySQLDriver
  }
  else {
    scala.slick.driver.PostgresDriver
  }

  lazy val schema = new DatabaseSchema(driver)

  lazy val randomizationPluginManager = new RandomizationPluginManager

  lazy val auditDao = new AuditDao
  lazy val randomizationMethodDao = new RandomizationMethodDao
  lazy val trialDao = new TrialDao
  lazy val securityUtility = new SecurityUtility {
    def currentUser: Option[User] = CurrentLoggedInUser.get
  }
  lazy val treatmentArmDao = new TreatmentArmDao
  lazy val trialSubjectDao = new TrialSubjectDao
  lazy val trialSiteDao = new TrialSiteDao
  lazy val trialRightDao = new TrialRightDao
  lazy val userDao = new UserDao
  lazy val criterionDao = new CriterionDao
  lazy val i18n = I18N()


  lazy val utilityDB = new UtilityDB
  lazy val utilityMail = new UtilityMail


  lazy val mailServer = configurationService.getConfigurationEntry(MAIL_SERVER.toString).toOption.getOrElse("")
  lazy val mailPort = configurationService.getConfigurationEntry(MAIL_PORT.toString).toOption.getOrElse("25")
  lazy val mailSMPT_Auth = configurationService.getConfigurationEntry(MAIL_SMTP_AUTH.toString).toOption.getOrElse("true").toBoolean
  lazy val mailUsername = configurationService.getConfigurationEntry(MAIL_USERNAME.toString).toOption.getOrElse("")
  lazy val mailPassword = configurationService.getConfigurationEntry(MAIL_PASSWORD.toString).toOption.getOrElse("")
  lazy val mailSSL = configurationService.getConfigurationEntry(MAIL_SSL.toString).toOption.getOrElse("true").toBoolean
  lazy val mailFrom = configurationService.getConfigurationEntry(MAIL_FROM.toString).toOption.getOrElse("")


  lazy val mailSender = new MailSender(mailServer, mailPort, mailSMPT_Auth, mailUsername, mailPassword, mailSSL, mailFrom)

  lazy val trialSiteService = new TrialSiteService
  lazy val trialService = new TrialService
  lazy val userService = new UserService
  lazy val auditService = new AuditService


  lazy val openClinicaSchema = new OpenClinicaDatabaseSchema(driver)
  lazy val openClinicaDao = new OpenClinicaDao

  lazy val openClinicaService = new OpenClinicaService()

}

}

}
