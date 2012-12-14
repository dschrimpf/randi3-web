package org.randi3.web {

package lib {

import org.scalaquery.session._
import org.scalaquery.ql._
import org.randi3.dao._

import org.randi3.service._

import org.randi3.randomization._
import org.randi3.model.User
import org.randi3.web.util.CurrentLoggedInUser
import org.randi3.utility._
import org.randi3.configuration.{ConfigurationService, ConfigurationServiceComponent}
import org.randi3.schema.DatabaseSchema


/**
 *
 */
object DependencyFactory extends RandomizationPluginManagerComponent with DaoComponent with AuditDaoComponent with CriterionDaoComponent with TreatmentArmDaoComponent with TrialSubjectDaoComponent with TrialSiteDaoComponent with TrialRightDaoComponent with TrialDaoComponent with UserDaoComponent with SecurityComponent with I18NComponent with RandomizationMethodDaoComponent with TrialSiteServiceComponent with UtilityDBComponent with UtilityMailComponent with MailSenderComponent with TrialServiceComponent with UserServiceComponent with AuditServiceComponent with ConfigurationServiceComponent {

  import org.randi3.configuration.ConfigurationValues._

  val configurationService = new ConfigurationService


  lazy val dbType = configurationService.getConfigurationEntry(DB_TYPE.toString).toOption.getOrElse("")
  lazy val dbAddress = configurationService.getConfigurationEntry(DB_ADDRESS.toString).toOption.getOrElse("")
  lazy val dbUser = configurationService.getConfigurationEntry(DB_USER.toString).toOption.getOrElse("")
  lazy val dbPassword = configurationService.getConfigurationEntry(DB_PASSWORD.toString).toOption.getOrElse("")
  lazy val dbName = configurationService.getConfigurationEntry(DB_NAME.toString).toOption.getOrElse("")

  //val databaseTupel: (Database, ExtendedProfile) = DatabaseSchema.createDatabaseMySql //("Randi3TestDatabase")


  lazy val database = Database.forURL(ConfigurationService.generateJDBCURL(dbType, dbAddress, dbUser, dbPassword, dbName))
  lazy val driver = org.scalaquery.ql.extended.MySQLDriver

  lazy val schema = new DatabaseSchema(driver)

  lazy val randomizationPluginManager = new RandomizationPluginManager

//  lazy val ddl: DDL = {
//    var ddlTmp: DDL = null
//    randomizationPluginManager.getPluginNames.foreach(name => if (!randomizationPluginManager.getPlugin(name).get.databaseTables().isEmpty) ddlTmp =
//      if (ddlTmp != null) {
//        ddlTmp ++ randomizationPluginManager.getPlugin(name).get.databaseTables().get
//      } else {
//        randomizationPluginManager.getPlugin(name).get.databaseTables().get
//      })
//
//    ddlTmp
//  }

 // createTable(ddl, driver, database)


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


}

}

}
