package org.randi3.web.snippet


import net.liftweb.http._
import net.liftweb.wizard._
import org.randi3.configuration.ConfigurationValues
import org.randi3.configuration.ConfigurationService

import org.randi3.schema.{LiquibaseUtil, DatabaseSchema}
import org.randi3.model.{TrialSite, User}
import org.randi3.web.lib.DependencyFactory
import net.liftweb.util.FieldError
import org.randi3.utility.Logging

import org.scalaquery.session.Database
import org.scalaquery.meta.MTable

object InstallationWizard extends Wizard with Logging {


  import ConfigurationValues._

  val configurationService =  DependencyFactory.configurationService

  var skipUserTrialSiteInformation = false

  val serverUrlScreen = new Screen {
    override val screenName = "Server URL"
    val serverURL = field("Server url", configurationService.getConfigurationEntry(SERVER_URL.toString).toOption.getOrElse(""), valMinLen(1, "URL is neccesary"))

    override def nextScreen = {

      if (!serverURL.get.isEmpty) {
        configurationService.saveConfigurationEntry(SERVER_URL.toString, serverURL)
        logger.info("Installation: Server url saved!")
        super.nextScreen
      }  else {
        this
      }

      super.nextScreen
    }
  }

  // define the first screen
  val databaseScreen = new Screen {

    var firstView = true
    override val screenName = "Database"

    val dbType = select("Database type", configurationService.getConfigurationEntry(DB_TYPE.toString).toOption.getOrElse("MySQL"), List("MySQL"))
    val dbAddress = field("Database URL", configurationService.getConfigurationEntry(DB_ADDRESS.toString).toOption.getOrElse(""))
    val dbName = field("Database Name", configurationService.getConfigurationEntry(DB_NAME.toString).toOption.getOrElse(""))
    val dbUser = field("Database User", configurationService.getConfigurationEntry(DB_USER.toString).toOption.getOrElse(""))
    val dbPassword = password("Database Password", configurationService.getConfigurationEntry(DB_PASSWORD.toString).toOption.getOrElse(""))

    override def nextScreen = if (isDBSettingCorrect) mailScreen else this


    def isDBSettingCorrect: Boolean = {
      if (firstView) {
        firstView = false
        return false
      }
      val jdbcURL = ConfigurationService.generateJDBCURL(dbType, dbAddress, dbUser, dbPassword, dbName)
      val database = Database.forURL(jdbcURL)

      try {
        val tableList = MTable.getTables.list()(database.createSession())

        if (tableList.size != 0) {
          skipUserTrialSiteInformation  = true
        }

        LiquibaseUtil.updateDatabase(database)

        configurationService.saveConfigurationEntry(DB_TYPE.toString, dbType)
        configurationService.saveConfigurationEntry(DB_ADDRESS.toString, dbAddress)
        configurationService.saveConfigurationEntry(DB_NAME.toString, dbName)
        configurationService.saveConfigurationEntry(DB_USER.toString, dbUser)
        configurationService.saveConfigurationEntry(DB_PASSWORD.toString, dbPassword)

        logger.info("Installation: Database created!")
        return true

      } catch {
        case e: Exception => S.error("Error: " + e.getMessage)
      }
      false
    }
  }

  // define the second screen
  val mailScreen = new Screen {

    override val screenName = "Mail Server"
    //TODO checks
    val mailServer = field("Mail Server", configurationService.getConfigurationEntry(MAIL_SERVER.toString).toOption.getOrElse(""), valMinLen(1, "Entry neccesary"))
    val mailPort = field("Mail Port", configurationService.getConfigurationEntry(MAIL_PORT.toString).toOption.getOrElse("25"), valMinLen(1, "Entry neccesary"))

    val mailSMPT_Auth = select("SMTP AUTH?", configurationService.getConfigurationEntry(MAIL_SMTP_AUTH.toString).toOption.getOrElse("false"), Seq("true", "false"))

    val mailUsername = field("Mail server username", configurationService.getConfigurationEntry(MAIL_USERNAME.toString).toOption.getOrElse(""))
    val mailPassword = password("Mail server password", configurationService.getConfigurationEntry(MAIL_PASSWORD.toString).toOption.getOrElse(""))

    val mailSSL = select("SSL", configurationService.getConfigurationEntry(MAIL_SSL.toString).toOption.getOrElse("false"), Seq("true", "false"))

    val mailFrom = field("Mail Sender", configurationService.getConfigurationEntry(MAIL_FROM.toString).toOption.getOrElse(""), valMinLen(1, "Entry neccesary"))


    override def nextScreen = {
      if (!(mailServer.get.isEmpty || mailFrom.get.isEmpty)) {
        configurationService.saveConfigurationEntry(MAIL_SERVER.toString, mailServer)
        configurationService.saveConfigurationEntry(MAIL_PORT.toString, mailPort)
        configurationService.saveConfigurationEntry(MAIL_SMTP_AUTH.toString, mailSMPT_Auth)
        configurationService.saveConfigurationEntry(MAIL_USERNAME.toString, mailUsername)
        configurationService.saveConfigurationEntry(MAIL_PASSWORD.toString, mailPassword)
        configurationService.saveConfigurationEntry(MAIL_SSL.toString, mailSSL)
        configurationService.saveConfigurationEntry(MAIL_FROM.toString, mailFrom)
        logger.info("Installation: Mail-server configuration saved!")
        super.nextScreen
      }  else {
        this
      }

    }
  }

  val pluginPathScreen = new Screen {
    override val screenName = "Randomization Plugin Path"
    val pluginPath = field("Plugin Path", configurationService.getConfigurationEntry(PLUGIN_PATH.toString).toOption.getOrElse(""), valMinLen(1, "Path is neccesary"))

    override def nextScreen = {
      configurationService.saveConfigurationEntry(PLUGIN_PATH.toString, pluginPath)


      if (!pluginPath.isEmpty) {
        logger.info("Installation: Plugin-path (" + pluginPath + ") saved!")

        val pluginManager = DependencyFactory.randomizationPluginManager

        pluginManager.getPluginNames.foreach(pluginName => {
          val plugin = pluginManager.getPlugin(pluginName).get
          plugin.updateDatabase()

        })
      }

       super.nextScreen
    }
  }

  val trialSiteScreen = new Screen {


    var firstView = true

    val trialSiteDao = DependencyFactory.trialSiteDao

    var site: TrialSite = null


    override val screenName = "Trial Site"
    val name = field("Trial site name", "", valMinLen(1, "Field entry neccesary"))
    val street = field("Trial site street", "", valMinLen(1, "Field entry neccesary"))
    val postCode = field("Trial site post code", "", valMinLen(1, "Field entry neccesary"))
    val city = field("Trial site city", "", valMinLen(1, "Field entry neccesary"))
    val country = field("Trial site country", "", valMinLen(1, "Field entry neccesary"))
    val password1 = password("Trial site password", "", valMinLen(1, "Field entry neccesary"))
    val password2 = password("Retype password", "", mustMatch _)

    def mustMatch(s: String): List[FieldError] =
      if (s != password1.get) "Passwords do not match" else Nil

    override def nextScreen = {
       if (skipUserTrialSiteInformation){
        configurationService.saveConfigurationEntry(ConfigurationValues.INITIAL_OBJECTS_CREATED.toString, "true")
        logger.info("Installation completed, without user and trial site information!")
        S.notice("Installation completed!!")
        S.redirectTo("login")
      }

      if (firstView) {
        firstView = false
        this
      } else {
        TrialSite(name = name, country = country, street = street, postCode = postCode, city = city, password = password1).either match {
          case Left(failureFields) => S.error("Error: " + failureFields); this
          case Right(trialSite) => {
            trialSiteDao.create(trialSite).either match {
              case Left(failureCreate) => S.error("Error: " + failureCreate); this
              case Right(id) => {
                trialSiteDao.get(id).either match {
                  case Left(failure) => S.error("Error: " + failure); this
                  case Right(trialSiteDB) => site = trialSiteDB.get; super.nextScreen
                }
              }
            }
          }
        }
      }
    }
  }

  val adminScreen = new Screen {
    val userDao = DependencyFactory.userDao

    var firstView = true

    override val screenName = "Trial Site"
    val name = field("User name", "", valMinLen(1, "Field entry neccesary"))
    val firstName = field("User first name", "", valMinLen(1, "Field entry neccesary"))
    val lastName = field("User last name", "", valMinLen(1, "Field entry neccesary"))
    val eMail = field("E-Mail Address", "", valMinLen(1, "Field entry neccesary"))
    //val locale = field("Locale", "", valMinLen(1, "Field entry neccesary"))
    val phoneNumber = field("Phone Number", "", valMinLen(1, "Field entry neccesary"))
    val password1 = password("Password", "", valMinLen(1, "Field entry neccesary"))
    val password2 = password("Retype password", "", mustMatch _)

    def mustMatch(s: String): List[FieldError] =
      if (s != password1.get) "Passwords do not match" else Nil

    override def nextScreen = {
      if (firstView) {
        firstView = false
        this
      } else {
        User(username = name, password = password1, firstName = firstName, lastName = lastName, email = eMail, phoneNumber = phoneNumber, rights = Set(), administrator = true, canCreateTrial = false, site = trialSiteScreen.site).either match {
          case Left(failureCreate) => S.error("Error: " + failureCreate); this
          case Right(user) => {
            userDao.create(user).either match {
              case Left(failureCreate) => S.error("Error: " + failureCreate); this
              case Right(id) => super.nextScreen
            }
          }
        }
      }
    }
  }

  def finish() {
    configurationService.saveConfigurationEntry(ConfigurationValues.INITIAL_OBJECTS_CREATED.toString, "true")
    logger.info("Installation completed!")
    S.notice("Installation completed!!")
  }
}