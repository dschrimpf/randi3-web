package org.randi3.web.snippet

import scala.xml.{Elem, NodeSeq}
import org.randi3.model.{User, TrialSite}
import net.liftweb.http.{ResponseShortcutException, StatefulSnippet, S}
import org.randi3.utility.{Logging, Utility}
import org.randi3.configuration.{ConfigurationService, ConfigurationValues, ConfigurationServiceComponent}
import org.randi3.web.lib.DependencyFactory
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import scala.Left
import net.liftweb.http.js.JsCmds.Replace
import scala.Right
import net.liftweb.common.Full
import java.util.Locale
import org.randi3.schema.{LiquibaseUtil, DatabaseSchema, SupportedDatabases}
import scalaz.NonEmptyList
import org.scalaquery.session.Database
import org.scalaquery.meta.MTable
import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.ql.Query
import org.scalaquery.session.Database.threadLocalSession
import net.liftweb.http.js.JsCommands


class InstallationSnippet extends StatefulSnippet with GeneralFormSnippet with Utility with Logging with ConfigurationServiceComponent {


  import ConfigurationValues._

  val configurationService = new ConfigurationService

  //server url
  private var serverURL = configurationService.getConfigurationEntry(SERVER_URL.toString).toOption.getOrElse("")

  //database config
  private var dbType = configurationService.getConfigurationEntry(DB_TYPE.toString).toOption.getOrElse(SupportedDatabases.MySQL.toString)
  private var dbAddress = configurationService.getConfigurationEntry(DB_ADDRESS.toString).toOption.getOrElse("")
  private var dbName = configurationService.getConfigurationEntry(DB_NAME.toString).toOption.getOrElse("")
  private var dbUser = configurationService.getConfigurationEntry(DB_USER.toString).toOption.getOrElse("")
  private var dbPassword = configurationService.getConfigurationEntry(DB_PASSWORD.toString).toOption.getOrElse("")

  //mail config
  private var mailServer = configurationService.getConfigurationEntry(MAIL_SERVER.toString).toOption.getOrElse("")
  private var mailPort = configurationService.getConfigurationEntry(MAIL_PORT.toString).toOption.getOrElse("25")
  private var mailSMPT_Auth = configurationService.getConfigurationEntry(MAIL_SMTP_AUTH.toString).toOption.getOrElse("false")
  private var mailUsername = configurationService.getConfigurationEntry(MAIL_USERNAME.toString).toOption.getOrElse("")
  private var mailPassword = configurationService.getConfigurationEntry(MAIL_PASSWORD.toString).toOption.getOrElse("")
  private var mailSSL = configurationService.getConfigurationEntry(MAIL_SSL.toString).toOption.getOrElse("false")
  private var mailFrom = configurationService.getConfigurationEntry(MAIL_FROM.toString).toOption.getOrElse("")

  //plugin path
  private var pluginPath = configurationService.getConfigurationEntry(PLUGIN_PATH.toString).toOption.getOrElse("")

  //TrialSite Fields

  private var trialSiteName = ""
  private var country = ""
  private var postCode = ""
  private var city = ""
  private var street = ""
  private var passwordTrialSite = ""
  private var passwordCheckTrialSite = ""


  private var trialSiteDatabase: TrialSite = null

  //User fields
  private var username = ""
  private var passwordUser = ""
  private var passwordCheckUser = ""
  private var email = ""
  private var firstName = ""
  private var lastName = ""
  private var phoneNumber = ""

  private val locales = Locale.getAvailableLocales.toList
    .sortBy(locale => if (!locale.getCountry.isEmpty) {
    locale.getDisplayLanguage + " (" + locale.getDisplayCountry + ")"
  } else {
    locale.getDisplayLanguage
  })
    .map(locale => (locale, if (!locale.getCountry.isEmpty) {
    locale.getDisplayLanguage + " (" + locale.getDisplayCountry + ")"
  } else {
    locale.getDisplayLanguage
  })).toSeq

  private var locale: Locale = Locale.ENGLISH


  def dispatch = {
    case "serverURL" => serverURLForm _
    case "databaseConfig" => databaseConfigForm _
    case "mailConfig" => mailConfigForm _
    case "pluginPath" => pluginPathForm _
    case "trialSite" => trialSiteForm _
    case "user" => userForm _
    case "finish" => finishForm _
  }


  private def serverURLForm(xhtml: NodeSeq): NodeSeq = {

    def next() {
      if (!serverURL.isEmpty) {
        configurationService.saveConfigurationEntry(SERVER_URL.toString, serverURL)
        logger.info("Installation: Server url (" + serverURL + ") saved!")
        S.redirectTo("/installation/database")
      } else {
        S.error("errMsg", "Server URL is empty")
      }
    }

    def serverURLField(failure: Boolean = false): Elem = {
      val id = "serverURL"
      generateEntry(id, failure, {
        ajaxText(serverURL, v => {
          serverURL = v
          if (serverURL.isEmpty) {
            showErrorMessage(id, NonEmptyList("Server URL is empty"))
            Replace(id + "Li", serverURLField(true))
          } else {
            clearErrorMessage(id)
            Replace(id + "Li", serverURLField(false))
          }
        }, "id" -> id)
      }
      )
    }

    bind("serverURL", xhtml,
      "serverURL" -> serverURLField(),
      "next" -> submit(S.?("next"), next _, "class" -> "btnSend")
    )
  }


  private def databaseConfigForm(xhtml: NodeSeq): NodeSeq = {

    def next() {
      var errorMessage = new StringBuilder

      if (dbAddress.isEmpty) {
        errorMessage = errorMessage.append("Database address is empty; ")
      }
      if (dbName.isEmpty) {
        errorMessage = errorMessage.append("Database name is empty; ")
      }
      if (dbUser.isEmpty) {
        errorMessage = errorMessage.append("Database user is empty; ")
      }

      if(errorMessage.isEmpty){

      val jdbcURL = ConfigurationService.generateJDBCURL(dbType, dbAddress, dbUser, dbPassword, dbName)
      val database = Database.forURL(jdbcURL)

      try {

        configurationService.saveConfigurationEntry(DB_TYPE.toString, dbType)
        configurationService.saveConfigurationEntry(DB_ADDRESS.toString, dbAddress)
        configurationService.saveConfigurationEntry(DB_NAME.toString, dbName)
        configurationService.saveConfigurationEntry(DB_USER.toString, dbUser)
        configurationService.saveConfigurationEntry(DB_PASSWORD.toString, dbPassword)

        LiquibaseUtil.updateDatabase(database)

        logger.info("Installation: Database created!")

        S.redirectTo("/installation/mail")

      } catch {
        case (rse: ResponseShortcutException) => throw rse
        case e: Exception => println(e); S.error("errMsg", "Error: " + e.getMessage)
      }
      }else {
        S.error("errMsg", errorMessage.toString)
      }
      }


      def dbTypeField(failure: Boolean = false): Elem = {
        val id = "dbType"
        generateEntry(id, failure, {
          select(SupportedDatabases.values.map(value => (value.toString, value.toString)).toList,
            Full(configurationService.getConfigurationEntry(DB_TYPE.toString).toOption.getOrElse(SupportedDatabases.MySQL.toString)),
            v => {
              dbType = v
              if (dbType.isEmpty) {
                showErrorMessage(id, NonEmptyList("DB type is empty"))
                Replace(id + "Li", dbTypeField(true))
              } else {
                clearErrorMessage(id)
                Replace(id + "Li", dbTypeField(false))
              }
            }, "id" -> id)
        }
        )
      }

      def dbAddressField(failure: Boolean = false): Elem = {
        val id = "dbAdress"
        generateEntry(id, failure, {
          ajaxText(dbAddress, v => {
            dbAddress = v
            if (dbAddress.isEmpty) {
              showErrorMessage(id, NonEmptyList("Database URL is empty"))
              Replace(id + "Li", dbAddressField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", dbAddressField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def dbNameField(failure: Boolean = false): Elem = {
        val id = "dbName"
        generateEntry(id, failure, {
          ajaxText(dbName, v => {
            dbName = v
            if (dbName.isEmpty) {
              showErrorMessage(id, NonEmptyList("Database name is empty"))
              Replace(id + "Li", dbNameField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", dbNameField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def dbUserField(failure: Boolean = false): Elem = {
        val id = "dbUser"
        generateEntry(id, failure, {
          ajaxText(dbUser, v => {
            dbUser = v
            if (dbUser.isEmpty) {
              showErrorMessage(id, NonEmptyList("Database user is empty"))
              Replace(id + "Li", dbUserField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", dbUserField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def dbPasswordField(failure: Boolean = false): Elem = {
        val id = "dbPassword"
        generateEntry(id, failure, {
          password(dbPassword, v => {
            dbPassword = v
            if (dbPassword.isEmpty) {
              showErrorMessage(id, NonEmptyList("Database password is empty"))
              Replace(id + "Li", dbPasswordField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", dbPasswordField(false))
            }
          }, "id" -> id)
        }
        )
      }


      bind("database", xhtml,
        "dbType" -> dbTypeField(),
        "dbAddress" -> dbAddressField(),
        "dbName" -> dbNameField(),
        "dbUser" -> dbUserField(),
        "dbPassword" -> dbPasswordField(),
        "back" ->   submit(S.?("back"), () => {S.redirectTo("/installation/serverURL") }, "class" -> "btnCancel"),
        "next" -> submit(S.?("next"), next _, "class" -> "btnSend")
      )
    }



    private def mailConfigForm(xhtml: NodeSeq): NodeSeq =
    {

      def next() {

        var errorMessage = new StringBuilder

        if (mailServer.isEmpty) {
          errorMessage = errorMessage.append("Mail server address is empty; ")
        }
        if (mailPort.isEmpty || !mailPort.forall(_.isDigit)) {
          errorMessage = errorMessage.append("Mail server port is empty or not a number; ")
        }
        if (mailFrom.isEmpty) {
          errorMessage = errorMessage.append("Mail sender is empty; ")
        }

        if(errorMessage.isEmpty){
          configurationService.saveConfigurationEntry(MAIL_SERVER.toString, mailServer)
          configurationService.saveConfigurationEntry(MAIL_PORT.toString, mailPort)
          configurationService.saveConfigurationEntry(MAIL_SMTP_AUTH.toString, mailSMPT_Auth)
          configurationService.saveConfigurationEntry(MAIL_USERNAME.toString, mailUsername)
          configurationService.saveConfigurationEntry(MAIL_PASSWORD.toString, mailPassword)
          configurationService.saveConfigurationEntry(MAIL_SSL.toString, mailSSL)
          configurationService.saveConfigurationEntry(MAIL_FROM.toString, mailFrom)

          S.redirectTo("/installation/pluginPath")
        }    else{
          S.error("errMsg", errorMessage.toString)
        }

      }


      def mailServerField(failure: Boolean = false): Elem = {
        val id = "mailServerURL"
        generateEntry(id, failure, {
          ajaxText(mailServer, v => {
            mailServer = v
            if (mailServer.isEmpty) {
              showErrorMessage(id, NonEmptyList("Mail server URL is empty"))
              Replace(id + "Li", mailServerField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", mailServerField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def mailServerPortField(failure: Boolean = false): Elem = {
        val id = "mailServerPort"
        generateEntry(id, failure, {
          ajaxText(mailPort, v => {
            mailPort = v
            if (mailPort.isEmpty || !mailPort.forall(_.isDigit)) {
              showErrorMessage(id, NonEmptyList("Mail server port is empty or not a number"))
              Replace(id + "Li", mailServerPortField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", mailServerPortField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def mailSMTPField(failure: Boolean = false): Elem = {
        val id = "mailSMTP_Auth"
        generateEntry(id, failure, {
          select(Seq(("true","true"), ("false","false")), Full("false"), v => {
            mailSMPT_Auth = v
           } , "id" -> id)
        }
        )
      }

      def mailServerUserField(failure: Boolean = false): Elem = {
        val id = "mailServerUser"
        generateEntry(id, failure, {
          ajaxText(mailUsername, v => {
            mailUsername = v
          }, "id" -> id)
        }
        )
      }

      def mailServerPasswordField(failure: Boolean = false): Elem = {
        val id = "mailServerPassword"
        generateEntry(id, failure, {
          password(mailPassword, v => {
            mailPassword = v
          }, "id" -> id)
        }
        )
      }

      def mailSSLField(failure: Boolean = false): Elem = {
        val id = "mailSSL"
        generateEntry(id, failure, {
          select(Seq(("true","true"), ("false","false")), Full("false"), v => {
            mailSSL = v
          } , "id" -> id)
        }
        )
      }

      def mailServerFrom(failure: Boolean = false): Elem = {
        val id = "mailServerFrom"
        generateEntry(id, failure, {
          ajaxText(mailFrom, v => {
            mailFrom = v
            if (mailFrom.isEmpty) {
              showErrorMessage(id, NonEmptyList("Sender mail address is empty"))
              Replace(id + "Li", mailServerFrom(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", mailServerFrom(false))
            }
          }, "id" -> id)
        }
        )
      }


      bind("mail", xhtml,
      "serverURL" -> mailServerField(),
        "serverPort" -> mailServerPortField(),
        "smtp_auth" -> mailSMTPField(),
        "username" -> mailServerUserField(),
        "password" -> mailServerPasswordField(),
        "ssl" -> mailSSLField(),
        "from" -> mailServerFrom(),
        "back" ->   submit(S.?("back"), () => {S.redirectTo("/installation/database") }, "class" -> "btnCancel"),
        "next" -> submit(S.?("next"), next _, "class" -> "btnSend")
      )
    }

    private def pluginPathForm(xhtml: NodeSeq): NodeSeq =
    {

      def next() {

        if (!pluginPath.isEmpty) {

          configurationService.saveConfigurationEntry(PLUGIN_PATH.toString, pluginPath)

          logger.info("Installation: Plugin-path (" + pluginPath + ") saved!")

          DependencyFactory.reInitializeDependencies
          val pluginManager = DependencyFactory.get.randomizationPluginManager
          pluginManager.init()

          if (pluginManager.getPluginNames.isEmpty){
            S.error("errMsg", "Error: No plugins available in path (" + pluginPath +")")
          } else {

            pluginManager.getPluginNames.foreach(pluginName => {
              val plugin = pluginManager.getPlugin(pluginName).get
              plugin.updateDatabase()

            })

              S.redirectTo("/installation/trialSite")

          }
        } else {
         S.error("errMsg", "Plugin path is empty")
        }
      }

      def pluginPathField(failure: Boolean = false): Elem = {
        val id = "pluginPath"
        generateEntry(id, failure, {
          ajaxText(pluginPath, v => {
            pluginPath = v
            if (pluginPath.isEmpty) {
              showErrorMessage(id, NonEmptyList("Plugin path is empty"))
              Replace(id + "Li", pluginPathField(true))
            } else {
              clearErrorMessage(id)
              Replace(id + "Li", pluginPathField(false))
            }
          }, "id" -> id)
        }
        )
      }

      bind("pluginPath", xhtml,
        "path" -> pluginPathField(),
        "back" ->   submit(S.?("back"), () => {S.redirectTo("/installation/mail") }, "class" -> "btnCancel"),
        "next" -> submit(S.?("next"), next _, "class" -> "btnSend")
      )
    }


    private def trialSiteForm(xhtml: NodeSeq): NodeSeq =
    {

      def next() {

        def trialSiteDao = {
          DependencyFactory.reInitializeDependencies
          DependencyFactory.get.trialSiteDao
        }

        TrialSite(name = trialSiteName, street = street, postCode = postCode, city = city, country = country, password = passwordTrialSite, isActive = true).either match {
          case Left(x) => S.error("errMsg", x.toString)
          case Right(site) => trialSiteDao.create(site).either match {
            case Left(failureCreate) => S.error("errMsg", "Error: " + failureCreate)
            case Right(id) => {
              trialSiteDao.get(id).either match {
                case Left(failure) => S.error("errMsg", "Error: " + failure)
                case Right(trialSiteDB) => {
                  trialSiteDatabase = trialSiteDB.get
                  S.redirectTo("/installation/user")
                }
              }
            }
          }
        }
      }


      def nameField(failure: Boolean = false): Elem = {
        val id = "name"
        generateEntry(id, failure, {
          ajaxText(trialSiteName, v => {
            trialSiteName = v
            TrialSite.check(name = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", nameField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", nameField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def streetField(failure: Boolean = false): Elem = {
        val id = "street"
        generateEntry(id, failure, {
          ajaxText(street, v => {
            street = v
            TrialSite.check(street = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", streetField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", streetField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def postCodeField(failure: Boolean = false): Elem = {
        val id = "postCode"
        generateEntry(id, failure, {
          ajaxText(postCode, v => {
            postCode = v
            TrialSite.check(postCode = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", postCodeField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", postCodeField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def cityField(failure: Boolean = false): Elem = {
        val id = "city"
        generateEntry(id, failure, {
          ajaxText(city, v => {
            city = v
            TrialSite.check(city = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", cityField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", cityField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def countryField(failure: Boolean = false): Elem = {
        val id = "country"
        generateEntry(id, failure, {
          ajaxText(country, v => {
            country = v
            TrialSite.check(country = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", countryField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", countryField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def passwordField(failure: Boolean = false): Elem = {
        val id = "password"
        generateEntry(id, failure, {
          ajaxText(passwordTrialSite, v => {
            passwordTrialSite = v
            TrialSite.check(password = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", passwordField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", passwordField(false))
            }
          }, "id" -> id, "type" -> "password")
        }
        )
      }

      def passwordCheckField(failure: Boolean = false): Elem = {
        val id = "passwordCheck"
        generateEntry(id, failure, {
          ajaxText(passwordCheckTrialSite, v => {
            passwordCheckTrialSite = v
            if (passwordTrialSite == passwordCheckTrialSite) {
              clearErrorMessage(id)
              Replace(id + "Li", passwordCheckField(false))
            } else {
              S.error(id + "Msg", "<- passwords does not match")
              Replace(id + "Li", passwordCheckField(true))
            }
          }, "id" -> id, "type" -> "password")
        }
        )
      }

      bind("trialSite", xhtml,
        "name" -> nameField(),
        "street" -> streetField(),
        "postCode" -> postCodeField(),
        "city" -> cityField(),
        "country" -> countryField(),
        "password" -> passwordField(),
        "passwordCheck" -> passwordCheckField(),
        "back" ->   submit(S.?("back"), () => {S.redirectTo("/installation/pluginPath") }, "class" -> "btnCancel"),
        "skip" ->   submit(S.?("skip"), () => {S.redirectTo("/installation/finish") }, "class" -> "btnNormal", "style" -> "margin-left:20em;"),
        "next" -> submit(S.?("next"), next _, "class" -> "btnSend")
      )
    }


    private def userForm(xhtml: NodeSeq): NodeSeq =
    {
      def trialSiteDao = {
        DependencyFactory.reInitializeDependencies
        DependencyFactory.get.trialSiteDao
      }
      val trialSites: List[(TrialSite, String)] = trialSiteDao.getAll.toOption.get.map(trialSite => (trialSite, trialSite.name))
      var actualTrialSite: TrialSite = trialSites.head._1

      def next() {
        def userDao = {
          DependencyFactory.reInitializeDependencies
          DependencyFactory.get.userDao
        }
         User(username = username, password = passwordUser, email = email, firstName = firstName, lastName = lastName, phoneNumber = phoneNumber, site = actualTrialSite, rights = Set(), administrator = true, canCreateTrial = false, locale = locale).either match {
          case Left(x) => S.error("errMsg", x.toString())
          case Right(user) =>
            userDao.create(user).either match {
              case Left(failureCreate) => S.error("errMsg", "Error: " + failureCreate)
              case Right(id) => S.redirectTo("/installation/finish")
            }
        }

      }


      def usernameField(failure: Boolean = false): Elem = {
        val id = "username"
        generateEntry(id, failure, {
          ajaxText(username, v => {
            username = v
            User.check(username = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", usernameField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", usernameField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def passwordField(failure: Boolean = false): Elem = {
        val id = "password"
        generateEntry(id, failure, {
          ajaxText(passwordUser, v => {
            passwordUser = v
            User.check(password = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", passwordField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", passwordField(false))
            }
          }, "id" -> id, "type" -> "password")
        }
        )
      }

      def passwordCheckField(failure: Boolean = false): Elem = {
        val id = "passwordCheck"
        generateEntry(id, failure, {
          ajaxText(passwordCheckUser, v => {
            passwordCheckUser = v
            if (passwordUser == passwordCheckUser) {
              clearErrorMessage(id)
              Replace(id + "Li", passwordCheckField(false))
            } else {
              S.error(id + "Msg", "<- passwords does not match")
              Replace(id + "Li", passwordCheckField(true))
            }
          }, "id" -> id, "type" -> "password")
        }
        )
      }

      def firstNameField(failure: Boolean = false): Elem = {
        val id = "firstName"
        generateEntry(id, failure, {
          ajaxText(firstName, v => {
            firstName = v
            User.check(firstName = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", firstNameField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", firstNameField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def lastNameField(failure: Boolean = false): Elem = {
        val id = "lastName"
        generateEntry(id, failure, {
          ajaxText(lastName, v => {
            lastName = v
            User.check(firstName = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", lastNameField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", lastNameField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def emailField(failure: Boolean = false): Elem = {
        val id = "email"
        generateEntry(id, failure, {
          ajaxText(email, v => {
            email = v
            User.check(firstName = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", emailField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", emailField(false))
            }
          }, "id" -> id)
        }
        )
      }

      def phoneNumberField(failure: Boolean = false): Elem = {
        val id = "phoneNumber"
        generateEntry(id, failure, {
          ajaxText(phoneNumber, v => {
            phoneNumber = v
            User.check(firstName = v).either match {
              case Left(x) => showErrorMessage(id, x); Replace(id + "Li", phoneNumberField(true))
              case Right(_) => clearErrorMessage(id); Replace(id + "Li", phoneNumberField(false))
            }
          }, "id" -> id)
        }
        )
      }


      def trialSiteField: Elem = {
        val id = "trialSite"
        generateEntry(id, false, {
          ajaxSelectObj(trialSites, Full(actualTrialSite), (trialSite: TrialSite) => {
            actualTrialSite = trialSite
          }, "id" -> id)
        })
      }


      bind("user", xhtml,
        "info" -> <span>
          {username}
        </span>,
        "username" -> usernameField(),
        "password" -> passwordField(),
        "passwordCheck" -> passwordCheckField(),
        "firstName" -> firstNameField(),
        "lastName" -> lastNameField(),
        "email" -> emailField(),
        "phoneNumber" -> phoneNumberField(),
        "locale" -> selectObj(locales, Full(locale), (loc: Locale) => locale = loc),
         "trialSite" -> trialSiteField,
        "back" ->   submit(S.?("back"), () => {S.redirectTo("/installation/trialSite") }, "class" -> "btnCancel"),
        "next" -> submit(S.?("next"), next _, "class" -> "btnSend")
      )
    }


    private def finishForm(xhtml: NodeSeq): NodeSeq =
    {

      def next() {
        configurationService.saveConfigurationEntry(ConfigurationValues.INITIAL_OBJECTS_CREATED.toString, "true")
        logger.info("Installation completed!")
        S.notice("Installation completed!!")
        S.redirectTo("/login")
      }

      bind("finish", xhtml,
        "back" ->   submit(S.?("back"), () => {S.redirectTo("/installation/user")}, "class" -> "btnCancel"),
        "finish" -> submit(S.?("finish"), next _, "class" -> "btnSend")
      )
    }

  }
