package org.randi3.web.snippet

import scala.xml._
import scala.xml.NodeSeq

import org.randi3.web.lib.DependencyFactory

import net.liftweb.http.SHtml._
import net.liftweb.http._

import js.JsCmds.Replace
import net.liftweb.util.Helpers._
import org.randi3.model.criterion._
import constraint._
import org.randi3.web.util.{CurrentLocalEDCTrial, Utility, CurrentLoggedInUser, CurrentEDCTrial}
import collection.mutable.ListBuffer
import collection.mutable
import net.liftweb.common.Empty
import org.randi3.randomization.RandomizationMethod
import org.randi3.model._
import org.apache.commons.math3.random.MersenneTwister
import org.joda.time.LocalDate
import scala.Left
import org.randi3.randomization.configuration.OrdinalConfigurationType
import org.randi3.randomization.configuration.IntegerConfigurationType
import org.randi3.randomization.configuration.BooleanConfigurationType
import scala.Some
import xml.Node

import scalaz._
import scala.Right
import org.randi3.randomization.configuration.DoubleConfigurationType

import org.randi3.web.model._


class EdcRandomizationSnippet extends StatefulSnippet {


  def dispatch: EdcRandomizationSnippet#DispatchIt = {
    case "randomize" => randomize _
  }

val openClinicaService =   DependencyFactory.get.openClinicaService

  private def randomize(nodeSeq: NodeSeq): NodeSeq = {
    if (CurrentLocalEDCTrial.isEmpty) S.redirectTo("/edcTrial/list")

    val trialOC = CurrentLocalEDCTrial.get.get

    def randomize() {
         openClinicaService.randomizeNewTrialSubjects(trialOC).toEither match {
           case Left(error) => S.error("randomizeMsg", error)
           case Right(x) => {
             CurrentLocalEDCTrial.set(Some(x))
             S.notice("Patients randomized")
             redirectTo("/edcTrial/viewLocalDetails")
           }
         }         
    }

    bind("edcTrial", nodeSeq,
      "cancel" ->       submit(S.?("cancel"), () => {
                redirectTo("/edcTrial/list")
      }, "class" -> "btnCancel"),
      "randomize" -> submit(S.?("randomize"), randomize _, "class" -> "btnSend")
    )
  }



}
