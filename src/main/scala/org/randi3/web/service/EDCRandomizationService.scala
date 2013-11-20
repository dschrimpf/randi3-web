package org.randi3.web.service

import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.common.Box
import scala.xml.Elem
import org.randi3.web.lib.DependencyFactory

object EDCRandomizationService extends RestHelper {
   
  def randomizeTrial(): Box[Elem] = {
       for {
        trialID <- S.param("trialID") ?~ "Trial identifier (trialOID) is missing" ~> 400
        //<- S.param("firstname") ?~ "firstname parameter missing" ~> 400
      } yield {
         DependencyFactory.get.openClinicaService.randomizeNewTrialSubjects(trialID).toEither match {
           case Left(failure) =>  <error>{failure}</error>
           case Right(_) => <success>Patients randomized</success>
         }
      }
  }

  serve {
    case Get("api" :: "randomize" :: _, _) => randomizeTrial()
  }

}