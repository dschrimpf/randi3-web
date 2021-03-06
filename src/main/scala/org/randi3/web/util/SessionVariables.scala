package org.randi3.web.util

import net.liftweb.http.SessionVar
import org.randi3.model._
import org.randi3.edc.model.openClinica.TrialOC
import org.randi3.edc.model.openClinica.TrialOC
import org.randi3.simulation.model.SimulationResult

object CurrentTrial extends SessionVar[Option[Trial]](None)

object CurrentLoggedInUser extends SessionVar[Option[User]](None)

object CurrentUser extends SessionVar[Option[User]](None)

object CurrentTrialSite extends SessionVar[Option[TrialSite]](None)

object CurrentEDCTrial extends SessionVar[Option[TrialOC]](None)

object CurrentLocalEDCTrial extends SessionVar[Option[TrialOC]](None)

object RandomizationResult extends SessionVar[Option[(TreatmentArm, String, TrialSubject)]](None)

object CurrentSubjectToRandomizeAndSuspicionOfDuplicatedProperties extends SessionVar[Option[(TrialSubject, Boolean)]](None)

object CurrentSimulationResult extends SessionVar[Option[SimulationResult]](None)