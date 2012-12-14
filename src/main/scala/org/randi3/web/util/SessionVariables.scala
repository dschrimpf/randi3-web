package org.randi3.web.util

import net.liftweb.http.SessionVar
import org.randi3.model.{TrialSite, User, Trial}

object CurrentTrial extends SessionVar[Option[Trial]](None)

object CurrentLoggedInUser extends SessionVar[Option[User]](None)

object CurrentUser extends SessionVar[Option[User]](None)

object CurrentTrialSite extends SessionVar[Option[TrialSite]](None)