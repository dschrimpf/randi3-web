package org.randi3.web.snippet

import net.liftweb.util.Helpers._

import xml.{NodeSeq, Group}
import org.randi3.web.util.CurrentTrial


class TrialSelectForm {

  def render(xhtml: Group): NodeSeq = {
    if (CurrentTrial.get.isDefined) {
      bind("trial", xhtml,
        "name" ->  <div>Selected Trial:<br /><b>{CurrentTrial.get.get.name}</b></div>
      )
    } else {
      bind("trial", xhtml,
        "name" -> ""

      )
    }

  }
}
